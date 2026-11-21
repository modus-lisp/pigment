;;;; src/render/jpeg.lisp — a pure-CL JPEG/JFIF decoder to RGBA (baseline + progressive).
;;;;
;;;; Decodes real <img src="…jpg">: marker parse (DQT/SOF0/SOF2/DHT/DRI/SOS),
;;;; canonical-Huffman entropy decode with byte-stuffing and restart intervals,
;;;; and — for both sequential (baseline) and PROGRESSIVE images — accumulates the
;;;; DCT coefficients across every scan (spectral selection + successive
;;;; approximation) before a single dequantise / 8x8 inverse DCT / level-shift /
;;;; chroma-upsample / YCbCr->RGB pass.  Because the frame's SOF dimensions are
;;;; fixed up front and every scan only refines coefficients, the result is always
;;;; the full intrinsic size — a partial/truncated progressive stream yields a
;;;; lower-quality image at the SAME size, so the layout box never resizes.
;;;; Arithmetic coding is not handled.
(in-package #:pigment)

(defun j16 (v i) (logior (ash (aref v i) 8) (aref v (+ i 1))))

;; The k-th zig-zag coefficient maps to this index in the natural 8x8 block.
(defparameter +zigzag+
  #(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
    35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63))

;; idct-cos[x][u] = C(u) * cos((2x+1) u pi / 16), C(0)=1/sqrt2 else 1.
(defparameter +idct-cos+
  (let ((tbl (make-array '(8 8) :element-type 'double-float))
        (pd (coerce pi 'double-float)))
    (dotimes (x 8)
      (dotimes (u 8)
        (setf (aref tbl x u)
              (* (if (zerop u) (/ 1d0 (sqrt 2d0)) 1d0)
                 (cos (/ (* (+ (* 2 x) 1) u pd) 16d0))))))
    tbl))

;;; ---- canonical Huffman ---------------------------------------------------
(defstruct jhuff mincode maxcode valptr symbols)

(defun build-huff (counts symbols)
  "Build a decode table from the 16 per-length COUNTS and the flat SYMBOLS."
  (let ((mincode (make-array 17 :initial-element 0))
        (maxcode (make-array 17 :initial-element -1))
        (valptr (make-array 17 :initial-element 0))
        (code 0) (k 0))
    (loop for l from 1 to 16 for c = (aref counts (1- l)) do
      (when (plusp c)
        (setf (aref valptr l) k (aref mincode l) code)
        (incf code c) (incf k c)
        (setf (aref maxcode l) (1- code)))
      (setf code (ash code 1)))
    (make-jhuff :mincode mincode :maxcode maxcode :valptr valptr :symbols symbols)))

;;; ---- entropy bit reader (byte-stuffing + restart markers) ----------------
(defstruct jbr data (pos 0) (cnt 0) (buf 0) (eof nil))

(defun jbr-next-bit (br)
  "Next entropy bit; 0 once a marker is reached (marker left at POS)."
  (when (zerop (jbr-cnt br))
    (let ((data (jbr-data br)) (n (length (jbr-data br))))
      (when (>= (jbr-pos br) n) (setf (jbr-eof br) t) (return-from jbr-next-bit 0))
      (let ((b (aref data (jbr-pos br))))
        (incf (jbr-pos br))
        (when (= b #xff)
          (let ((b2 (if (< (jbr-pos br) n) (aref data (jbr-pos br)) #xd9)))
            (cond ((= b2 0) (incf (jbr-pos br)))        ; stuffed FF00 -> data byte FF
                  (t (decf (jbr-pos br))                ; a marker: leave FF.. for the caller
                     (setf (jbr-eof br) t)
                     (return-from jbr-next-bit 0)))))
        (setf (jbr-buf br) b (jbr-cnt br) 8))))
  (decf (jbr-cnt br))
  (logand (ash (jbr-buf br) (- (jbr-cnt br))) 1))

(defun jbr-restart (br)
  "Byte-align and consume the next RSTn (FF D0-D7) marker at a restart boundary."
  (setf (jbr-cnt br) 0 (jbr-eof br) nil)
  (let ((data (jbr-data br)) (n (length (jbr-data br))))
    (loop while (< (jbr-pos br) n) do
      (if (and (= (aref data (jbr-pos br)) #xff)
               (< (1+ (jbr-pos br)) n)
               (<= #xd0 (aref data (1+ (jbr-pos br))) #xd7))
          (progn (incf (jbr-pos br) 2) (return))
          (incf (jbr-pos br))))))

(defun jbr-receive (br n)
  (let ((v 0)) (dotimes (i n v) (setf v (logior (ash v 1) (jbr-next-bit br))))))

(defun jhuff-extend (v n)
  "Sign-extend an N-bit magnitude V (JPEG EXTEND)."
  (if (and (plusp n) (< v (ash 1 (1- n)))) (+ v 1 (- (ash 1 n))) v))

(defun jhuff-decode (br huff)
  (let ((code 0))
    (loop for l from 1 to 16 do
      (setf code (logior (ash code 1) (jbr-next-bit br)))
      (when (and (>= (aref (jhuff-maxcode huff) l) 0) (<= code (aref (jhuff-maxcode huff) l)))
        (return-from jhuff-decode
          (aref (jhuff-symbols huff)
                (+ (aref (jhuff-valptr huff) l) (- code (aref (jhuff-mincode huff) l)))))))
    0))

;;; ---- per-block coefficient decoders --------------------------------------
;;; COEF is a component's flat (signed-byte 16) coefficient buffer (natural order,
;;; one 64-slot block per grid cell); OFF is this block's base index.  EOB is a
;;; one-element list holding the shared end-of-band run counter for AC scans.

(defun decode-baseline-block (br dc-huff ac-huff comp coef off)
  "Sequential (baseline) block: DC diff + the full AC run, no bit shifting."
  (let* ((s (jhuff-decode br dc-huff))
         (diff (if (plusp s) (jhuff-extend (jbr-receive br s) s) 0)))
    (incf (jcomp-pred comp) diff)
    (setf (aref coef off) (jcomp-pred comp))
    (let ((k 1))
      (loop while (< k 64) do
        (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)))
          (cond ((zerop sz) (if (= r 15) (incf k 16) (return)))     ; ZRL / EOB
                (t (incf k r)
                   (when (< k 64)
                     (setf (aref coef (+ off (aref +zigzag+ k))) (jhuff-extend (jbr-receive br sz) sz))
                     (incf k)))))))))

(defun decode-dc-first (br dc-huff comp coef off al)
  (let* ((s (jhuff-decode br dc-huff))
         (diff (if (plusp s) (jhuff-extend (jbr-receive br s) s) 0)))
    (incf (jcomp-pred comp) diff)
    (setf (aref coef off) (ash (jcomp-pred comp) al))))

(defun decode-dc-refine (br coef off al)
  (when (= (jbr-next-bit br) 1)
    (setf (aref coef off) (logior (aref coef off) (ash 1 al)))))

(defun decode-ac-first (br ac-huff coef off ss se al eob)
  "Progressive first AC scan for one band [Ss,Se], approximation bit Al."
  (if (plusp (car eob))
      (decf (car eob))
      (let ((k ss))
        (loop while (<= k se) do
          (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)))
            (cond ((zerop sz)
                   (if (< r 15)
                       (progn (setf (car eob) (1- (ash 1 r)))
                              (when (plusp r) (incf (car eob) (jbr-receive br r)))
                              (return))
                       (incf k 16)))                       ; ZRL: 16 zeros
                  (t (incf k r)
                     (when (<= k se)
                       (setf (aref coef (+ off (aref +zigzag+ k)))
                             (ash (jhuff-extend (jbr-receive br sz) sz) al))
                       (incf k)))))))))

(defun decode-ac-refine (br ac-huff coef off ss se al eob)
  "Progressive AC refinement scan: correction bits for already-nonzero
   coefficients plus newly-significant ones (JPEG Annex G.1.2.3)."
  (let ((p1 (ash 1 al)) (m1 (- (ash 1 al))) (k ss))
    (when (zerop (car eob))
      (loop while (<= k se) do
        (let* ((rs (jhuff-decode br ac-huff)) (r (ash rs -4)) (sz (logand rs 15)) (val 0))
          (cond ((zerop sz)
                 (when (< r 15)
                   (setf (car eob) (ash 1 r))
                   (when (plusp r) (incf (car eob) (jbr-receive br r)))
                   (return)))
                (t (setf val (if (= (jbr-next-bit br) 1) p1 m1))))   ; sz is 1: a new coefficient
          ;; advance over R zero-history coefficients, refining nonzero ones met
          (loop while (<= k se) do
            (let ((idx (+ off (aref +zigzag+ k))))
              (if (/= (aref coef idx) 0)
                  (when (and (= (jbr-next-bit br) 1) (zerop (logand (aref coef idx) p1)))
                    (incf (aref coef idx) (if (plusp (aref coef idx)) p1 m1)))
                  (if (zerop r) (return) (decf r))))
            (incf k))
          (when (and (/= sz 0) (<= k se))
            (setf (aref coef (+ off (aref +zigzag+ k))) val))
          (incf k))))
    ;; within an EOB run: refine every remaining nonzero coefficient
    (when (plusp (car eob))
      (loop while (<= k se) do
        (let ((idx (+ off (aref +zigzag+ k))))
          (when (and (/= (aref coef idx) 0)
                     (= (jbr-next-bit br) 1) (zerop (logand (aref coef idx) p1)))
            (incf (aref coef idx) (if (plusp (aref coef idx)) p1 m1))))
        (incf k))
      (decf (car eob)))))

;;; ---- inverse DCT ---------------------------------------------------------
(defun idct-8x8 (block out)
  "Separable inverse DCT of the 64-double coefficient BLOCK into OUT (spatial)."
  (declare (type (simple-array double-float (64)) block out) (optimize speed (safety 0)))
  (let ((tmp (make-array 64 :element-type 'double-float)) (cs +idct-cos+))
    (declare (type (simple-array double-float (8 8)) cs) (dynamic-extent tmp))
    (dotimes (r 8)                                   ; rows
      (dotimes (x 8)
        (let ((sum 0d0))
          (declare (type double-float sum))
          (dotimes (u 8) (incf sum (* (aref block (+ (* r 8) u)) (aref cs x u))))
          (setf (aref tmp (+ (* r 8) x)) (* 0.5d0 sum)))))
    (dotimes (c 8)                                   ; columns
      (dotimes (y 8)
        (let ((sum 0d0))
          (declare (type double-float sum))
          (dotimes (v 8) (incf sum (* (aref tmp (+ (* v 8) c)) (aref cs y v))))
          (setf (aref out (+ (* y 8) c)) (* 0.5d0 sum)))))))

(declaim (inline clamp8))
(defun clamp8 (x)
  (declare (type double-float x))
  (cond ((< x 0d0) 0) ((> x 255d0) 255) (t (values (round x)))))

;;; ---- top level -----------------------------------------------------------
(defstruct jcomp id h v qsel coef bpl bh (pred 0))   ; coef: natural-order (s16) blocks

(defun jpeg-size (bytes &key (orient t))
  "Read a JPEG's intrinsic (values WIDTH HEIGHT) from its SOF marker without
   decoding the pixels — so a box can be reserved before/without a full decode.
   ORIENT (the default): as JPEG-DECODE shows it, turned by its EXIF orientation."
  (when (and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
    (let ((n (length bytes)) (i 2))
      (loop
        (loop while (and (< i n) (/= (aref bytes i) #xff)) do (incf i))
        (loop while (and (< i n) (= (aref bytes i) #xff)) do (incf i))
        (when (>= i (- n 4)) (return))
        (let ((m (aref bytes i))) (incf i)
          (cond ((or (<= #xc0 m #xc3) (<= #xc5 m #xc7) (<= #xc9 m #xcb) (<= #xcd m #xcf))  ; any SOF
                 (let ((w (j16 bytes (+ i 5))) (h (j16 bytes (+ i 3))))
                   (return-from jpeg-size
                     (if (and orient (>= (jpeg-orientation bytes) 5)) (values h w) (values w h)))))
                ((or (= m #xd8) (= m #xd9) (<= #xd0 m #xd7)))       ; standalone markers
                (t (incf i (j16 bytes i)))))))
    nil))

(declaim (inline clamp8i))
(defun clamp8i (x) (cond ((< x 0) 0) ((> x 255) 255) (t x)))

(defun %jpeg-int-idct-table (n)
  "The N-point inverse DCT's basis, fixed point (x 2^13), N x N in a vector, X*8+U:
   C(U)/2 cos((2X+1)U pi / 2N), C(0) = 1/sqrt 2 -- the 8-point IDCT's own scaling at N 8, and at
   N 4 and 2 the reduced IDCT (libjpeg's jidctred): an N x N block from the top-left N x N
   coefficients, which is the block at 1/(8/N) of its size."
  (let ((tbl (make-array 64 :initial-element 0)) (pd (coerce pi 'double-float)))
    (dotimes (x n tbl)
      (dotimes (u n)
        (setf (svref tbl (+ (* x 8) u))
              (round (* 8192d0 0.5d0 (if (zerop u) (/ 1d0 (sqrt 2d0)) 1d0)
                        (cos (/ (* (+ (* 2 x) 1) u pd) (* 2d0 n))))))))))

(defparameter +jpeg-idct-8+ (%jpeg-int-idct-table 8))
(defparameter +jpeg-idct-4+ (%jpeg-int-idct-table 4))
(defparameter +jpeg-idct-2+ (%jpeg-int-idct-table 2))

(defun %jpeg-block-to-plane (coef off qtab plane pw col row sc fblock spatial)
  "One 8x8 block -- natural-order coefficients COEF from OFF, dequantised by QTAB -- as pixels at
   1/SC into PLANE (row length PW) at (COL, ROW): an 8x8, 4x4 or 2x2 block, or (SC 8) the DC term
   alone.  INTEGER ARITHMETIC ONLY (fixed point, the basis x 2^13): on modus a double is boxed at
   every operation, and the double-float IDCT made a 4K photo at 1/4 a 6.6 s decode.  FBLOCK and
   SPATIAL are 64-element scratch vectors (any element type that holds a fixnum)."
  (declare (type (simple-array (signed-byte 16) (*)) coef) (type simple-vector qtab fblock)
           (type (simple-array (unsigned-byte 8) (*)) plane) (fixnum off pw col row sc)
           (ignore spatial))
  (let ((n (floor 8 sc)))
    (declare (fixnum n))
    (if (= n 1)
        (setf (aref plane (+ (* row pw) col))
              (clamp8i (+ (round (* (aref coef off) (aref qtab 0)) 8) 128)))
        (let ((tbl (case n (8 +jpeg-idct-8+) (4 +jpeg-idct-4+) (t +jpeg-idct-2+))))
          (declare (type simple-vector tbl))
          ;; rows: SPATIAL[v*8+x] = sum_u F(v,u) T(x,u), over the top-left N x N coefficients.
          ;; Every product fits a fixnum: |F| < 2^16, |Q| < 2^16, |T| < 2^13.
          (dotimes (v n)
            (dotimes (x n)
              (let ((sum 0))
                (declare (fixnum sum))
                (dotimes (u n)
                  (let ((k (+ (* v 8) u)))
                    (declare (fixnum k))
                    (setq sum (+ sum (the fixnum (* (the fixnum (* (the fixnum (aref coef (+ off k)))
                                                                    (the fixnum (aref qtab k))))
                                                     (the fixnum (svref tbl (+ (* x 8) u)))))))))
                (setf (svref fblock (+ (* v 8) x)) sum))))
          ;; columns: pixel(y,x) = sum_v ROWS[v*8+x] T(y,v), then 2^26 off, centred on 128
          (dotimes (x n)
            (dotimes (y n)
              (let ((sum 0))
                (declare (fixnum sum))
                (dotimes (v n)
                  (setq sum (+ sum (the fixnum (* (the fixnum (svref fblock (+ (* v 8) x)))
                                                   (the fixnum (svref tbl (+ (* y 8) v))))))))
                (setf (aref plane (+ (* (+ row y) pw) col x))
                      (clamp8i (+ (ash (+ sum 33554432) -26) 128))))))))))

(defun %jpeg-ycc-to-rgba (out w h c0 c1 c2 hmax vmax)
  "The common case of the compose pass, fast: three YCbCr planes (C0 C1 C2, each upsampled by
   replication) into OUT as RGBA.  libjpeg's fixed point (the coefficients x 2^16, rounded) in
   place of doubles -- on modus a double is boxed at every operation and this pass was most of a
   4K photo's decode -- and which sample of each plane a pixel takes is found once per column
   and per row, not per pixel."
  (declare (type (simple-array (unsigned-byte 8) (*)) out) (fixnum w h hmax vmax))
  (let ((p0 (jcomp-coef c0)) (p1 (jcomp-coef c1)) (p2 (jcomp-coef c2))
        (b0 (jcomp-bpl c0)) (b1 (jcomp-bpl c1)) (b2 (jcomp-bpl c2))
        (x0 (make-array w :element-type 'fixnum)) (x1 (make-array w :element-type 'fixnum))
        (x2 (make-array w :element-type 'fixnum)))
    (declare (type (simple-array (unsigned-byte 8) (*)) p0 p1 p2) (fixnum b0 b1 b2)
             (type (simple-array fixnum (*)) x0 x1 x2))
    (dotimes (px w)
      (setf (aref x0 px) (floor (* px (jcomp-h c0)) hmax)
            (aref x1 px) (floor (* px (jcomp-h c1)) hmax)
            (aref x2 px) (floor (* px (jcomp-h c2)) hmax)))
    (dotimes (py h)
      (let ((r0 (* (floor (* py (jcomp-v c0)) vmax) b0))
            (r1 (* (floor (* py (jcomp-v c1)) vmax) b1))
            (r2 (* (floor (* py (jcomp-v c2)) vmax) b2))
            (o (* py w 4)))
        (declare (fixnum r0 r1 r2 o))
        (dotimes (px w)
          (let ((yy (aref p0 (+ r0 (aref x0 px))))
                (cb (- (aref p1 (+ r1 (aref x1 px))) 128))
                (cr (- (aref p2 (+ r2 (aref x2 px))) 128)))
            (declare (fixnum yy cb cr))
            (setf (aref out o)       (clamp8i (+ yy (ash (+ (* 91881 cr) 32768) -16)))
                  (aref out (+ o 1)) (clamp8i (- yy (ash (+ (* 22554 cb) (* 46802 cr) 32768) -16)))
                  (aref out (+ o 2)) (clamp8i (+ yy (ash (+ (* 116130 cb) 32768) -16)))
                  (aref out (+ o 3)) 255)
            (setq o (+ o 4))))))))

(defun jpeg-decode (bytes &key (orient t) (scale 1))
  "Decode a baseline OR progressive JPEG byte vector to an IMG (RGBA), or NIL.
   ORIENT (the default): turned upright by its EXIF orientation, as a browser
   shows it (CSS image-orientation: from-image); NIL: as stored.
   SCALE 2, 4 or 8 decodes at 1/SCALE of the size, as libjpeg's scaled IDCT does: 8 is
   each 8x8 block's DC term alone, 2 and 4 average the block's pixels.  A 3840x2160 slide
   in a PDF shown 400 pixels wide needs 1/8 of it, and its full RGBA alone is 33 MB --
   more than a phone's document worker has (folio's PAINT-DECODED-IMAGE asks).
   A BASELINE frame keeps no coefficients at all: each block goes to pixels as it is decoded (a
   3840x2160 photo's coefficients were 25 MB); a progressive one refines every coefficient over its
   scans, so it keeps them."
  (when (and (>= (length bytes) 3) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
    (let ((n (length bytes)) (i 2)
          (qt (make-array 4 :initial-element nil))     ; 4 quant tables, NATURAL order
          (dht (make-array 4 :initial-element nil)) (aht (make-array 4 :initial-element nil))
          (w 0) (h 0) (comps nil) (ri 0) (hmax 1) (vmax 1) (mx 0) (my 0)
          (adobe nil)          ; APP14 Adobe colour transform: NIL / 0 / 1 / 2, else :seen
          (stream nil)         ; a BASELINE frame: blocks straight to pixels, no coefficient store
          (fscale 1)           ; the scale, where DECODE-SCAN's own SC (its components) hides SC
          (scratch (make-array 64 :element-type '(signed-byte 16) :initial-element 0))
          (sc (cond ((and (integerp scale) (>= scale 8)) 8) ((and (integerp scale) (>= scale 4)) 4)
                    ((and (integerp scale) (>= scale 2)) 2) (t 1)))
          (br nil) (spatial (make-array 64 :initial-element 0))
          (fblock (make-array 64 :initial-element 0)))
      (labels
          ((setup-frame ()
             (setf hmax (reduce #'max comps :key #'jcomp-h) vmax (reduce #'max comps :key #'jcomp-v)
                   mx (ceiling w (* 8 hmax)) my (ceiling h (* 8 vmax)))
             (dolist (c comps)
               (setf (jcomp-bpl c) (* mx (jcomp-h c)) (jcomp-bh c) (* my (jcomp-v c))
                     (jcomp-coef c)
                     (if stream
                         ;; the PLANE itself, at 1/FSCALE: what each block is written into
                         (let ((bs (floor 8 fscale)))
                           (make-array (* (jcomp-bpl c) bs (jcomp-bh c) bs)
                                       :element-type '(unsigned-byte 8) :initial-element 0))
                         (make-array (* (jcomp-bpl c) (jcomp-bh c) 64)
                                     :element-type '(signed-byte 16) :initial-element 0)))))
           (decode-scan (sos-i)
             ;; parse the SOS header at SOS-I, then decode its entropy segment.
             (let* ((ns (aref bytes (+ sos-i 2)))
                    (sc (loop for k below ns for o = (+ sos-i 3 (* k 2))
                              for c = (find (aref bytes o) comps :key #'jcomp-id)
                              collect (list c (ash (aref bytes (+ o 1)) -4) (logand (aref bytes (+ o 1)) 15))))
                    (p (+ sos-i 3 (* ns 2)))
                    (ss (aref bytes p)) (se (aref bytes (+ p 1)))
                    (ah (ash (aref bytes (+ p 2)) -4)) (al (logand (aref bytes (+ p 2)) 15)))
               (setf (jbr-pos br) (+ p 3) (jbr-cnt br) 0 (jbr-eof br) nil)
               (dolist (e sc) (setf (jcomp-pred (first e)) 0))
               (if (plusp ss)
                   ;; ---- AC scan: single component, non-interleaved block order ----
                   (destructuring-bind (c dc-id ac-id) (first sc)
                     (declare (ignore dc-id))
                     (let* ((ach (aref aht ac-id)) (eob (list 0)) (cnt 0)
                            (bw (ceiling (ceiling (* w (jcomp-h c)) hmax) 8))
                            (bh (ceiling (ceiling (* h (jcomp-v c)) vmax) 8)))
                       (dotimes (by bh)
                         (dotimes (bx bw)
                           (let ((off (* (+ (* by (jcomp-bpl c)) bx) 64)))
                             (if (zerop ah) (decode-ac-first br ach (jcomp-coef c) off ss se al eob)
                                 (decode-ac-refine br ach (jcomp-coef c) off ss se al eob)))
                           (incf cnt)
                           (when (and (plusp ri) (zerop (mod cnt ri)) (< cnt (* bw bh)))
                             (jbr-restart br) (setf (car eob) 0))))))
                   ;; ---- DC / baseline scan: interleaved MCU order ----
                   (let ((mcu 0))
                     (dotimes (myi my)
                       (dotimes (mxi mx)
                         (dolist (e sc)
                           (destructuring-bind (c dc-id ac-id) e
                             (let ((dch (aref dht dc-id)) (ach (aref aht ac-id)) (coef (jcomp-coef c)))
                               (dotimes (byi (jcomp-v c))
                                 (dotimes (bxi (jcomp-h c))
                                   (let ((off (* (+ (* (+ (* myi (jcomp-v c)) byi) (jcomp-bpl c))
                                                    (+ (* mxi (jcomp-h c)) bxi)) 64)))
                                     (cond ((and stream (= se 63) (zerop ah))
                                            ;; the block into SCRATCH, then straight to its pixels
                                            (fill scratch 0)
                                            (decode-baseline-block br dch ach c scratch 0)
                                            (let* ((bpl (jcomp-bpl c)) (idx (floor off 64)) (bs (floor 8 fscale)))
                                              (%jpeg-block-to-plane scratch 0 (aref qt (jcomp-qsel c)) coef (* bpl bs)
                                                                    (* (mod idx bpl) bs) (* (floor idx bpl) bs)
                                                                    fscale fblock spatial)))
                                           ((and (= se 63) (zerop ah)) (decode-baseline-block br dch ach c coef off))
                                           ((zerop ah) (decode-dc-first br dch c coef off al))
                                           (t (decode-dc-refine br coef off al)))))))))
                         (incf mcu)
                         (when (and (plusp ri) (zerop (mod mcu ri)) (< mcu (* mx my)))
                           (jbr-restart br) (dolist (e sc) (setf (jcomp-pred (first e)) 0))))))))))
        ;; ---- marker loop (parses tables + drives every scan) ----
        (block markers
          (loop
            (loop while (and (< i n) (/= (aref bytes i) #xff)) do (incf i))
            (loop while (and (< i n) (= (aref bytes i) #xff)) do (incf i))
            (when (>= i n) (return-from markers))
            (let ((m (aref bytes i))) (incf i)
              (cond
                ((= m #xd9) (return-from markers))                          ; EOI
                ((<= #xd0 m #xd7))                                          ; stray RSTn
                ((= m #xdb)                                                 ; DQT (store natural order)
                 (let ((len (j16 bytes i)) (p (+ i 2)))
                   (loop while (< p (+ i len)) do
                     (let* ((pq/tq (aref bytes p)) (prec (ash pq/tq -4)) (id (logand pq/tq 15))
                            (tbl (make-array 64)))
                       (incf p)
                       (dotimes (k 64)
                         (setf (aref tbl (aref +zigzag+ k))
                               (if (zerop prec) (aref bytes (+ p k)) (j16 bytes (+ p (* k 2))))))
                       (incf p (if (zerop prec) 64 128))
                       (setf (aref qt id) tbl)))
                   (incf i len)))
                ((or (= m #xc0) (= m #xc1) (= m #xc2))                      ; SOF0/1 (baseline) / SOF2 (progressive)
                 (let ((nc (aref bytes (+ i 7))))
                   (setf h (j16 bytes (+ i 3)) w (j16 bytes (+ i 5)) comps nil)
                   (dotimes (c nc)
                     (let ((o (+ i 8 (* c 3))))
                       (push (make-jcomp :id (aref bytes o) :h (ash (aref bytes (+ o 1)) -4)
                                         :v (logand (aref bytes (+ o 1)) 15) :qsel (aref bytes (+ o 2))) comps)))
                   (setf comps (nreverse comps)
                         ;; a progressive frame refines every coefficient over many scans, so it keeps them all
                         stream (/= m #xc2) fscale sc)
                   (incf i (j16 bytes i))
                   (setup-frame)
                   (setf br (make-jbr :data bytes))))
                ((= m #xc4)                                                 ; DHT (in the C3-CF range, so BEFORE the decline)
                 (let ((len (j16 bytes i)) (p (+ i 2)))
                   (loop while (< p (+ i len)) do
                     (let* ((tc/th (aref bytes p)) (class (ash tc/th -4)) (id (logand tc/th 15))
                            (counts (make-array 16)) (total 0))
                       (incf p)
                       (dotimes (l 16) (setf (aref counts l) (aref bytes (+ p l))) (incf total (aref bytes (+ p l))))
                       (incf p 16)
                       (let ((syms (subseq bytes p (+ p total))))
                         (incf p total)
                         (if (zerop class) (setf (aref dht id) (build-huff counts syms))
                             (setf (aref aht id) (build-huff counts syms))))))
                   (incf i len)))
                ((<= #xc3 m #xcf) (return-from markers))                    ; lossless / arithmetic / other SOF — decline
                ((= m #xdd) (setf ri (j16 bytes (+ i 2))) (incf i (j16 bytes i)))   ; DRI
                ((= m #xda)                                                 ; SOS — decode this scan
                 (when (null br) (return-from markers))
                 (let ((sos-i i))
                   ;; a truncated / malformed scan must not abort the whole image:
                   ;; keep whatever coefficients decoded so far (progressive-friendly).
                   (ignore-errors (decode-scan sos-i))
                   ;; resume marker parsing right after the entropy segment.
                   (setf i (if (and br (> (jbr-pos br) sos-i)) (jbr-pos br) (+ sos-i (j16 bytes i))))))
                ((= m #xee)                                                 ; APP14 (Adobe colour transform)
                 (let ((len (j16 bytes i)))
                   (when (and (>= len 14)
                              (= (aref bytes (+ i 2)) 65) (= (aref bytes (+ i 3)) 100) ; "Ad"
                              (= (aref bytes (+ i 4)) 111) (= (aref bytes (+ i 5)) 98) ; "ob"
                              (= (aref bytes (+ i 6)) 101))                            ; "e"
                     (setf adobe (aref bytes (+ i 13))))                              ; transform byte
                   (incf i len)))
                (t (incf i (j16 bytes i))))))))                              ; APPn / COM / …
      (when (or (zerop w) (zerop h) (null comps)) (return-from jpeg-decode nil))
      ;; the picture's own size at 1/SC (rounded up, as the planes are whole blocks)
      (setf w (ceiling w sc) h (ceiling h sc))
      ;; ---- dequantise + IDCT into per-component planes (a streamed frame's are made already) ----
      (if stream
          (dolist (c comps) (setf (jcomp-bpl c) (* (jcomp-bpl c) (floor 8 sc))))   ; bpl -> plane width
      (dolist (c comps)
        (let* ((bs (floor 8 sc))                 ; a block's side in the output: 8, 4, 2 or 1
               (cw (* (jcomp-bpl c) bs)) (ch (* (jcomp-bh c) bs)) (coef (jcomp-coef c))
               (qtab (aref qt (jcomp-qsel c)))
               (plane (make-array (* cw ch) :element-type '(unsigned-byte 8) :initial-element 0)))
          (dotimes (by (jcomp-bh c))
            (dotimes (bx (jcomp-bpl c))
              (%jpeg-block-to-plane coef (* (+ (* by (jcomp-bpl c)) bx) 64) qtab plane cw
                                    (* bx bs) (* by bs) sc fblock spatial)))
          (setf (jcomp-coef c) plane (jcomp-bpl c) cw))))   ; reuse slots: coef->plane, bpl->plane width
      ;; ---- compose RGBA (upsample chroma by replication) ----
      (let* ((nc (length comps))
             (out (make-array (* w h 4) :element-type '(unsigned-byte 8)))
             (c0 (first comps)) (c1 (second comps)) (c2 (third comps)) (c3 (fourth comps))
             ;; a 3-component frame is YCbCr unless an Adobe APP14 marker says 0 (RGB).
             ;; a 4-component frame is CMYK (transform 0) or YCCK (transform 2); Adobe
             ;; stores CMYK/YCCK inverted, so the true colour is C_stored*K_stored/255.
             (ycc3 (and (= nc 3) (not (eql adobe 0))))
             (ycck (and (= nc 4) (eql adobe 2)))
             (cmyk-inv (and (= nc 4) adobe)))       ; Adobe present => inverted CMYK
        (if ycc3
            (%jpeg-ycc-to-rgba out w h c0 c1 c2 hmax vmax)
        (dotimes (py h)
          (dotimes (px w)
            (flet ((samp (c) (aref (jcomp-coef c)
                                   (+ (* (floor (* py (jcomp-v c)) vmax) (jcomp-bpl c))
                                      (floor (* px (jcomp-h c)) hmax)))))
              (let ((o (* (+ (* py w) px) 4)))
                (cond
                  ;; ---- 4-component CMYK / YCCK ----
                  ((= nc 4)
                   (let (ci mi yi ki)
                     (if ycck
                         ;; YCCK: YCbCr -> RGB gives the (inverted) CMY directly; K is comp3.
                         (let ((yy (samp c0)) (cb (- (samp c1) 128)) (cr (- (samp c2) 128)))
                           (setf ci (clamp8 (+ yy (* 1.402d0 cr)))
                                 mi (clamp8 (- yy (* 0.344136d0 cb) (* 0.714136d0 cr)))
                                 yi (clamp8 (+ yy (* 1.772d0 cb)))
                                 ki (samp c3)))
                         (setf ci (samp c0) mi (samp c1) yi (samp c2) ki (samp c3)))
                     (if cmyk-inv
                         ;; inverted CMYK (Adobe): R = C_stored * K_stored / 255
                         (setf (aref out o)       (floor (* ci ki) 255)
                               (aref out (+ o 1)) (floor (* mi ki) 255)
                               (aref out (+ o 2)) (floor (* yi ki) 255))
                         ;; plain CMYK: R = (255-C)(255-K)/255
                         (setf (aref out o)       (floor (* (- 255 ci) (- 255 ki)) 255)
                               (aref out (+ o 1)) (floor (* (- 255 mi) (- 255 ki)) 255)
                               (aref out (+ o 2)) (floor (* (- 255 yi) (- 255 ki)) 255)))))
                  ;; ---- 3-component YCbCr ----
                  (ycc3
                   (let ((yy (samp c0)) (cb (- (samp c1) 128)) (cr (- (samp c2) 128)))
                     (setf (aref out o)       (clamp8 (+ yy (* 1.402d0 cr)))
                           (aref out (+ o 1)) (clamp8 (- yy (* 0.344136d0 cb) (* 0.714136d0 cr)))
                           (aref out (+ o 2)) (clamp8 (+ yy (* 1.772d0 cb))))))
                  ;; ---- 3-component RGB (Adobe transform 0) ----
                  ((= nc 3)
                   (setf (aref out o) (samp c0) (aref out (+ o 1)) (samp c1) (aref out (+ o 2)) (samp c2)))
                  ;; ---- 1-component grayscale ----
                  (t (let ((g (samp c0))) (setf (aref out o) g (aref out (+ o 1)) g (aref out (+ o 2)) g))))
                (setf (aref out (+ o 3)) 255))))))
        (let ((img (make-img :w w :h h :rgba out)))
          (if orient (orient-img img (jpeg-orientation bytes)) img))))))

;;; ---- EXIF orientation ---------------------------------------------------
;;; A camera stores its pixels as the sensor read them and says in EXIF (tag
;;; 0x0112, in the first APP1) how to turn them upright; a phone's portrait
;;; photo is a landscape image with orientation 6.

(defun jpeg-orientation (bytes)
  "BYTES' EXIF orientation, 1 (as stored) to 8; 1 when it has none."
  (let ((n (length bytes)) (i 2))
    (loop while (and (<= (+ i 4) n) (= (aref bytes i) #xff))
          do (let ((m (aref bytes (1+ i))))
               (incf i 2)
               (unless (or (= m #xd8) (<= #xd0 m #xd9))
                 (let ((seg (j16 bytes i)))
                   (when (or (< seg 2) (> seg (- n i))) (return))
                   (when (and (= m #xe1) (>= seg 14)
                              (every #'= (subseq bytes (+ i 2) (+ i 8)) '(69 120 105 102 0 0)))   ; "Exif\0\0"
                     (let* ((tf (+ i 8)) (tlen (- seg 8))
                            (le (and (= (aref bytes tf) 73) (= (aref bytes (1+ tf)) 73))))       ; "II"
                       (flet ((u16 (o) (if le (logior (aref bytes o) (ash (aref bytes (1+ o)) 8)) (j16 bytes o)))
                              (u32 (o) (let ((a (aref bytes o)) (b (aref bytes (+ o 1)))
                                             (c (aref bytes (+ o 2))) (d (aref bytes (+ o 3))))
                                         (if le (logior a (ash b 8) (ash c 16) (ash d 24))
                                             (logior (ash a 24) (ash b 16) (ash c 8) d)))))
                         (when (or (< tlen 8) (/= (u16 (+ tf 2)) 42)) (return))
                         (let ((ifd (u32 (+ tf 4))))
                           (when (or (> ifd tlen) (< (- tlen ifd) 2)) (return))
                           (let ((count (u16 (+ tf ifd))))
                             (when (> (* 12 count) (- tlen ifd 2)) (return))
                             (dotimes (k count)
                               (let ((e (+ tf ifd 2 (* 12 k))))
                                 (when (and (= (u16 e) #x112) (= (u16 (+ e 2)) 3) (= (u32 (+ e 4)) 1))
                                   (let ((o (u16 (+ e 8)))) (return-from jpeg-orientation (if (<= 1 o 8) o 1))))))
                             (return))))))
                   (when (= m #xda) (return))
                   (incf i seg)))))
    1))

(defun orient-img (img orientation)
  "IMG turned and mirrored upright by EXIF ORIENTATION (1 to 8): a new IMG, or IMG for 1."
  (if (or (null orientation) (= orientation 1))
      img
      (let* ((w (img-w img)) (h (img-h img)) (src (img-rgba img))
             (ow (if (>= orientation 5) h w)) (oh (if (>= orientation 5) w h))
             (out (make-array (* ow oh 4) :element-type '(unsigned-byte 8))))
        (dotimes (y h)
          (dotimes (x w)
            (multiple-value-bind (ox oy)
                (case orientation
                  (2 (values (- w 1 x) y))           (3 (values (- w 1 x) (- h 1 y)))
                  (4 (values x (- h 1 y)))           (5 (values y x))
                  (6 (values (- h 1 y) x))           (7 (values (- h 1 y) (- w 1 x)))
                  (8 (values y (- w 1 x)))           (t (values x y)))
              (replace out src :start1 (* 4 (+ ox (* oy ow))) :start2 (* 4 (+ x (* y w))) :end2 (* 4 (+ 1 x (* y w)))))))
        (make-img :w ow :h oh :rgba out))))
