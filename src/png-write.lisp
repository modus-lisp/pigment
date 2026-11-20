;;;; src/png-write.lisp — RGBA to PNG bytes, and to a file.
;;;;
;;;; For screenshots and for looking at what a decoder made: a framebuffer, a rendered PDF
;;;; page, a decoded JPEG.  Colour type 6 (RGBA) or, with :ALPHA NIL, 2 (RGB), 8 bits a
;;;; channel; each row filtered with SUB (the byte to its left subtracted), which turns a
;;;; screen's flat runs and gradients into zeros and small numbers that deflate well; zlib
;;;; from cram.

(in-package #:pigment)

(defun %png-u32 (out n)
  (vector-push-extend (ldb (byte 8 24) n) out) (vector-push-extend (ldb (byte 8 16) n) out)
  (vector-push-extend (ldb (byte 8 8) n) out) (vector-push-extend (ldb (byte 8 0) n) out))

(defun %png-chunk (out type data)
  "Append a chunk: length, TYPE (a 4-character string), DATA, then the CRC of type and data."
  (let ((td (make-array (+ 4 (length data)) :element-type '(unsigned-byte 8))))
    (dotimes (i 4) (setf (aref td i) (char-code (char type i))))
    (replace td data :start1 4)
    (%png-u32 out (length data))
    (loop for b across td do (vector-push-extend b out))
    (%png-u32 out (cram:crc32 td))))

(defun png-encode (rgba w h &key (alpha t))
  "RGBA (W*H*4 octets, row-major, straight alpha) as a PNG byte vector.  ALPHA NIL writes
   RGB and drops the alpha channel (a screenshot of an opaque framebuffer)."
  (let* ((bpp (if alpha 4 3))
         (stride (* w bpp))
         (raw (make-array (* h (1+ stride)) :element-type '(unsigned-byte 8)))
         (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dotimes (y h)
      (let ((o (* y (1+ stride))) (src (* y w 4)))
        (setf (aref raw o) 1)                                   ; filter SUB
        (dotimes (x w)
          (dotimes (c bpp)
            (let ((v (aref rgba (+ src (* x 4) c)))
                  (left (if (zerop x) 0 (aref rgba (+ src (* (1- x) 4) c)))))
              (setf (aref raw (+ o 1 (* x bpp) c)) (logand (- v left) 255)))))))
    (loop for b in '(137 80 78 71 13 10 26 10) do (vector-push-extend b out))
    (let ((ihdr (make-array 13 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref ihdr 0) (ldb (byte 8 24) w) (aref ihdr 1) (ldb (byte 8 16) w)
            (aref ihdr 2) (ldb (byte 8 8) w) (aref ihdr 3) (ldb (byte 8 0) w)
            (aref ihdr 4) (ldb (byte 8 24) h) (aref ihdr 5) (ldb (byte 8 16) h)
            (aref ihdr 6) (ldb (byte 8 8) h) (aref ihdr 7) (ldb (byte 8 0) h)
            (aref ihdr 8) 8 (aref ihdr 9) (if alpha 6 2))       ; depth 8; compression, filter, interlace 0
      (%png-chunk out "IHDR" ihdr))
    (%png-chunk out "IDAT" (cram:zlib-compress raw))
    (%png-chunk out "IEND" (make-array 0 :element-type '(unsigned-byte 8)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun write-png (path image &key w h (alpha t))
  "Write IMAGE -- an IMG, or an RGBA octet vector with W and H -- to PATH as a PNG.  PATH."
  (multiple-value-bind (rgba iw ih)
      (if (img-p image) (values (img-rgba image) (img-w image) (img-h image)) (values image w h))
    (let ((bytes (png-encode rgba iw ih :alpha alpha)))
      (with-open-file (s path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence bytes s))
      path)))
