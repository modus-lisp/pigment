;;;; decode-gate.lisp — the smallest real images of each kind, decoded to pixels.
;;;;
;;;;     sbcl --load inspect/decode-gate.lisp
;;;;
;;;; DECODE-IMAGE-BYTES dispatches on magic bytes, and the interesting property of a
;;;; dispatcher is that it can be confidently wrong: hand it a PNG and get a GIF
;;;; decoder, and the failure is a blank image rather than an error.  So each case
;;;; below asserts the PIXEL that came out, not merely that something did.
;;;;
;;;; The two raster images are one pixel each and are written out byte by byte here,
;;;; generated to be valid — a real zlib IDAT with a correct CRC, a real GIF LZW
;;;; stream — so this file needs no fixture directory, no network, and no font.
;;;; Which is the point: it is the same test on any machine.

(require :asdf)
(let ((here (make-pathname :name nil :type nil
                           :defaults (or *load-truename* *compile-file-truename*))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor") (:exclude "deps") :inherit-configuration)))
(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :pigment)))

(in-package #:pigment)

(defvar *pass* 0) (defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (progn (incf *pass*) (format t "  ok   ~a~@[ — ~a~]~%" name detail))
      (progn (incf *fail*) (format t "  FAIL ~a~@[ — ~a~]~%" name detail)))
  (finish-output))

(defun bv (&rest ns) (make-array (length ns) :element-type '(unsigned-byte 8)
                                             :initial-contents ns))
(defun px (img i) (list (aref (img-rgba img) (* 4 i)) (aref (img-rgba img) (+ 1 (* 4 i)))
                        (aref (img-rgba img) (+ 2 (* 4 i))) (aref (img-rgba img) (+ 3 (* 4 i)))))

;;; A 1x1 truecolour-with-alpha PNG holding one opaque crimson pixel.
(defparameter *png* (bv   137 80 78 71 13 10 26 10 0 0 0 13 73 72 68 82 0 0 0 1 0 0 0 1 8 6 0 0 0 31 21 196 137 0 0 0 13 73 68 65 84 120 156 99 184 35 98 243 31 0 5 40 2 44 16 8 117 219 0 0 0 0 73 69 78 68 174 66 96 130))
;;; A 1x1 GIF87a whose single palette entry is dodger blue.
(defparameter *gif* (bv   71 73 70 56 55 97 1 0 1 0 128 0 0 30 144 255 0 0 0 44 0 0 0 0 1 0 1 0 0 2 2 68 1 0 59))

(format t "~&== a PNG, down to the pixel ==~%")
(let ((img (decode-image-bytes *png*)))
  (ok "it decodes at all" img)
  (when img
    (ok "one pixel wide and high" (and (= (img-w img) 1) (= (img-h img) 1))
        (format nil "~dx~d" (img-w img) (img-h img)))
    (ok "RGBA is four bytes per pixel" (= (length (img-rgba img)) 4)
        (format nil "~d" (length (img-rgba img))))
    ;; The actual colour, which is what a wrong decoder gets wrong quietly.
    (ok "and the pixel is the crimson that went in" (equal (px img 0) '(220 20 60 255))
        (format nil "~a" (px img 0)))))

(format t "~&== a GIF, likewise ==~%")
(let ((img (decode-image-bytes *gif*)))
  (ok "it decodes at all" img)
  (when img
    (ok "one pixel wide and high" (and (= (img-w img) 1) (= (img-h img) 1))
        (format nil "~dx~d" (img-w img) (img-h img)))
    (ok "and the palette entry came through opaque"
        (equal (px img 0) '(30 144 255 255)) (format nil "~a" (px img 0)))))

(format t "~&== dispatch is on the magic, not on the caller's word ==~%")
;; A PNG announced as a GIF must still decode as a PNG: the bytes are the truth, and
;; a server that mislabels a body is the ordinary case rather than the strange one.
(let ((img (decode-image-bytes *png* "image/gif")))
  (ok "a mislabelled PNG is still read as a PNG"
      (and img (equal (px img 0) '(220 20 60 255)))
      (and img (format nil "~a" (px img 0)))))

(format t "~&== and nonsense is NIL, not a condition ==~%")
(ok "empty input" (null (decode-image-bytes (bv))))
(ok "eight bytes of noise" (null (decode-image-bytes (bv 1 2 3 4 5 6 7 8))))
(ok "a truncated PNG header" (null (decode-image-bytes (bv 137 80 78 71))))

(format t "~&== SVG is text, and sizes to what it is asked for ==~%")
(let* ((svg "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='2'></svg>")
       (bytes (map '(vector (unsigned-byte 8)) #'char-code svg))
       (img (decode-image-bytes bytes "image/svg+xml")))
  (ok "an SVG decodes from its MIME type" img)
  (when img
    (ok "at its own intrinsic size when none is asked for"
        (and (= (img-w img) 4) (= (img-h img) 2))
        (format nil "~dx~d" (img-w img) (img-h img)))))

(format t "~&~%~d passed, ~d failed~%" *pass* *fail*)
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
