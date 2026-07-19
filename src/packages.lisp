;;;; src/packages.lisp — pigment package definition.
(defpackage #:pigment
  (:use #:cl)
  (:local-nicknames (#:sc #:scribe) (#:st #:stencil))
  (:export
   ;; the decoded-image struct + accessors
   #:img #:make-img #:img-w #:img-h #:img-rgba #:img-sw #:img-sh #:img-sr
   ;; top-level dispatchers
   #:decode-image #:decode-image-bytes
   ;; per-format decoders
   #:png-decode #:gif-decode #:jpeg-decode #:jpeg-size #:webp-to-img
   #:decode-svg-source #:decode-svg-image #:rgba-canvas->img
   ;; helpers
   #:parse-data-uri #:base64-decode #:svg-bytes-p))
