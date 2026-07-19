;;;; pigment.asd — pure Common Lisp image codecs to RGBA.
(asdf:defsystem "pigment"
  :description "Pure-CL image codecs: PNG/GIF/JPEG/WebP/SVG and data: URIs
decoded to a straight-alpha RGBA IMG struct.  No FFI, no libpng/libjpeg.
Standalone — depends only on the sibling pure-CL codecs (scribe/stencil for
SVG, webp-pure for WebP) and chipz for PNG inflate."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("scribe" "stencil" "webp-pure" "chipz")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "jpeg")
                             (:file "image")))))
