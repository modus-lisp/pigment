# pigment

A standalone, pure Common Lisp image codec library.  It decodes the raster and
vector image formats a browser needs — PNG, GIF, JPEG, WebP, SVG, and `data:`
URIs — into a single straight-alpha RGBA bitmap (`img`).  No FFI, no libpng /
libjpeg / librsvg: everything is native Lisp.

pigment does not depend on any renderer or web engine; it is a leaf codec
library reusable on its own (for example, as the SVG/raster decoder for another
project).

## Dependencies

- [`chipz`](https://github.com/froydnj/chipz) — zlib/deflate inflate (PNG `IDAT`).
- [`scribe`](../scribe) — RGBA rasterizer canvas (SVG rendering target).
- [`stencil`](../stencil) — pure-CL SVG parser + renderer.
- [`webp-pure`](../webp-pure) — pure-CL WebP (VP8) decoder.

## What it decodes

| Format | Entry / notes |
| ------ | ------------- |
| PNG    | `png-decode` — color types 0/2/3/4/6, 1/2/4/8-bit, Adam7 interlace, `tRNS`. |
| GIF    | `gif-decode` — dimensions-only (first-frame canvas). |
| JPEG   | `jpeg-decode` — baseline **and** progressive (spectral selection + successive approximation), restart intervals, chroma upsampling. |
| WebP   | `webp-to-img` — wraps `webp-pure:decode` (VP8 lossy intra). |
| SVG    | `decode-svg-source` — rendered through stencil at intrinsic size; also carries CSS intrinsic sizing (`sw`/`sh`/`sr`). |

## The `img` struct

```lisp
(defstruct img w h rgba
  ;; SVG intrinsic sizing (NIL for raster images): intrinsic width/height and
  ;; ratio (a number, 0, or :infinite) so a consumer can run the CSS default
  ;; sizing algorithm instead of using the rasterized bitmap size.
  (sw nil) (sh nil) (sr nil))
```

`rgba` is a `(w*h*4)` `(unsigned-byte 8)` vector, straight (non-premultiplied)
alpha, row-major RGBA.

## Usage

```lisp
(asdf:load-system "pigment")

;; A data: URI (PNG/GIF/JPEG/WebP/SVG) -> IMG
(let ((img (pigment:decode-image "data:image/png;base64,iVBORw0KGgo...")))
  (values (pigment:img-w img) (pigment:img-h img)))

;; Raw bytes from a fetch, dispatched on MIME or magic bytes -> IMG
(pigment:decode-image-bytes byte-vector "image/jpeg")
```

## License

MIT.
