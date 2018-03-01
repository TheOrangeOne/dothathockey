#lang racket

(require scribble/html/html)
(require scribble/html/xml)

(provide make-page output-pages svg-team content-style)

(define-struct page (src fn))

(define content-style "font-size: 1.2em; font-family: \"Lucida Console\", Monaco, monospace; line-height: 1.5;")

(define meta-mobile
  (element 'meta 'name: "viewport" 'content: "width=device-width,initial-scale=1"))

(define (svg-team src)
  (element
    'img
    'style: "width: 1.2em; margin-right: 0.2em; vertical-align: middle;"
    'src: src))

(define (wrap-src src header)
  (element 'html 'lang: "en"
           (head (title "DTH") meta-mobile)
           (body header src)))

(define (gen-header pages)
  (if (empty? pages)
    empty
    (let* ([name (page-fn (first pages))]
           [href (~a name ".html")])
      (span (element 'a 'href: href name) nbsp (gen-header (rest pages))))))

(define (output-page header page)
  (define filename (page-fn page))
  (define source (wrap-src (page-src page) header))
  (define file (open-output-file (~a "build/" filename ".html") #:exists 'replace))
  (output-xml (doctype 'html) file)
  (output-xml source file)
  (close-output-port file))

(define (output-pages pages)
  (define header (gen-header pages))
  (void (map (curry output-page header) pages)))
