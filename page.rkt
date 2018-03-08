#lang racket

(require scribble/html/html)
(require scribble/html/xml)

(provide make-page output-pages svg-team content-style)

(define-struct page (src title fn))

(define site-style (style "body{margin:40px
auto;max-width:650px;line-height:1.6;font-size:18px;color:#444;padding:0
10px}h1,h2,h3{line-height:1.2}</style>"))

(define content-style "font-size: 1.1em; font-family: \"Lucida Console\", Monaco, monospace; line-height: 1.5; list-style-type: none;")

(define meta-mobile
  (element 'meta 'name: "viewport" 'content: "width=device-width,initial-scale=1"))

(define (svg-team src)
  (element
    'img
    'style: "width: 1.1em; margin-right: 0.2em; vertical-align: middle;"
    'src: src))

(define (wrap-src src header)
  (element 'html 'lang: "en"
           (head (title "DTH") meta-mobile site-style)
           (element 'body 'style: "text-align: center;" header src)))

(define (gen-header pages)
  (if (empty? pages)
    empty
    (let* ([name (page-title (first pages))]
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
  (define header (gen-header (filter (lambda (x) (not (string=? (page-title x) ""))) pages)))
  (void (map (curry output-page header) pages)))
