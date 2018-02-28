#lang racket

(require scribble/html/html)
(require scribble/html/xml)

(provide make-page output-pages)

(define-struct page (src fn))

(define meta-mobile
  (element 'meta 'name: "viewport" 'content: "width=device-width, initial-scale=1"))

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
