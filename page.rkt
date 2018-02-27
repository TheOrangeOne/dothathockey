#lang racket

(require scribble/html/html)
(require scribble/html/xml)

(provide make-page
         output-page)

(define-struct page (src fn))

(define meta-mobile (element 'meta 'name: "viewport" 'content: "width=device-width, initial-scale=1"))

(define (wrap-src src)
  (element 'html 'lang: "en"
           (head (title "DTH") meta-mobile)
           src))

(define (output-page page)
  (define filename (page-fn page))
  (define source (wrap-src (page-src page)))
  (define file (open-output-file filename #:exists 'replace))
  (output-xml (doctype 'html) file)
  (output-xml source file)
  (close-output-port file))
