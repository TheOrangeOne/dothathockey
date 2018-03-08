#lang racket

(require "page.rkt")
(require "pages/today.rkt")
(require "pages/ratings.rkt")

(define pages (append (list today-page cur-ratings-page) rating-pages))
(output-pages pages)
