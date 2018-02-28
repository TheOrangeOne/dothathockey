#lang racket

(require "page.rkt")
(require "pages/today.rkt")
(require "pages/ratings.rkt")

(define pages (list today-page ratings-page))
(output-pages pages)
