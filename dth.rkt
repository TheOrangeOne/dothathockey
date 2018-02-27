#lang racket

(require "data.rkt")
(require "page.rkt")
(require "pages/today.rkt")

(define pages (list today-page))
(void (map output-page pages))
