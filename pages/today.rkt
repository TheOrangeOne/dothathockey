#lang racket

(require scribble/html/html)
(require scribble/html/xml)
(require "../page.rkt")
(require "../stores/today.rkt")
(require "ratings.rkt")

(provide today-page)


(define (html-team team)
  (define abbr (first team))
  (define rating (second team))
  (define src (third team))
  (element 'span (svg-team src) abbr nbsp (html-grad-rating rating)))

(define (html-game game)
  (define home (html-team (first game)))
  (define away (html-team (second game)))
  (div away nbsp "at" nbsp home))

(define (html-games games)
  (element 'div 'style: content-style (map html-game games)))

(define fn "today")
(define title "today")
(define src (div (h1 "today's games")
                  (html-games games)
                  (h1 "close-ish games")
                  (html-games close-games)))
                  ; (h1 "Good games:")
                  ;(html-games good-games)))

(define today-page (make-page src title fn))
