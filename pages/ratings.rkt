#lang racket

(require scribble/html/html)
(require "../page.rkt")
(require "../rating.rkt")
(require "../date.rkt")

(provide ratings-page)

(define (html-rating data)
  (define abbr (first data))
  (define rating (format-rating (second data)))
  (li (span rating nbsp (b abbr))))

(define (html-ratings data)
  (ul (map html-rating data)))

(define fn "ratings")
(define src (div (h1 (~a "[" todayf "]" " ratings"))
                  (html-ratings team-ratings)))
(define ratings-page (make-page src fn))
