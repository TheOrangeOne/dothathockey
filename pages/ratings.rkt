#lang racket

(require scribble/html/html)
(require scribble/html/xml)
(require "../page.rkt")
(require "../rating.rkt")
(require "../date.rkt")

(provide cur-ratings-page rating-pages)

(define (html-prev-rating date)
  (define prev-date (get-prev-game-day date))
  (define prev-fn (~a (rating-fn prev-date) ".html"))
  (if (string=? prev-date "") "←" (element 'a 'href: prev-fn "←")))


(define (html-next-rating date)
  (define next-date (get-next-game-day date))
  (define next-fn (~a (rating-fn next-date) ".html"))
  (if (string=? next-date "") "→" (element 'a 'href: next-fn "→")))

(define (rating-fn date) (~a date "-ratings"))

(define (html-diff diff)
  (define fdiff (format-diff diff))
  (define rnd-diff (string->number (real->decimal-string diff 2)))
  (define fmt-diff (~a (if (> diff 0) "+" "") fdiff))
  (define diff-style
    (cond [(> diff 0) "color: green;"]
          [(< diff 0) "color: red;"]
          [(= diff 0) "color: black;"]))
  (element 'span 'style: diff-style (~a "("fmt-diff")")))

(define (html-rating data)
  (define abbr (first data))
  (define rating (format-rating (second data)))
  (define src (third data))
  (define diff (fourth data))
  (li (span (svg-team src) (b abbr) nbsp rating nbsp (html-diff diff))))

(define (html-ratings data)
  (element 'ul 'style: content-style (map html-rating data)))

(define (gen-src date team-ratings)
  (div (h1 (html-prev-rating date)
           (~a date " ratings")
           (html-next-rating date))
       (html-ratings team-ratings)))

(define (gen-rating-page date-team-rating)
  (define date (first date-team-rating))
  (define ratings (second date-team-rating))
  (define fn (rating-fn date))
  (define src (gen-src date ratings))
  (make-page src "" fn))

(define rating-pages (map gen-rating-page date-team-ratings))

(define cur-src (gen-src todayf team-ratings))
(define fn "ratings")
(define title "ratings")
(define cur-ratings-page (make-page cur-src title fn))
