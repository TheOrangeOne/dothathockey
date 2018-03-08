#lang racket

(require scribble/html/html)
(require scribble/html/xml)
(require "../page.rkt")
(require "../rating.rkt")
(require "../date.rkt")

(provide cur-ratings-page rating-pages html-grad-rating)

(define (html-grad-rating rating)
  (define r (string->number rating))
  (define cg (exact-round (* (/ r 100) 255)))
  (define cr (exact-round (- 255 (* (/ r 100) 255))))
  (define style (~a "color: rgb("cr","cg", 0)"))
  (element 'span 'style: style rating))

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
          [(< diff 0) "color: red;"]))
  (if (= diff 0) empty (element 'span 'style: diff-style (~a fmt-diff))))

(define table-style "font-size: 1.1em; font-family: \"Lucida Console\", Monaco, monospace; line-height: 1.5; margin-left:auto; margin-right:auto;")
(define (ttd x) (element 'td 'style: "padding-left: 10px; padding-right: 10px" x))

(define (html-rating data)
  (define abbr (first data))
  (define rating (format-rating (second data)))
  (define src (third data))
  (define diff (fourth data))
  (tr (ttd  (span (svg-team src) (b abbr))) (ttd rating) (ttd (html-diff diff))))


(define (html-ratings data)
  (element 'table 'style: table-style (map html-rating data)))

(define (gen-src date team-ratings)
  (div (h1 (html-prev-rating date)
           (~a " " date " ")
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
