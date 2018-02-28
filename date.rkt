#lang racket

(provide todayf
         today)

(define (-day time)
  (- time (* 24 60 60 )))

(define (pad-date d)
  (~a d #:align 'right #:min-width 2 #:left-pad-string "0"))

(define (fdate date)
  (let ((year (date-year date))
       (month (pad-date (date-month date)))
       (day (pad-date (date-day date))))
       (~a year "-" month "-" day)))

(define today (seconds->date (current-seconds)))
(define todayf (fdate today))
