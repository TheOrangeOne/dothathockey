#lang racket

(require racket/date)

(provide todayf
         today
         date-prev
         date-next)

(define EST-OFFSET (* 5 60 60))

(define (-day time)
  (- time (* 24 60 60 )))

(define (+day time)
  (+ time (* 24 60 60 )))

(define (string->time date)
  (define split (map string->number (string-split date "-")))
  (define year (first split))
  (define month (second split))
  (define day (third split))
  (find-seconds 0 0 0 day month year))

(define (date-prev date)
  (define date-time (string->time date))
  (fdate (seconds->date (-day date-time) EST-OFFSET)))

(define (date-next date)
  (define date-time (string->time date))
  (fdate (seconds->date (+day date-time) EST-OFFSET)))

(define (pad-date d)
  (~a d #:align 'right #:min-width 2 #:left-pad-string "0"))

(define (fdate date)
  (let ((year (date-year date))
       (month (pad-date (date-month date)))
       (day (pad-date (date-day date))))
       (~a year "-" month "-" day)))

(define today (seconds->date (current-seconds) EST-OFFSET))
(define todayf (fdate today))
