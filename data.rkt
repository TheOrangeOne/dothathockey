#lang racket

(require "api.rkt")

(provide today-games)

(define date (seconds->date (current-seconds)))
(define todayf (~a (date-year date) "-" (date-month date) "-" (date-day date)))
(define schedule-params (~a "?startDate=" todayf "&" "endDate=" todayf))

(define sched-blob (nhl-get (string-append "schedule" schedule-params)))
(define today-sched (first (hash-ref sched-blob 'dates)))
(define num-games (hash-ref today-sched 'totalGames))
(define today-games (hash-ref today-sched 'games))
