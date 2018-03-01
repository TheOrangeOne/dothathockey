#lang racket

(require json)
(require racket/hash)
(require "api.rkt")
(require "date.rkt")

(provide schedule teams id->js)

(define SEASON-START "2017-10-04")

(define (id->js id)
  (cond [(symbol? id) id]
        [(number? id)
         (string->symbol (number->string id))]
        [(string? id)
         (string->symbol id)]))

(define (parse-teams lteams [hteams (make-immutable-hash)])
  (if (empty? lteams)
    hteams
    (let* ([team (first lteams)]
           [id (number->string (hash-ref team 'id))])
      (parse-teams (rest lteams) (hash-set hteams (string->symbol id) team)))))

(define (get-teams)
  (define raw (nhl-get "teams"))
  (define teams (hash-ref raw 'teams))
  (parse-teams teams))

(define teams-file-name "data/teams.json")
(close-output-port (open-output-file teams-file-name #:exists 'append))
(define teams-file-in (open-input-file teams-file-name))
(define teams-cached (read-json teams-file-in))
(close-input-port teams-file-in)
(define teams (if (eof-object? teams-cached) (get-teams) teams-cached))
(define teams-file-out (open-output-file teams-file-name #:exists 'replace))
(write-json teams teams-file-out)
(close-output-port teams-file-out)

(define sched-file-name "data/sched.json")
(close-output-port (open-output-file sched-file-name #:exists 'append))
(define sched-file-in (open-input-file sched-file-name))
(define sched-raw (read-json sched-file-in))
(define cached-sched (if (eof-object? sched-raw) empty sched-raw))
(close-input-port sched-file-in)

(define (parse-dates sched)
  (if (empty? sched)
    empty
    (cons (hash-ref (first sched) 'date) (parse-dates (rest sched)))))

; get the start date for the range of data we need to request
(define (get-start-date dates)
  (if (empty? dates)
    SEASON-START
    (last dates)))

; assume cached and part both sorted ascending
(define (merge-sched cached part)
  (reverse (append (reverse part) (reverse cached))))

(define (update-sched cached-sched)
  (define dates (parse-dates cached-sched))
  (define start-date (get-start-date dates))
  (define end-date todayf)
  ; remove the most recent day from the schedule, so it is replaced with the new data
  (define prep-sched (if (empty? cached-sched) empty (rest cached-sched)))
  (define schedule-params
    (~a "?startDate=" start-date "&" "endDate=" end-date "&expand=schedule.linescore"))
  (define part (nhl-get (string-append "schedule" schedule-params)))
  (define part-sched (hash-ref part 'dates empty))
  (merge-sched cached-sched part-sched))

(define schedule (update-sched cached-sched))
; (define schedule cached-sched)

; output the schedule to file
(define sched-file-out (open-output-file sched-file-name #:exists 'replace))
(write-json schedule sched-file-out)
(close-output-port sched-file-out)
