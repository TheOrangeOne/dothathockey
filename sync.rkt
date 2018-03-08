#lang racket

(require json)
(require racket/hash)
(require "api.rkt")
(require "date.rkt")

(provide schedule game-days teams id->js id->string get-img-fn)

(define DATA-DIR "data/")
(define BUILD-DIR "build/")
(define SEASON-START "2017-10-04")

(define (id->js id)
  (cond [(symbol? id) id]
        [(number? id) (string->symbol (number->string id))]
        [(string? id) (string->symbol id)]))

(define (id->string id)
  (cond [(symbol? id) (symbol->string id)]
        [(number? id) (number->string id)]
        [(string? id) id]))

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

(define teams-file-name (~a DATA-DIR "teams.json"))
(close-output-port (open-output-file teams-file-name #:exists 'append))
(define teams-file-in (open-input-file teams-file-name))
(define teams-cached (read-json teams-file-in))
(close-input-port teams-file-in)
(define teams (if (eof-object? teams-cached) (get-teams) teams-cached))
(define teams-file-out (open-output-file teams-file-name #:exists 'replace))
(write-json teams teams-file-out)
(close-output-port teams-file-out)

(define sched-file-name (~a DATA-DIR "sched.json"))
(close-output-port (open-output-file sched-file-name #:exists 'append))
(define sched-file-in (open-input-file sched-file-name))
(define sched-raw (read-json sched-file-in))
(define cached-sched (if (eof-object? sched-raw) empty sched-raw))
(close-input-port sched-file-in)

(define (parse-dates sched)
  (if (empty? sched)
    empty
    (cons (hash-ref (first sched) 'date) (parse-dates (rest sched)))))

(define (parse-dates-hash sched)
  (if (empty? sched)
    (make-immutable-hash)
    (hash-set (parse-dates-hash (rest sched)) (hash-ref (first sched) 'date) "")))

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

(define game-days (parse-dates-hash schedule))

; output the schedule to file
(define sched-file-out (open-output-file sched-file-name #:exists 'replace))
(write-json schedule sched-file-out)
(close-output-port sched-file-out)

(define (get-img-fn id)
  (~a id ".svg"))

(define (get-img-data-path id)
  (~a DATA-DIR (get-img-fn id)))

(define (get-img-build-path id)
  (~a BUILD-DIR (get-img-fn id)))

(define (restore-img id)
  (define img-file-data
    (open-input-file (get-img-data-path id)))
  (define svg (port->string img-file-data))
  (define img-file-build
    (open-output-file (get-img-build-path id) #:exists 'replace))
  (write-string svg img-file-build)
  (close-input-port img-file-data)
  (close-output-port img-file-build)
  void)

(define (fetch-img id)
  (define svg (logo-get id))
  (define img-file-data
    (open-output-file (get-img-data-path id) #:exists 'replace))
  (define img-file-build
    (open-output-file (get-img-build-path id) #:exists 'replace))
  (write-string svg img-file-data)
  (write-string svg img-file-build)
  (close-output-port img-file-data)
  (close-output-port img-file-build)
  void)

(define (get-img id)
  (define img-file-name (get-img-data-path id))
  (if (file-exists? img-file-name)
    (restore-img id)
    (fetch-img id)))

(define (get-imgs ids) (map get-img ids))

(define imgs (get-imgs (map id->string (hash-keys teams))))

