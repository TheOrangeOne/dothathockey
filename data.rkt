#lang racket

(require "api.rkt")
(require json)

(provide today-games
         is-top-team?
         )

(define date (seconds->date (current-seconds)))
(define todayf (~a (date-year date) "-" (date-month date) "-" (date-day date)))
(define schedule-params (~a "?startDate=" todayf "&" "endDate=" todayf))

(define sched-blob (nhl-get (string-append "schedule" schedule-params)))
(define today-sched (first (hash-ref sched-blob 'dates)))
(define num-games (hash-ref today-sched 'totalGames))
(define today-games (hash-ref today-sched 'games))


(define team-blob (nhl-get "teams"))
(define teams (hash-ref team-blob 'teams))
(define teamids (map (lambda (team) (hash-ref team 'id)) teams))


(define (collect-team-data id)
  (println (~a "fetching data for team " id))
  (define filename (~a "data/" id ".json"))
  (define file (open-output-file filename #:exists 'replace))
  (define endpoint (~a "teams/" id "/stats"))
  (define raw-blob (jsexpr->string (nhl-get endpoint)))
  (write raw-blob file)
  (close-output-port file))

; uncomment to recollect data
; (map collect-team-data teamids)

(define (cached-team-data id)
  (define filename (~a "data/" id ".json"))
  (define file (open-input-file filename))
  (define raw-team-data (port->string file))
  (close-input-port file)
  (string->jsexpr raw-team-data))

(define (cached-teams-data ids [mappedids (make-immutable-hash)])
  (if (empty? ids)
    mappedids
    (cached-teams-data
      (rest ids)
      (hash-set mappedids
                (first ids)
                (cached-team-data (first ids))))))

(define team-data (cached-teams-data teamids))


(define (strip-chars str chars)
  (list->string (remove* (string->list chars) (string->list str))))

(define (is-top-N-team? N id data)
  (define stats (second (hash-ref (string->jsexpr data) 'stats)))
  (define split (first (hash-ref stats 'splits)))
  (define stat (hash-ref split 'stat))
  (define pct (string->number (strip-chars (hash-ref stat 'ptPctg) "strdnh")))
  (<= pct N))

(define (is-top-10-team? id data)
  (is-top-N-team? 10 id data))

(define top-teams
  (hash-map team-data
            (lambda (k v) (if (is-top-10-team? k v) k #f))))

(define (is-top-team? id) (member id top-teams))
; (displayln top-teams)
