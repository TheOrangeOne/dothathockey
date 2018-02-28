#lang racket

(require json)
(require "../sync.rkt")

(provide games)

(define today-sched (last schedule))
(define today-num-games (hash-ref today-sched 'totalGames))
(define today-games (hash-ref today-sched 'games))

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

; (define top-teams
;   (hash-map team-data
;             (lambda (k v) (if (is-top-10-team? k v) k #f))))

; (define (is-top-team? id) (member id top-teams))

(define (get-game game)
  (define teams (hash-ref game 'teams))
  (define home (hash-ref teams 'home))
  (define home-team (hash-ref home 'team))
  (define away (hash-ref teams 'away))
  (define away-team (hash-ref away 'team))
  (list home-team away-team))

(define games (map get-game today-games))
