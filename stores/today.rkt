#lang racket

(require json)
(require "../sync.rkt")
(require "../rating.rkt")
(require "../team.rkt")

(provide games close-games)

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

(define (get-display game)
  (define match (game->ids game))
  (define ids (flatten match))
  (define dids
    (map (lambda (id)
           (list (get-abbr id)
                 (get-rating id)
                 (get-img id))) ids))
  (list (first dids) (second dids)))

(define (is-close-game? game)
  (define home-rating (string->number (second (first game))))
  (define away-rating (string->number (second (second game))))
  (< (abs (- home-rating away-rating)) 10))


(define games (map get-display today-games))
(define close-games (filter is-close-game? games))
