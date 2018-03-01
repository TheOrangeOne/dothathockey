#lang racket

(require "sync.rkt")

(provide get-display)

(define (zip l1 l2) (map list l1 l2))

(define (get-team-id team)
  (hash-ref team 'id))

(define (get-teams game)
  (define teams (hash-ref game 'teams))
  (define home (hash-ref teams 'home))
  (define home-team (hash-ref home 'team))
  (define away (hash-ref teams 'away))
  (define away-team (hash-ref away 'team))
  (list home-team away-team))

(define (get-abbr id)
  (define team (hash-ref teams (id->js id)))
  (hash-ref team 'abbreviation))

(define (get-abbrs game)
  (map get-abbr (map get-team-id (get-teams game))))

(define (get-img id)
  (define sid (id->string id))
  (get-img-fn sid))

(define (get-imgs game)
  (map get-img (map get-team-id (get-teams game))))

(define (get-display game)
  (define abbrs (get-abbrs game))
  (define imgs (get-imgs game))
  (zip abbrs imgs))
