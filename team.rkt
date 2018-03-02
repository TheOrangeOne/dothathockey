#lang racket

(require "sync.rkt")

(provide get-abbr get-img game->ids)

(define (zip l1 l2) (map list l1 l2))

(define (get-team-id team)
  (id->js (hash-ref team 'id)))

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

(define (get-img id)
  (define sid (id->string id))
  (get-img-fn sid))

(define (game->ids game)
  (define teams (get-teams game))
  (map get-team-id teams))
