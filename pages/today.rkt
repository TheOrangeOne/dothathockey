#lang racket

(require scribble/html/html)
(require "../page.rkt")
(require "../data.rkt")

(provide today-page)


(define (get-game game)
  (define teams (hash-ref game 'teams))
  (define home (hash-ref teams 'home))
  (define home-team (hash-ref home 'team))
  (define away (hash-ref teams 'away))
  (define away-team (hash-ref away 'team))
  (list home-team away-team))

(define games (map get-game today-games))


(define (html-game game)
  (define home-team (first game))
  (define away-team (second game))
  (li (~a (hash-ref home-team 'name) " vs " (hash-ref away-team 'name))))

(define (html-games games) (ul (map html-game games)))

(define (is-good-game? game)
  (define homeid (hash-ref (first game) 'id))
  (define awayid (hash-ref (second game) 'id))
  (and (is-top-team? homeid) (is-top-team? awayid)))

(define good-games (filter is-good-game? games))


(define fn "build/today.html")
(define src (body (h1 "Today's games:")
                  (html-games games)
                  (h1 "Good games:")
                  (html-games good-games)))

(define today-page (make-page src fn))
