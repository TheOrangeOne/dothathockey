#lang racket

(require scribble/html/html)
(require "../page.rkt")
(require "../data.rkt")

(provide today-page)

(define fn "today.html")

(define (html-game game)
  (define teams (hash-ref game 'teams))
  (define home (hash-ref teams 'home))
  (define home-team (hash-ref home 'team))
  (define away (hash-ref teams 'away))
  (define away-team (hash-ref away 'team))
  (li (~a (hash-ref home-team 'name) " vs " (hash-ref away-team 'name))))

(define html-games (ul (map html-game today-games)))

(define src (html
              (head (title "Today's games"))
              (body (h1 "Today's games:")
                    html-games)))

(define today-page (make-page src fn))
