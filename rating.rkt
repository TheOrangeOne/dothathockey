#lang racket

(require json)
(require "sync.rkt")

(provide ratings team-ratings format-rating)

(define-struct team   (id score gp)   #:transparent)
(define-struct match  (home away res) #:transparent)
(define-struct rating (id rating gp)  #:transparent)

(define (format-rating rating)
  (real->decimal-string (/ rating 100) 3))

(define (parse-team team-blob)
  (define score (hash-ref team-blob 'score))
  (define team (hash-ref team-blob 'team))
  (define id (hash-ref team 'id))
  (make-team id score 1))

(define (get-match game)
  (define linescore (hash-ref game 'linescore))
  (define period (hash-ref linescore 'currentPeriodOrdinal "-"))
  (define teams (hash-ref game 'teams))
  (define home (parse-team (hash-ref teams 'home)))
  (define away (parse-team (hash-ref teams 'away)))
  (make-match home away period))

(define (get-matches date)
  (define games (hash-ref date 'games))
  (map get-match games))

(define HF 0)
(define SP 400)
(define (expected-scores a b)
  (define ra (rating-rating a))
  (define rb (rating-rating b))
  (define ea (/ 1 (+ 1 (expt 10 (/ (- rb (+ ra HF)) SP)))))
  (define eb (- 1 ea))
  (values ea eb))

(define (actual-scores match)
  (define res (match-res match))
  (define home-score (team-score (match-home match)))
  (define away-score (team-score (match-away match)))
  (cond [(string=? res "SO")
         (values 0.5 0.5)]
        [(and (string=? res "OT") (> home-score away-score))
         (values 0.6 0.4)]
        [(and (string=? res "OT") (< home-score away-score))
         (values 0.4 0.6)]
        [(> home-score away-score)
         (values 1 0)]
        [(< home-score away-score)
         (values 0 1)]))

(define (new-rating old s e)
  (define gp (rating-gp old))
  (define id (rating-id old))
  (define rating (rating-rating old))
  (define K (if (< gp 10) 15 5))
  (define newrating (+ rating (* K (- s e))))
  (make-rating id newrating (add1 gp)))

(define IR 1500)
(define GP 0)
(define (gen-rating match ratings)
  (if (string=? (match-res match) "-")
    ratings
    (let*-values
          ([(home) (match-home match)]
           [(home-id) (id->js (team-id home))]
           [(away) (match-away match)]
           [(away-id) (id->js (team-id away))]
           [(hr) (hash-ref ratings home-id (make-rating home-id IR GP))]
           [(ar) (hash-ref ratings away-id (make-rating away-id IR GP))]
           [(ea eb) (expected-scores hr ar)]
           [(sa sb) (actual-scores match)])
          (hash-set
            (hash-set ratings home-id (new-rating hr sa ea))
            away-id (new-rating ar sb eb)))))

(define (gen-ratings matches [ratings (make-immutable-hash)])
  (if (empty? matches)
    ratings
    (let ((newratings (gen-rating (first matches) ratings)))
      (gen-ratings (rest matches) newratings))))


(define matches (flatten (map get-matches schedule)))
(define ratings (gen-ratings matches))

(define (team-rating id)
  (define rating (rating-rating (hash-ref ratings id)))
  (define team (hash-ref teams id))
  (define abbr (hash-ref team 'abbreviation))
  (define src (get-img-fn id))
  (list abbr rating src))


(define team-ratings
  (sort (map team-rating (hash-keys teams)) > #:key (lambda (x) (second x))))

