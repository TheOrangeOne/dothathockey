#lang racket

(require json)
(require "sync.rkt")
(require "date.rkt")

(provide ratings
         team-ratings
         format-rating
         format-diff
         get-rating
         date-team-ratings
         daily-ratings
         get-next-game-day
         get-prev-game-day
         )

(define-struct team   (id score gp)   #:transparent)
(define-struct match  (home away res) #:transparent)
(define-struct rating (id rating gp diff)  #:transparent)

(define MAX-RATING 1600)
(define MIN-RATING 1400)

(define (normalize-rating rating)
  (/ (- rating MIN-RATING) (- MAX-RATING MIN-RATING)))

(define (format-rating rating)
  (real->decimal-string (* (normalize-rating rating) 100) 2))

(define (format-diff diff)
  (real->decimal-string (* diff 100) 2))

(define (get-next-game-day date [count (hash-count game-days)])
  (define next-date (date-next date))
  (cond [(= count 0) ""]
        [(hash-has-key? game-days next-date) next-date]
        [else (get-next-game-day next-date (sub1 count))]))

(define (get-prev-game-day date [count (hash-count game-days)])
  (define prev-date (date-prev date))
  (cond [(= count 0) ""]
        [(hash-has-key? game-days prev-date) prev-date]
        [else (get-prev-game-day prev-date (sub1 count))]))

(define (parse-team team-blob)
  (define score (hash-ref team-blob 'score))
  (define team (hash-ref team-blob 'team))
  (define id (hash-ref team 'id))
  (make-team id score 1))

(define (get-match game)
  (define linescore (hash-ref game 'linescore))
  (define status (hash-ref linescore 'currentPeriodTimeRemaining ""))
  (define nperiod (hash-ref linescore 'currentPeriod 0))
  ; if the game isn't over then set period to 0
  (define period (if (string=? status "Final") nperiod 0))
  (define teams (hash-ref game 'teams))
  (define home (parse-team (hash-ref teams 'home)))
  (define away (parse-team (hash-ref teams 'away)))
  (make-match home away period))

(define (get-matches date)
  (define games (hash-ref date 'games))
  (map get-match games))

(define (get-date-matches date)
  (define sdate (hash-ref date 'date))
  (define games (hash-ref date 'games))
  (list sdate (map get-match games)))

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
  (cond [(= res 5)
         (values 0.5 0.5)]
        [(and (= res 4) (> home-score away-score))
         (values 0.6 0.4)]
        [(and (= res 4) (< home-score away-score))
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
  (define diff (- (normalize-rating newrating) (normalize-rating rating)))
  (make-rating id newrating (add1 gp) diff))

(define IR 1500)
(define GP 0)
(define (gen-rating match ratings)
  (if (= (match-res match) 0)
    ratings
    (let*-values
          ([(home) (match-home match)]
           [(home-id) (id->js (team-id home))]
           [(away) (match-away match)]
           [(away-id) (id->js (team-id away))]
           [(hr) (hash-ref ratings home-id (make-rating home-id IR GP IR))]
           [(ar) (hash-ref ratings away-id (make-rating away-id IR GP IR))]
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

(define (remove-diffs ratings [keys (hash-keys ratings)])
  (if (empty? keys)
    (make-immutable-hash)
    (let* ((rrating (hash-ref ratings (first keys)))
           (rating (rating-rating rrating))
           (id (rating-id rrating))
           (gp (rating-gp rrating)))
      (hash-set (remove-diffs ratings (rest keys)) (first keys) (make-rating id rating gp 0)))))

(define (gen-daily-rating date-match daily-ratings)
  (define date (first date-match))
  (define prev-date (get-prev-game-day date))
  (define matches (second date-match))
  (define prev-ratings (hash-ref daily-ratings prev-date (make-immutable-hash)))
  (define rm-diff-ratings (remove-diffs prev-ratings))
  (define ratings (gen-ratings matches rm-diff-ratings))
  (hash-set daily-ratings date ratings))

(define (gen-daily-ratings date-matches [daily-ratings (make-immutable-hash)])
  (if (empty? date-matches)
    daily-ratings
    (let ((newratings (gen-daily-rating (first date-matches) daily-ratings)))
      (gen-daily-ratings (rest date-matches) newratings))))

(define matches (flatten (map get-matches schedule)))
(define (valid-match? match)
  (define home-id (team-id (match-home match)))
  (define away-id (team-id (match-away match)))
  (and (teams-has-id? (string->symbol (number->string home-id)))
       (teams-has-id? (string->symbol (number->string away-id)))))
(define valid-matches (filter valid-match? matches))
(define ratings (gen-ratings valid-matches))

(define date-matches (map get-date-matches schedule))
(define daily-ratings (gen-daily-ratings date-matches))

(define (get-rating id)
  (format-rating (rating-rating (hash-ref ratings id (make-rating 0 IR 0 IR)))))

(define (team-rating ratings id)
  (define rating (hash-ref ratings id (make-rating 0 IR 0 IR)))
  (define r (rating-rating rating))
  (define team (hash-ref teams id))
  (define abbr (hash-ref team 'abbreviation))
  (define src (get-img-fn id))
  (define diff (rating-diff rating))
  (list abbr r src diff))

(define (gen-team-ratings ratings teams)
  (sort (map (curry team-rating ratings) (hash-keys teams)) > #:key (lambda (x) (second x))))

(define (filter-teams keys teams)
  (cond [(empty? keys) (make-immutable-hash)]
        [(hash-has-key? teams (first keys))
         (hash-set (filter-teams (rest keys) teams) (first keys) (hash-ref teams (first keys)))]
        [else (filter-teams (rest keys) teams)]))

(define (gen-date-team-ratings date)
  (define ratings (hash-ref daily-ratings date))
  (list date (gen-team-ratings ratings teams)))

(define team-ratings (gen-team-ratings ratings teams))

(define date-team-ratings (map gen-date-team-ratings (hash-keys daily-ratings)))
