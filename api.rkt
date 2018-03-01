#lang racket

(require net/http-client)
(require net/head)
(require net/url)
(require json)

(provide nhl-get)

(define api-base "https://statsapi.web.nhl.com/api/v1/")

(define (nhl-get endpoint)
  (displayln (~a "GET" endpoint))
  (define-values (status header resp)
    (http-sendrecv/url (string->url (string-append api-base endpoint))
                       #:method "GET"
                       #:data void))
  (read-json resp))
