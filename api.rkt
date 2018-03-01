#lang racket

(require net/http-client)
(require net/head)
(require net/url)
(require json)

(provide nhl-get logo-get)

(define api-base "https://statsapi.web.nhl.com/api/v1/")
(define img-api-base "https://www-league.nhlstatic.com/builds/site-core/86d4b76cc03a4d111ee0e20f9f62eb054eef3b74_1502985652/images/logos/team/current/team-~s-dark.svg")

(define (logo-get id)
  (define sendpoint (format img-api-base id))
  (displayln (~a "GET " sendpoint))
  (define endpoint (string->url sendpoint))
  (define-values (status header resp)
    (http-sendrecv/url endpoint #:method "GET" #:data void))
  (port->string resp))

(define (nhl-get endpoint)
  (displayln (~a "GET " endpoint))
  (define-values (status header resp)
    (http-sendrecv/url (string->url (string-append api-base endpoint))
                       #:method "GET"
                       #:data void))
  (read-json resp))
