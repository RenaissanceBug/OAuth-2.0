#lang racket
;(require (planet vyzo/crypto:2:3))
(require net/url net/uri-codec net/base64)
(require web-server/stuffers web-server/stuffers/hmac-sha1)

(define HMAC-SHA1-blocksize 64) ;; bytes, per the HMAC-SHA1 standard spec

(define key (apply bytes (for/list ([i 20]) (random 256))))

(define stuff (stuffer-chain (HMAC-SHA1-stuffer key) base64-stuffer))

;; Bytes -> Bytes
(define (encode-state uri)
  (define ubytes (string->bytes/utf-8 uri))
  ((stuffer-in stuff) ubytes))

;; Bytes -> Bytes
(define (decode-state state)
  ((stuffer-out stuff) state))
;; errors if signature doesn't match or state is invalid


(provide decode-state encode-state)

(module+ test
  (require rackunit)
  (check-equal? (decode-state (encode-state "http://foo.com/xyz"))
                "http://foo.com/xyz"))
