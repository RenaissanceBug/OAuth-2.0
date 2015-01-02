#lang racket
(require racket/serialize)
(provide make-oauth-2 make-oauth-with-grant-type
         oauth?
         oauth-client-id oauth-client-secret 
         oauth-auth-uri oauth-token-uri oauth-redirect-uri
         oauth-grant-type
         oauth-response-type)

;;An OAuth-Obj is an oauth struct:
(serializable-struct oauth
  [client-id client-secret auth-uri token-uri redirect-uri grant-type])
;; client-id : string
;;    the Racket app's client ID
;; client-secret : string
;;    the Racket app's client secret
;; auth-uri : string
;;    URI on the resource server to which the Racket code is asking
;;    the resource owner (typically, browser user) to grant access;
;;    via this URI, the user obtains an authorization token.
;;    Example: https://accounts.google.com/o/oauth2/auth
;;      is Google's URL for obtaining the resource owner's permission
;; token-uri : string
;;    URI on the resource server from which Racket can obtain the access token
;;    after the user has granted permission and obtained an authorization token.
;;redirect-uri : string
;;    representation of a url
;;response-type is a string 

;;By-keyword constructor for oauth structure:
(define (make-oauth-2 #:client-id client-id
                      #:client-secret client-secret
                      #:authorization-uri authorization-uri
                      #:token-uri token-uri
                      #:redirect-uri redirect-uri
                      #:grant-type (grant-type 'authorization-code))
  
  (define check-grant-type
    (and (memq grant-type
               '(authorization-code token password client-cred refresh-token))
         #t))
  
  (if check-grant-type 
      (oauth client-id client-secret
             authorization-uri token-uri
             redirect-uri
             grant-type)
      (error 'make-oauth-2 "invalid grant type ~a" grant-type)))



(define (oauth-object? obj) (oauth? obj))


(define (make-oauth-with-grant-type oauth-obj grant-type)
  (make-oauth-2 #:client-id (oauth-client-id oauth-obj)
                #:client-secret (oauth-client-secret oauth-obj)
                #:authorization-uri (oauth-auth-uri oauth-obj)
                #:token-uri (oauth-token-uri oauth-obj)
                #:redirect-uri (oauth-redirect-uri oauth-obj)
                #:grant-type grant-type)) 



(define (oauth-response-type oauth-obj)
  (let ([grant-type (oauth-grant-type oauth-obj)])
    (case grant-type
      [(authorization-code) "code"]
      [(token) "token"]
      [else #f])))

#;
(begin
  (define oauth-obj
    (make-oauth-2
     #:client-id "abc .... blah"
     #:client-secret "45bg......"
     #:authorization-uri "https://accounts.google.com/o/oauth2/auth"
     #:token-uri "https://accounts.google.com/o/oauth2/token"
     #:redirect-uri "http://localhost:8000/oauth2callback.rkt"
     #:grant-type 'authorization-code))
  (list (oauth-auth-uri oauth-obj)
        (oauth-token-uri oauth-obj)
        (oauth-client-id oauth-obj)
        (oauth-client-secret oauth-obj)
        (oauth-redirect-uri oauth-obj)
        (get-response-type oauth-obj)))