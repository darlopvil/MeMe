;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
;; Copyright (C) 2026 darlopvil
;;
;; This file is part of MeMe
;;
;; MeMe is free software: you can redistribute it and/or modify it under the
;; terms of the GNU Affero General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of  MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (meme pages)
  #:use-module (meme templates)
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (pages?
            error-page
            home-page
            editorials-page
            meme-page
            memes-page
            categories-page
            photos-page
            photo-page
            proxy-page))

(define (pages? components str)
  (or (equal? (car (last-pair components)) str)
      (and (equal? (list-ref components (- (length components) 2)) "page")
           (equal? (list-ref components (- (length components) 3)) str))))

(define user-agent-header
  `((user-agent . ,(parse-header 'user-agent "Mozilla/5.0"))))

;; Open the upstream connection ourselves so it is closed on every exit
;; path. http-request only closes the port it opened when keep-alive? is
;; false AND the response is read successfully; if read-response throws
;; (e.g. bad-response), the socket is left open until the GC finds it.
(define* (fetch url #:key (headers '()))
  (let ((port (open-socket-for-uri url)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (http-request url #:port port #:headers headers))
      (lambda ()
        (unless (port-closed? port)
          (close-port port))))))

(define good-response
  (build-response #:code 200
                  #:headers `((content-type . (text/html)))))

(define (redirect response code)
  (values (build-response #:code code
                          #:headers `((location . ,(parse-header
                                                     'location
                                                     (uri-path
                                                       (response-location
                                                         response))))))
          "Redirect"))

(define (error-page code)
  (values (build-response
            #:code code
            #:headers `((content-type . (text/html))))
          (shtml->html (error-template code))))

(define* (generic-page procedure path
                       #:optional (base-url "https://knowyourmeme.com"))
  (let ((resp "")
        (body ""))
    (receive (_resp _body)
             (catch 'bad-response
                    (lambda _ (fetch (string-append base-url path)
                                     #:headers user-agent-header))
                    (lambda _ (values #f #f)))
             (set! resp _resp)
             (set! body _body))
    (if (and (eq? resp #f) (eq? body #f))
      (begin
        (display ";\tbad-response thrown") (newline)
        (values
          (build-response #:code 500
                          #:headers `((content-type . (text/html))))
          (shtml->html (eof-template))))
      (begin
        (display (string-append "; \tStatus: "
                                (number->string (response-code resp))))
        (newline)
        (cond ((equal? (response-code resp) 200)
               (let ((doc (html->sxml-0nf body #:strict? #t)))
                 (values good-response
                         (string-replace-substring
                           (string-replace-substring
                             (shtml->html (procedure doc))
                             "https://knowyourmeme.com:443" "")
                           "https://knowyourmeme.com" ""))))
              ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
               (if (equal? (uri-path (response-location resp)) path)
                 (generic-page procedure path
                               "https://trending.knowyourmeme.com")
                 (redirect resp (response-code resp))))
              (else (error-page (response-code resp))))))))

(define (home-page)
  (display "; \tHandler: home-page; \tStatus: 200")
  (newline)
  (values good-response (shtml->html home-template)))

(define (editorials-page path)
  (display "; \tHandler: editorials-page")
  (generic-page editorials-template path))

(define (meme-page path)
  (display "; \tHandler: meme-page")
  (generic-page meme-template path))

(define (memes-page path)
  (display "; \tHandler: memes-page")
  (generic-page memes-template path))

(define (categories-page path)
  (display "; \tHandler: categories-page")
  (generic-page categories-template path))

(define (photos-page path)
  (display "; \tHandler: photos-page")
  (generic-page photos-template path))

(define (photo-page path)
  (display "; \tHandler: photo-page")
  (generic-page photo-template path))

(define (proxy-page query)
  (display "; \tHandler: proxy-page")
  (let ((resp "")
        (body "")
        (url ""))
    (map (lambda (p)
           (if (equal? (car p) "url")
             (set! url (uri-decode (cadr p)))))
         (map (lambda (s)
                (string-split s #\=))
              (string-split query #\&)))
    (if (or (string-prefix? "https://i.kym-cdn.com/" url)
            (string-prefix? "https://i.ytimg.com/" url)
            (string-prefix? "https://a.kym-cdn.com/" url)
            (string-prefix? "https://knowyourmeme.com/" url))
      (begin
        (receive (_resp _body) (fetch url)
                 (set! resp _resp)
                 (set! body _body))
        (display (string-append "; \tStatus: "
                                (number->string (response-code resp))))
        (newline)
        (cond
          ((equal? (response-code resp) 200)
           (values (build-response
                     #:code 200
                     #:headers `((content-type .
                                  ,(response-content-type resp))
                                 (cache-control .
                                  ,(parse-header 'cache-control
                                                 "max-age=604800"))))
                   body))
          ((and (>= (response-code resp) 300) (<= (response-code resp) 399))
           (values (build-response
                     #:code (response-code resp)
                     #:headers `((location . ,(parse-header
                                                'location
                                                (uri-path
                                                  (response-location
                                                    resp))))
                                 (content-type .
                                  ,(response-content-type resp))))
          "Redirect"))
          (else (error-page 404))))
      (if (equal? (string-ref url 0) #\/)
        (proxy-page (string-append "url=https://knowyourmeme.com" url))
        (error-page 400)))))
