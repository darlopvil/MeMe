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

;; A small thread-per-connection HTTP server.
;;
;; (web server)'s run-server handles one request at a time, so a single
;; slow upstream call blocks every other client and a gallery's images
;; are proxied strictly in series. Guile has real POSIX threads, so each
;; connection is served on its own thread instead, using the same public
;; building blocks run-server uses (read-request, sanitize-response,
;; write-response).
;;
;; Each request also gets its own output port: whatever the handler logs
;; with display is collected and written to stdout as one block when the
;; request ends, so concurrent requests never interleave their log lines
;; and every line is flushed immediately.

(define-module (meme server)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 receive)
  #:export (run-threaded-server))

(define log-port (current-output-port))
(define log-mutex (make-mutex))

(define (log-text text)
  (unless (string-null? text)
    (with-mutex log-mutex
      (display text log-port)
      (unless (string-suffix? "\n" text) (newline log-port))
      (force-output log-port))))

;; Cap on connections served at once. When it is reached, the accept loop
;; waits for a free slot and new clients queue in the listen backlog.
(define max-workers 32)
(define active 0)
(define slots-mutex (make-mutex))
(define slots-free (make-condition-variable))

(define (acquire-slot!)
  (with-mutex slots-mutex
    (let wait ()
      (when (>= active max-workers)
        (wait-condition-variable slots-free slots-mutex)
        (wait)))
    (set! active (1+ active))))

(define (release-slot!)
  (with-mutex slots-mutex
    (set! active (1- active))
    (signal-condition-variable slots-free)))

;; One request per connection: every response says so and the socket is
;; closed afterwards, which keeps the server simple behind a reverse proxy.
(define (with-connection-close response)
  (build-response #:version (response-version response)
                  #:code (response-code response)
                  #:reason-phrase (response-reason-phrase response)
                  #:headers (cons '(connection close)
                                  (response-headers response))))

(define (write-reply client request response body)
  (receive (response body) (sanitize-response request response body)
    (write-response (with-connection-close response) client)
    (when body (put-bytevector client body))
    (force-output client)))

;; Run the handler on its own thread so a stuck upstream request can be
;; answered with ON-TIMEOUT after TIMEOUT seconds. The abandoned thread
;; finishes (or not) on its own; its output is no longer read.
(define (call-handler handler request body out timeout)
  (let ((worker (call-with-new-thread
                  (lambda ()
                    (parameterize ((current-output-port out))
                      (call-with-values
                        (lambda () (handler request body))
                        list))))))
    (join-thread worker (+ (current-time) timeout) #f)))

(define (serve-client client handler timeout on-timeout)
  (let ((out (open-output-string)))
    (catch #t
      (lambda ()
        (setvbuf client 'block)
        (let ((request (catch #t
                         (lambda () (read-request client))
                         (lambda _ #f))))
          (if (not request)
            ;; Malformed request line or headers.
            (begin
              (write-response
                (build-response #:code 400 #:headers '((connection close)))
                client)
              (force-output client))
            (let* ((body (read-request-body request))
                   (result (call-handler handler request body out timeout)))
              (if result
                (write-reply client request (car result) (cadr result))
                (begin
                  (display "; \tTimed out" out)
                  (newline out)
                  (call-with-values on-timeout
                    (lambda (response body)
                      (write-reply client request response body)))))))))
      (lambda (key . args)
        ;; Usually the client went away mid-response (broken pipe).
        (display (format #f "; \tConnection error: ~a" key) out)
        (newline out)))
    (catch #t (lambda () (close-port client)) (lambda _ #f))
    (log-text (get-output-string out))))

(define* (run-threaded-server handler sock
                              #:key (timeout 60) on-timeout (backlog 128))
  (listen sock backlog)
  (let loop ()
    (let ((client (car (accept sock))))
      (acquire-slot!)
      (call-with-new-thread
        (lambda ()
          (dynamic-wind
            (const #t)
            (lambda () (serve-client client handler timeout on-timeout))
            release-slot!))))
    (loop)))
