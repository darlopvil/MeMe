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

(add-to-load-path (dirname (current-filename)))

(define-module (meme)
  #:use-module (meme pages)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web http)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports))

(define (handler request request-body)
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (path-components (split-and-decode-uri-path path))
         (uristring (uri->string uri)))

    (display (string-append
               (strftime "%c" (localtime (current-time)))
               ": " (uri->string uri)))
    (if (and (not (equal? path-components '()))
             (equal? (car path-components) "sensitive"))
      (set! path-components (cdr path-components)))
    (cond
      ((equal? path "/style.css")
       (display ";\tHandler: internal") (newline)
       (values `((content-type . (text/css)))
               (call-with-input-file "static/style.css"
                                     get-string-all)))
      ((equal? path "/favicon.png")
       (display ";\tHandler: internal") (newline)
       (values `((content-type . (image/png))
                 (cache-control .
                                ,(parse-header 'cache-control
                                               "max-age=604800")))
               (call-with-input-file "static/logo.png"
                                     get-bytevector-all)))
      ((equal? path "/proxy")
       (proxy-page (uri-query uri)))
      ((equal? path "/")
       (home-page))
      ((and
         (equal? (car path-components) "editorials")
         (or
           (equal? (length path-components) 1)
           (pages? path-components "editorials")
           (pages? path-components "interviews")
           (pages? path-components "in-the-media")
           (pages? path-components "white-papers")
           (pages? path-components "episode-notes")
           (pages? path-components "behind-the-scenes")
           (pages? path-components "meme-review")
           (pages? path-components "collections")
           (pages? path-components "poll")
           (pages? path-components "guides")
           (pages? path-components "meme-insider")
           (pages? path-components "insights")))
       (editorials-page uristring))
      ((or
         (equal? (car (last-pair path-components)) "photos")
         (and (or
                (and
                  (>= (length path-components) 2)
                  (equal? (list-ref path-components
                                    (- (length path-components) 2))
                          "photos")
                  (not (> (string-count
                            (car (last-pair path-components))
                            char-set:digit 0 1) 0)))
                (and
                  (>= (length path-components) 3)
                  (equal? (list-ref path-components
                                    (- (length path-components) 3))
                          "photos"))
                (and
                  (>= (length path-components) 4)
                  (equal? (list-ref path-components
                                    (- (length path-components) 4))
                          "photos")))
              (or
                (pages? path-components "photos")
                (pages? path-components "trending")
                (pages? path-components "most-commented")
                (pages? path-components "most-favorited")
                (pages? path-components "most-liked")
                (pages? path-components "least-liked")
                (pages? path-components "most-viewed")
                (pages? path-components "templates"))))
       (photos-page uristring))
      ((and (>= (length path-components) 2)
            (equal? (list-ref
                      path-components
                      (- (length path-components) 2))
                    "photos")
            (> (string-count
                 (car (last-pair path-components))
                 char-set:digit 0 1)
               0))
       (photo-page uristring))
      ((and (equal? (car path-components) "memes")
            (or
              (equal? (length path-components) 1)
              (pages? path-components "memes")
              (pages? path-components "children")
              (pages? path-components "submissions")
              (pages? path-components "researching")
              (pages? path-components "newsworthy")
              (pages? path-components "popular")
              (pages? path-components "deadpool")
              (pages? path-components "all")))
       (memes-page uristring))
      ;; Editorial articles: /editorials/<section>/<slug>. A bare
      ;; /editorials/<slug> is redirected there by KYM.
      ((equal? (car path-components) "editorials")
       (article-page uristring))
      ((equal? (car path-components) "memes")
       (meme-page uristring))
      ((and (equal? (car path-components) "categories")
            (equal? (length path-components) 1))
       (categories-page uristring))
      ((or (equal? (car path-components) "categories")
           (equal? (car path-components) "types"))
       (memes-page uristring))
      (else (newline) (error-page 501)))))


;; this is taken from web/server.scm because it isn't exported
(define call-with-sigint
  (if (not (provided? 'posix))
    (lambda (thunk handler-thunk) (thunk))
    (lambda (thunk handler-thunk)
      (let ((handler #f))
        (catch 'interrupt
          (lambda ()
            (dynamic-wind
              (lambda ()
                (set! handler
                  (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
              thunk
              (lambda ()
                (if handler
                  ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                  (sigaction SIGINT (car handler) (cdr handler))
                  ;; restore original C handler.
                  (sigaction SIGINT #f)))))
          (lambda (k . _) (handler-thunk)))))))

(let ((port (if (getenv "PORT")
              (string->number (getenv "PORT"))
              8003))
      (sock (socket PF_INET SOCK_STREAM 0)))
  (setsockopt sock SOL_SOCKET SO_REUSEPORT 1)
  (bind sock AF_INET INADDR_ANY port)
  (fcntl sock F_SETFL (logior O_NONBLOCK
                              (fcntl sock F_GETFL)))
  (run-server handler 'http `(#:socket ,sock)))
