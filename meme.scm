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
  #:use-module (meme server)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web http)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports))

;; Last line of defence: anything the routes below throw becomes a
;; readable 500 page and one log line instead of a bare 500 + backtrace.
(define (handler request request-body)
  (catch #t
    (lambda () (dispatch request request-body))
    (lambda (key . args)
      (pass-signals key args)
      (display (format #f " handler-error=~a ~s" key args))
      (error-page 500 "Something went wrong while handling this request."))))

(define (dispatch request request-body)
  (let* ((uri (request-uri request))
         (path (uri-path uri))
         (path-components (split-and-decode-uri-path path))
         (uristring (uri->string uri)))

    (display (string-append
               (strftime "%F %T" (localtime (current-time)))
               " " (uri->string uri)))
    (if (and (not (equal? path-components '()))
             (equal? (car path-components) "sensitive"))
      (set! path-components (cdr path-components)))
    (cond
      ((equal? path "/style.css")
       (display " handler=internal status=200")
       (values `((content-type . (text/css)))
               (call-with-input-file "static/style.css"
                                     get-string-all)))
      ((equal? path "/favicon.png")
       (display " handler=internal status=200")
       (values `((content-type . (image/png))
                 (cache-control .
                                ,(parse-header 'cache-control
                                               "max-age=604800")))
               (call-with-input-file "static/logo.png"
                                     get-bytevector-all)))
      ((equal? path "/proxy")
       (proxy-page (uri-query uri)))
      ((or (equal? path "/") (null? path-components))
       (home-page))
      ;; Random entry / image / video: KYM answers with a redirect to a
      ;; random page, which generic-page relays.
      ((and (= (length path-components) 2)
            (equal? (cadr path-components) "random")
            (member (car path-components) '("memes" "photos" "videos")))
       (cond ((equal? (car path-components) "memes") (meme-page uristring))
             ((equal? (car path-components) "photos") (photo-page uristring))
             (else (video-page uristring))))
      ;; User profiles. KYM redirects short names (/users/don) to the
      ;; canonical one, and generic-page relays that redirect.
      ((and (equal? (car path-components) "users")
            (or (= (length path-components) 2)
                (and (= (length path-components) 3)
                     (equal? (caddr path-components) "wall"))))
       (profile-page uristring))
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
      ;; .../photos/sort/<order>: KYM's own menus link here and answer
      ;; with a redirect to .../photos?sort=<order>, which generic-page
      ;; relays with the query intact.
      ((and (>= (length path-components) 3)
            (equal? (list-ref path-components
                              (- (length path-components) 3))
                    "photos")
            (equal? (list-ref path-components
                              (- (length path-components) 2))
                    "sort"))
       (photos-page uristring))
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
      ;; A single video: /videos/<id>-<slug>
      ((and (equal? (car path-components) "videos")
            (= (length path-components) 2)
            (char-numeric? (string-ref (cadr path-components) 0)))
       (video-page uristring))
      ;; Video listings: /videos, /videos/trending, /memes/<slug>/videos,
      ;; with their /page/N and tab sub-paths
      ((member "videos" path-components)
       (videos-page uristring))
      ;; KYM's own episodes use the editorial list layout
      ((equal? (car path-components) "episodes")
       (editorials-page uristring))
      ((and (equal? (car path-components) "memes")
            (or
              (equal? (length path-components) 1)
              (pages? path-components "memes")
              (pages? path-components "children")
              (pages? path-components "submissions")
              (pages? path-components "confirmed")
              (pages? path-components "newsworthy")
              (pages? path-components "popular")
              (pages? path-components "deadpool")
              (pages? path-components "all")))
       (memes-page uristring))
      ;; News: listings share the editorial list layout, articles the
      ;; editorial article layout.
      ((and (equal? (car path-components) "news")
            (or (= (length path-components) 1)
                (pages? path-components "news")))
       (editorials-page uristring))
      ((equal? (car path-components) "news")
       (article-page uristring))
      ;; Editorial articles: /editorials/<section>/<slug>. A bare
      ;; /editorials/<slug> is redirected there by KYM.
      ((equal? (car path-components) "editorials")
       (article-page uristring))
      ((equal? (car path-components) "memes")
       (meme-page uristring))
      ((equal? (car path-components) "search")
       (search-page uristring))
      ((and (equal? (car path-components) "categories")
            (equal? (length path-components) 1))
       (categories-page uristring))
      ((or (equal? (car path-components) "categories")
           (equal? (car path-components) "types"))
       (memes-page uristring))
      (else (display " status=501") (error-page 501)))))


;; Requests are served concurrently by (meme server), one thread per
;; connection. A request still running after 60 seconds (usually KYM not
;; answering) gets a 504 page. SIGINT terminates the process right away.
(let ((port (if (getenv "PORT")
              (string->number (getenv "PORT"))
              8003))
      (sock (socket PF_INET SOCK_STREAM 0)))
  (setsockopt sock SOL_SOCKET SO_REUSEPORT 1)
  (bind sock AF_INET INADDR_ANY port)
  (run-threaded-server
    handler sock
    #:timeout 60
    #:on-timeout (lambda ()
                   (error-page 504 "Know Your Meme took too long to answer."))))
