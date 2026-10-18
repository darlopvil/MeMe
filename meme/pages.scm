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
  #:use-module ((meme scraping) #:select (proxyable? https-upgrade))
  #:use-module (htmlprag)
  #:use-module (web response)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (pages?
            pass-signals
            error-page
            home-page
            editorials-page
            meme-page
            memes-page
            categories-page
            photos-page
            photo-page
            article-page
            search-page
            videos-page
            video-page
            proxy-page))

(define (pages? components str)
  (let ((n (length components)))
    (and (> n 0)
         (or (equal? (list-ref components (- n 1)) str)
             (and (>= n 3)
                  (equal? (list-ref components (- n 2)) "page")
                  (equal? (list-ref components (- n 3)) str))))))

;; Our catch-all handlers must let these through, exactly like Guile's
;; own server does: 'interrupt is how SIGINT (docker stop) ends
;; run-server, and swallowing it would leave the container hanging until
;; it gets SIGKILLed.
(define (pass-signals key args)
  (when (memq key '(quit interrupt))
    (apply throw key args)))

(define (log-error what key args)
  (display (format #f "; \t~a: ~a ~s" what key args))
  (newline))

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

;; Local equivalent (path + query) of a Location pointing at KYM, or #f
;; when it points somewhere else. Relative locations are kept as is.
(define (local-location uri)
  (and (or (not (uri-host uri))
           (member (uri-host uri) '("knowyourmeme.com"
                                    "www.knowyourmeme.com"
                                    "trending.knowyourmeme.com")))
       (string-append (uri-path uri)
                      (if (uri-query uri)
                        (string-append "?" (uri-query uri))
                        ""))))

;; Relay an upstream redirect as a redirect within MeMe, keeping the
;; query string (e.g. /photos/sort/views -> /photos?sort=views). A
;; redirect off KYM is not followed blindly: the user gets a page
;; explaining where it leads.
(define (redirect response code path)
  (let ((target (local-location (response-location response))))
    (if target
      (values (build-response
                #:code code
                #:headers `((location . ,(parse-header 'location target))))
              "Redirect")
      (error-page 502 "Know Your Meme redirected this page outside the site."
                  path))))

(define* (error-page code #:optional message path)
  (values (build-response
            #:code code
            #:headers `((content-type . (text/html))))
          (shtml->html (error-template code message path))))

;; Parse and render an upstream page. Scrapers rely on KYM's markup, so a
;; layout change makes them throw: turn that into a readable error page
;; and a single log line instead of a bare 500 with a backtrace.
(define (render-page procedure path body)
  (catch #t
    (lambda ()
      (let ((doc (html->sxml-0nf body #:strict? #t)))
        (values good-response
                (string-replace-substring
                  (string-replace-substring
                    (shtml->html (parameterize ((current-path path))
                                   (procedure doc)))
                    "https://knowyourmeme.com:443" "")
                  "https://knowyourmeme.com" ""))))
    (lambda (key . args)
      (pass-signals key args)
      (log-error "Render error" key args)
      (error-page 500
                  (string-append "Know Your Meme changed the layout of "
                                 "this page and MeMe could not read it.")
                  path))))

(define* (generic-page procedure path
                       #:optional (base-url "https://knowyourmeme.com")
                                  (retried? #f))
  (receive (resp body)
      ;; Any failure talking to KYM (DNS, TLS, refused connection, a
      ;; malformed response...) ends up here, not just bad-response.
      (catch #t
        (lambda ()
          (fetch (string-append base-url path) #:headers user-agent-header))
        (lambda (key . args)
          (pass-signals key args)
          (log-error "Upstream error" key args)
          (values #f key)))
    (if (not resp)
      (values (build-response #:code 502
                              #:headers `((content-type . (text/html))))
              (shtml->html (eof-template path body)))
      (let ((code (response-code resp)))
        (display (string-append "; \tStatus: " (number->string code)))
        (newline)
        (cond ((= code 200)
               (render-page procedure path body))
              ((and (>= code 300) (<= code 399))
               (let ((location (response-location resp)))
                 (cond ((not location)
                        (error-page 502 "Know Your Meme sent an invalid redirect."
                                    path))
                       ((not (equal? (uri-path location) path))
                        (redirect resp code path))
                       ;; Redirected to the same path: some listings live
                       ;; on the trending host. Try it once, never loop.
                       ((not retried?)
                        (generic-page procedure path
                                      "https://trending.knowyourmeme.com" #t))
                       (else
                        (error-page 502 "Know Your Meme keeps redirecting this page to itself."
                                    path)))))
              (else (error-page code)))))))

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

;; Search results use the same gallery markup as image listings, for
;; every context (entries, images, videos, news, editorials, users...).
(define (search-page path)
  (display "; \tHandler: search-page")
  (generic-page photos-template path))

;; Video listings (/videos, /videos/trending, /memes/<slug>/videos) use
;; the same gallery markup as image listings.
(define (videos-page path)
  (display "; \tHandler: videos-page")
  (generic-page photos-template path))

(define (video-page path)
  (display "; \tHandler: video-page")
  (generic-page video-template path))

(define (article-page path)
  (display "; \tHandler: article-page")
  (generic-page article-template path))

;; Value of the url= parameter. Split on the first "=" only: the value
;; itself may contain "=" (older links were not percent-encoded).
(define (proxy-target query)
  (let loop ((pairs (string-split (or query "") #\&)))
    (cond ((null? pairs) #f)
          ((string-prefix? "url=" (car pairs))
           (let ((url (uri-decode (substring (car pairs) 4)
                                  #:decode-plus-to-space? #f)))
             (cond ((string-null? url) #f)
                   ((string-prefix? "//" url) (string-append "https:" url))
                   ((string-prefix? "/" url)
                    (string-append "https://knowyourmeme.com" url))
                   (else (https-upgrade url)))))
          (else (loop (cdr pairs))))))

;; Fetch an image, following redirects internally as long as they stay
;; on allowed hosts. Relaying them to the browser would turn a CDN path
;; into a local path that does not exist.
(define* (proxy-fetch url #:optional (hops 0))
  (receive (resp body) (fetch url)
    (let ((code (response-code resp))
          (location (response-location resp)))
      (if (and (>= code 300) (<= code 399) location (< hops 3))
        (let ((next (if (uri-host location)
                      (uri->string location)
                      (let ((base (string->uri url)))
                        (string-append (symbol->string (uri-scheme base))
                                       "://" (uri-host base)
                                       (uri->string location))))))
          (if (proxyable? next)
            (proxy-fetch next (1+ hops))
            (values resp body)))
        (values resp body)))))

(define (proxy-page query)
  (display "; \tHandler: proxy-page")
  (let ((url (proxy-target query)))
    (if (not (and url (proxyable? url)))
      (begin (newline) (error-page 400))
      (receive (resp body)
          (catch #t
            (lambda () (proxy-fetch url))
            (lambda (key . args)
              (pass-signals key args)
              (log-error "Proxy upstream error" key args)
              (values #f #f)))
        (if (not resp)
          (error-page 502)
          (let ((code (response-code resp)))
            (display (string-append "; \tStatus: " (number->string code)))
            (newline)
            (if (= code 200)
              (values (build-response
                        #:code 200
                        #:headers `((content-type
                                     . ,(or (response-content-type resp)
                                            '(application/octet-stream)))
                                    (cache-control
                                     . ,(parse-header 'cache-control
                                                      "max-age=604800"))))
                      body)
              (error-page (if (and (>= code 400) (<= code 499)) 404 502)))))))))
