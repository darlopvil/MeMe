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

(define-module (meme templates)
  #:use-module (meme scraping)
  #:use-module (ice-9 string-fun)
  #:export (current-path
            error-template
            eof-template
            home-template
            editorials-template
            meme-template
            memes-template
            categories-template
            photos-template
            photo-template
            article-template))

;; Path (with query string) of the request being rendered; set by
;; generic-page so templates can build links back to the same listing.
(define current-path (make-parameter "/"))

(define (kym->local url)
  (cond ((string-prefix? "https://knowyourmeme.com" url)
         (string-drop url 24))
        ((string-prefix? "https://trending.knowyourmeme.com" url)
         (string-drop url 33))
        (else url)))

(define (html-head body)
  `(head (title ,(string-append (get-title body) " | MeMe"))
         (meta (@ (charset "UTF-8")))
         (meta (@ (name "viewport") (content "width=device-width")))
         (link (@ (rel "stylesheet") (href "/style.css")))
         (link (@ (rel "icon") (type "image/png") (href "/favicon.png")))))

(define heading
  `(header
     (div (@ (class "box heading"))
       (ul (@ (class "navbar"))
           (li (@ (class "home"))
             (a (@ (href "/")) "home"))
           (li (@ (class "memes"))
             (a (@ (href "/memes")) "memes")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/memes/submissions")) "submissions"))
                 (li (a (@ (href "/memes/researching")) "researching"))
                 (li (a (@ (href "/memes/newsworthy")) "newsworthy"))
                 (li (a (@ (href "/memes/popular")) "popular"))
                 (li (a (@ (href "/memes/deadpool")) "deadpool"))
                 (li (a (@ (href "/memes/all")) "all")))))
           (li (@ (class "categories"))
             (a (@ (href "/categories")) "categories")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/categories/culture")) "cultures"))
                 (li (a (@ (href "/categories/event")) "events"))
                 (li (a (@ (href "/categories/meme")) "memes"))
                 (li (a (@ (href "/categories/person")) "people"))
                 (li (a (@ (href "/categories/site")) "sites"))
                 (li (a (@ (href "/categories/subculture")) "subcultures")))))
           (li (@ (class "images"))
             (a (@ (href "/photos")) "images")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/photos/trending")) "trending"))
                 (li (a (@ (href "/photos/most-commented")) "most comments"))
                 (li (a (@ (href "/photos/most-favorited"))
                       "most favorites"))
                 (li (a (@ (href "/photos/most-liked")) "most likes"))
                 (li (a (@ (href "/photos/least-liked")) "least likes"))
                 (li (a (@ (href "/photos/most-viewed")) "most views"))
                 (li (a (@ (href "/photos/templates")) "template")))))
           (li (@ (class "editorials"))
             (a (@ (href "/editorials")) "editorial")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/editorials/interviews")) "interviews"))
                 (li (a (@ (href "/editorials/in-the-media")) "in the media"))
                 (li (a (@ (href "/editorials/white-papers")) "white papers"))
                 (li (a (@ (href "/editorials/episode-notes")) "episode notes"))
                 (li (a (@ (href "/editorials/behind-the-scenes"))
                       "behind the scenes"))
                 (li (a (@ (href "/editorials/meme-review")) "meme review"))
                 (li (a (@ (href "/editorials/collections")) "collections"))
                 (li (a (@ (href "/editorials/poll")) "poll"))
                 (li (a (@ (href "/editorials/guides")) "guides"))
                 (li (a (@ (href "/editorials/meme-insider")) "meme-insider"))
                 (li (a (@ (href "/editorials/insights")) "insights")))))
              (li (@ (class "episodes"))
                (a (@ (href "/episodes"))))))))

(define source-url
  (or (getenv "SOURCE_URL") "https://github.com/darlopvil/MeMe"))

(define footer
  `(footer
     (div (@ (class "box footer"))
       (p (a (@ (href ,source-url)) "Source Code")
          " · Fork of "
          (a (@ (href "https://codeberg.org/cobra/MeMe")) "cobra/MeMe")))))

;; Link to page N of the listing at PATH, which may already carry a
;; /page/M suffix and a query string (e.g. ?sort=views).
(define (page-link path n)
  (let* ((q (string-index path #\?))
         (base (if q (substring path 0 q) path))
         (query (if q (substring path q) ""))
         (m (string-contains base "/page/"))
         (base (if m (substring base 0 m) base))
         (base (if (string-suffix? "/" base) (string-drop-right base 1) base)))
    (string-append base
                   (if (= n 1) "" (string-append "/page/" (number->string n)))
                   query)))

;; KYM galleries use infinite scroll and no longer ship a pager, but
;; they still serve /page/N server-side, so build prev/next links here.
(define (gallery-pager page count total)
  (let ((prev? (> page 1))
        (next? (and (> count 0)
                    (if total (< (* page count) total) #t))))
    (if (or prev? next?)
      `(div (@ (class "wide-pagination"))
         (div (@ (class "pagination box"))
           (ul (@ (class "pager"))
             (li ,(if prev?
                    `(a (@ (class "page-button")
                           (href ,(page-link (current-path) (- page 1))))
                        (i (@ (class "fa fa-chevron-left"))))
                    '()))
             (li ,(number->string page))
             (li ,(if next?
                    `(a (@ (class "page-button")
                           (href ,(page-link (current-path) (+ page 1))))
                        (i (@ (class "fa fa-chevron-right"))))
                    '())))))
      '())))

(define (pagination-template pagination)
  (if (not (equal? pagination '()))
    `(div (@ (class "pagination box"))
       (ul (@ (class "pager"))
         ,pagination))
    '()))



(define (error-template code)
  `(html ,(html-head (number->string code))
         (body
           ,heading
           (center
             (h1 (@ (class "error")) ,(number->string code)))
           ,footer)))

(define (eof-template)
  `(html (head (title "bad-response | MeMe")
               (meta (@ (http-equiv "refresh")
                        (content "1")))
               (meta (@ (charset "UTF-8")))
               (meta (@ (name "viewport") (content "width=device-width")))
               (link (@ (rel "stylesheet") (href "/style.css")))
               (link (@ (rel "icon") (type "image/png") (href "/favicon.png"))))
         (body
           ,heading
           (br)
           (center
             (p ,(string-append "Key \"bad-response\" thrown while reading "
                                "from knowyourmeme.com. This usually goes "
                                "away after a few refreshes.")))
           (br)
           ,footer)))

(define home-template
  `(html ,(html-head "Home")
         (body
           ,heading
           (div (@ (class "home box"))
             (h1 "MeMe")
             (p "MeMe is an alternative front-end for KnowYourMeme.")
             (p "MeMe provides a lightweight, JavaScript-free interface for browsing KnowYourMeme.")
             (h2 "How to use")
             (p "You can use MeMe by replacing knowyourmeme.com with the hostname of this instance in the URL of a KnowYourMeme page.")
             (p "Alternatively, you can use the " (a (@ (href "https://libredirect.github.io/")) "LibRedirect browser extension") " to have this done automatically for all KnowYourMeme pages.")
             (p "Example: "
                (a (@ (href
                        "https://knowyourmeme.com/memes/navy-seal-copypasta"))
                   "KnowYourMeme")
                " / " (a (@ (href "/memes/navy-seal-copypasta")) "MeMe")))
           ,footer)))

(define (editorials-template body)
  `(html ,(html-head body)
         (body
           ,heading
           ,(map (lambda (l)
                   `(div (@ (class "editorials box"))
                      (a (@ (href ,(kym->local (car l))))
                        (img (@ (src ,(proxy (caddr l)))
                                (alt ,(cadr l))
                                (loading "lazy")))
                        (h3 ,(cadr l))
                        (p ,(cadddr l)))))
                 (get-editorials body))
           (div (@ (class "wide-pagination"))
             ,(pagination-template (get-pagination body)))
           ,footer)))

(define (meme-template body)
  (let ((meme (get-meme body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box meme"))
               (img (@ (src ,(proxy (cadr meme)))
                       (loading "lazy")))
               (h1 ,(car meme))
               (div (@ (class "subdiv"))
                 ,(caddr meme)
                 ,(cadddr meme)
                 ,(list-ref meme 4)))
             (div (@ (class "box meme-body"))
                ,(append '(div)
                         (list-ref meme 5)))
             ,(if (not (equal? (car (last-pair meme)) '()))
                `(div (@ (class "collection-wrapper"))
                   ,(map (lambda (l)
                           `(div (@ (class "box collection-item"))
                              ,(caar l)
                              (p (a (@ (href ,(car (cadddr l)))) "Source"))
                              (a (@ (href ,(kym->local (caadr l))))
                                (img (@ (src ,(caaddr l))
                                        (loading "lazy"))))
                              ,(append '(span) (list-ref l 4))))
                         (car (last-pair meme))))
                '())
             ,footer))))

(define (memes-template body)
  (let ((memes (get-memes body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box memes"))
               (h1 ,(car memes))
               ,(if (equal? (cadr memes) '())
                  '()
                  (caadr memes))
               ,(if (equal? (caddr memes) '())
                  '()
                  (caaddr memes))
               (br)
               (div (@ (style "display: inline-flex;"))
                 ,(cddar (cadddr memes))))
             (div (@ (class "box memes-body"))
               (ul (@ (class "memes-list"))
                 ,(map (lambda (l)
                         `(li
                            (a (@ (href ,(kym->local (car l))))
                              (img (@ (src ,(proxy (cadr l)))
                                      (loading "lazy")))
                              (p (@ (class "tooltip box"))
                                ,(caddr l)))))
                       (car (cddddr memes)))))
             ,(if (not (equal? (get-pagination body) ""))
                `(div (@ (class "wide-pagination"))
                   ,(pagination-template (get-pagination body)))
                '())
             ,footer))))

(define (categories-template body)
  (let ((categories (get-categories body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box categories"))
               ,(car categories)
               (ul
                 ,(map (lambda (l)
                         `(li
                            (a (@ (href ,(caar l)))
                              ,(string-append
                                 (cadar l) " ("
                                 (caddar l) ")"))
                            (ul
                              ,(map (lambda (m)
                                      `(li (a (@ (href ,(car m)))
                                             ,(string-append
                                                (cadr m) " (" (caddr m) ")"))))
                                    (cadr l)))
                            (br)))
                       (cadr categories))))
             ,footer))))

(define (photos-template body)
  (let* ((photos (get-photos body))
         (title (list-ref photos 0))
         (tabs  (list-ref photos 1))
         (sorts (list-ref photos 2))
         (items (list-ref photos 3))
         (page  (list-ref photos 4))
         (total (list-ref photos 5)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box photos"))
               ,(or title '(h1 "Images"))
               ,(if (null? tabs)
                  '()
                  `(div (@ (style "display: inline-flex;")) ,@tabs))
               ,(if (null? sorts)
                  '()
                  `(div (b "Sorted by: ")
                        (div (@ (style "display: inline-flex;")) ,@sorts))))
             ,(if (null? items)
                `(div (@ (class "box photos")) (p "No images found."))
                `(div (@ (class "box photos-gallery"))
                   ,(map (lambda (i)
                           `(a (@ (href ,(car i)) (title ,(cadr i)))
                              (img (@ (src ,(proxy (caddr i)))
                                      (alt ,(cadr i))
                                      (loading "lazy")))))
                         items)))
             ,(gallery-pager page (length items) total)
             ,footer))))

(define (article-template body)
  (let ((a (get-article body)))
    `(html ,(html-head body)
           (body
             ,heading
             ,(if (not a)
                `(div (@ (class "box meme"))
                   (p "Could not read this article."))
                `((div (@ (class "box meme"))
                    ,(if (list-ref a 2)
                       `(img (@ (src ,(proxy (list-ref a 2)))
                                (loading "lazy")))
                       '())
                    ,(or (list-ref a 0) '())
                    (h1 ,(list-ref a 1))
                    (div (@ (class "subdiv"))
                      ,(if (null? (list-ref a 3))
                         '()
                         `(p "By " ,@(list-ref a 3)))
                      (p ,(list-ref a 4))))
                  (div (@ (class "box meme-body"))
                    ,(list-ref a 5)
                    ,(or (list-ref a 6) '()))))
             ,footer))))

(define (photo-template body)
  (let ((photo (get-photo body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "photo-container"))
               (div (@ (class "box photo-header"))
                 ,(car photo)
                 (p "Source: " ,(cadddr photo))
                 ,(list-ref photo 4)
                 ,(list-ref photo 5))
               (div (@ (class "box photo-body"))
                 (a (@ (href ,(proxy (caddr photo)))
                       (target "_blank"))
                   (img (@ (alt ,(cadr photo))
                           (src ,(proxy (caddr photo)))
                           (loading "lazy"))))))
             ,footer))))
