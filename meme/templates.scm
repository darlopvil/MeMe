;; Copyright (C) 2024-2025 Skylar Astaroth <cobra@vern.cc>
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
  #:export (error-template
            eof-template
            articles-template
            editorials-template
            meme-template
            meme-list-template
            category-list-template
            photos-template
            photo-template))

(define (kym->local url)
  (if (and (>= (string-length url) 24)
           (equal? (substring url 0 24) "https://knowyourmeme.com"))
    (substring url 24)
    (if (and (>= (string-length url) 33)
             (equal? (substring url 0 33) "https://trending.knowyourmeme.com"))
      (substring url 33)
      url)))

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

(define footer
  `(footer
     (div (@ (class "box footer"))
       (p (a (@ (href "https://git.vern.cc/cobra/MeMe")) "Source Code"))
       ,(if (getenv "PATCHES_URL")
          `(p (a (@ (href ,(getenv "PATCHES_URL"))) "Patches"))
          '()))))

(define (sidebar-gallery-template sidebar-gallery)
  (map (lambda (l)
         `(div (@ (class "sidebar box"))
           ,(car l)
           (table (@ (class "gallery"))
             (tr (td (a (@ (href ,(kym->local (cadr l))))
                       (img (@ (src ,(proxy (cadddr l)))
                               (alt ,(caddr l))
                               (loading "lazy")))
                       (p (b ,(list-ref l 4)))))
                 (td (a (@ (href ,(kym->local (list-ref l 5))))
                       (img (@ (src ,(proxy (list-ref l 7)))
                               (alt ,(list-ref l 6))
                               (loading "lazy")))
                       (p (b ,(list-ref l 8))))))
             (tr (td (a (@ (href ,(kym->local (list-ref l 9))))
                       (img (@ (src ,(proxy (list-ref l 11)))
                               (alt ,(list-ref l 10))
                               (loading "lazy")))
                       (p (b ,(list-ref l 12)))))
                 (td (a (@ (href ,(kym->local (list-ref l 13))))
                       (img (@ (src ,(proxy (list-ref l 15)))
                               (alt ,(list-ref l 14))
                               (loading "lazy")))
                       (p (b ,(list-ref l 16)))))))))
       sidebar-gallery))

(define (sidebar-trending-template sidebar-trending)
  (if (not (equal? sidebar-trending ""))
    `(div (@ (class "sidebar box"))
       (h3 (a (@ (href ,(kym->local (car sidebar-trending))))
             ,(cadr sidebar-trending)))
       (table (@ (class "trending-images"))
         ,(map
            (lambda (r)
              `(tr
                 ,(map (lambda (p)
                         `(td (a (@ (href ,(kym->local (car p))))
                                (img (@ (src ,(proxy (cdr p)))
                                        (loading "lazy"))))))
                       r)))
            (cddr sidebar-trending))))
    '()))

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

(define (articles-template body)
  `(html ,(html-head body)
         (body
           ,heading
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
                           (display l) (newline)
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

(define (meme-list-template body)
  (let ((meme-list (get-meme-list body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box meme-list"))
               (h1 ,(car meme-list))
               ,(if (equal? (cadr meme-list) '())
                  '()
                  (caadr meme-list))
               ,(caaddr meme-list)
               (br)
               (div (@ (style "display: inline-flex;"))
                 ,(cddar (cadddr meme-list))))
             (div (@ (class "box meme-list-body"))
               (ul (@ (class "meme-list-list"))
                 ,(map (lambda (l)
                         `(li
                            (a (@ (href ,(kym->local (car l))))
                              (img (@ (src ,(proxy (cadr l)))
                                      (loading "lazy")))
                              (p (@ (class "tooltip box"))
                                ,(caddr l)))))
                       (car (cddddr meme-list)))))
             ,(if (not (equal? (get-pagination body) ""))
                `(div (@ (class "wide-pagination"))
                   ,(pagination-template (get-pagination body)))
                '())
             ,footer))))

(define (category-list-template body)
  (let ((category-list (get-category-list body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box category-list"))
               ,(car category-list)
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
                       (cadr category-list))))
             ,footer))))

(define (photos-template body)
  (let ((photos (get-photos body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "box photos"))
               (h1 ,(car photos))
               ,(cadr photos)
               (br)
               ,(if (equal? '() (caddr photos))
                  '()
                  `(div (@ (style "display: inline-flex;"))
                     ,(cddr (caddr photos)))))
             (div (@ (class "box photos-gallery"))
               ,(map (lambda (p)
                       `(a (@ (href ,(car p)))
                          (img (@ (src ,(proxy (cdr p)))
                                  (loading "lazy")))))
                     (cadddr photos)))
             ,(if (not (equal? (get-pagination body) '()))
                `(div (@ (class "wide-pagination"))
                   ,(pagination-template (get-pagination body)))
                '())
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
