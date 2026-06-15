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
            home-template
            editorials-template
            meme-template
            memes-template
            categories-template
            photos-template
            photo-template))

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

(define footer
  `(footer
     (div (@ (class "box footer"))
       (p (a (@ (href "https://git.vern.cc/cobra/MeMe")) "Source Code"))
       ,(if (getenv "PATCHES_URL")
          `(p (a (@ (href ,(getenv "PATCHES_URL"))) "Patches"))
          '()))))

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
               ,(caaddr memes)
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
