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
  #:use-module (ice-9 regex)
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
            article-template
            video-template
            profile-template))

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
                 (li (a (@ (href "/memes/confirmed")) "confirmed"))
                 (li (a (@ (href "/memes/submissions")) "submissions"))
                 (li (a (@ (href "/memes/newsworthy")) "newsworthy"))
                 (li (a (@ (href "/memes/popular")) "popular"))
                 (li (a (@ (href "/memes/deadpool")) "deadpool"))
                 (li (a (@ (href "/memes/all")) "all"))
                 (li (a (@ (href "/memes/random")) "random")))))
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
                 (li (a (@ (href "/photos?sort=comments")) "most comments"))
                 (li (a (@ (href "/photos?sort=favorites")) "most favorites"))
                 (li (a (@ (href "/photos?sort=score")) "most likes"))
                 (li (a (@ (href "/photos?sort=low-score")) "least likes"))
                 (li (a (@ (href "/photos?sort=views")) "most views"))
                 (li (a (@ (href "/photos/templates")) "template"))
                 (li (a (@ (href "/photos/random")) "random")))))
           (li (@ (class "videos"))
             (a (@ (href "/videos")) "videos")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/videos/trending")) "trending"))
                 (li (a (@ (href "/episodes")) "episodes"))
                 (li (a (@ (href "/videos/random")) "random")))))
           (li (@ (class "editorials"))
             (a (@ (href "/editorials")) "editorial")
             (div (@ (class "dropdown box"))
               (ul
                 (li (a (@ (href "/news")) "news"))
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
              (li (@ (class "search"))
                (form (@ (method "get") (action "/search"))
                  (input (@ (type "search") (name "q")
                            (placeholder "search")
                            (aria-label "Search Know Your Meme")))))))))

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
;; Search paginates with a page= query parameter instead of /page/N.
(define (search-page-link query n)
  (let* ((params (filter (lambda (p)
                           (not (or (string-null? p)
                                    (string-prefix? "page=" p))))
                         (string-split (if (string-prefix? "?" query)
                                         (substring query 1)
                                         query)
                                       #\&)))
         (params (if (= n 1)
                   params
                   (append params
                           (list (string-append "page=" (number->string n)))))))
    (string-append "/search"
                   (if (null? params) "" (string-append "?" (string-join params "&"))))))

(define (page-link path n)
  (let* ((q (string-index path #\?))
         (base (if q (substring path 0 q) path))
         (query (if q (substring path q) ""))
         (m (string-contains base "/page/"))
         (base (if m (substring base 0 m) base))
         (base (if (string-suffix? "/" base) (string-drop-right base 1) base)))
    (if (string=? base "/search")
      (search-page-link query n)
      (string-append base
                     (if (= n 1) "" (string-append "/page/" (number->string n)))
                     query))))

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



;; MESSAGE explains what went wrong; PATH, when given, adds a link to
;; the same page on KYM (clearly marked as leaving MeMe).
(define* (error-template code #:optional message path)
  `(html ,(html-head (number->string code))
         (body
           ,heading
           (center
             (h1 (@ (class "error")) ,(number->string code))
             ,(if message `(p ,message) '())
             ,(if path
                `(p (a (@ (href ,(string-append "https://knowyourmeme.com"
                                                path)))
                       "Open this page on Know Your Meme")
                    " (leaves MeMe)")
                '()))
           ,footer)))

;; KYM could not be reached or answered garbage. No automatic refresh:
;; an upstream outage must not turn every open tab into a request loop.
(define* (eof-template #:optional path reason)
  `(html ,(html-head "Upstream error")
         (body
           ,heading
           (br)
           (center
             (h1 (@ (class "error")) "502")
             (p "MeMe could not get a valid response from Know Your Meme"
                ,(if reason (format #f " (~a)" reason) "")
                ". This is usually temporary.")
             ,(if path `(p (a (@ (href ,path)) "Try again")) '()))
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

;; KYM double-escapes some markup in editorial summaries, so once parsed
;; they read "a <i>Hamilton</i> fandom" literally. Turn the harmless
;; inline tags (i, em, b, strong) back into elements; anything else stays
;; plain text, which shtml->html escapes.
(define (inline-markup s)
  (define (add frame x) (cons (car frame) (cons x (cdr frame))))
  (define (close frame) (cons (car frame) (reverse (cdr frame))))
  (let loop ((s s) (stack (list (list 'top))))
    (let* ((m (string-match "<(/?)(i|em|b|strong)>" s))
           (pre (if m (match:prefix m) s))
           (stack (if (string-null? pre)
                    stack
                    (cons (add (car stack) pre) (cdr stack)))))
      (cond
        ((not m)
         (let unwind ((stack stack))
           (if (null? (cdr stack))
             (cdr (close (car stack)))
             (unwind (cons (add (cadr stack) (close (car stack)))
                           (cddr stack))))))
        ((string-null? (match:substring m 1))
         (loop (match:suffix m)
               (cons (list (string->symbol (match:substring m 2))) stack)))
        ((and (pair? (cdr stack))
              (eq? (caar stack) (string->symbol (match:substring m 2))))
         (loop (match:suffix m)
               (cons (add (cadr stack) (close (car stack))) (cddr stack))))
        (else
         (loop (match:suffix m)
               (cons (add (car stack) (match:substring m 0))
                     (cdr stack))))))))

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
                        (h3 ,@(inline-markup (cadr l)))
                        (p ,@(inline-markup (cadddr l))))))
                 (get-editorials body))
           (div (@ (class "wide-pagination"))
             ,(pagination-template (get-pagination body)))
           ,footer)))

;; Path of the page being rendered, without query string.
(define (current-base-path)
  (let* ((p (current-path)) (q (string-index p #\?)))
    (if q (substring p 0 q) p)))

(define (entry-grid items)
  `(ul (@ (class "entry-grid-list"))
     ,(map (lambda (i)
             `(li (a (@ (href ,(car i)) (title ,(cadr i)))
                     (img (@ (src ,(proxy (caddr i))) (alt ,(cadr i)) (loading "lazy")))
                     (span ,(cadr i)))))
           items)))

;; A thumbnail strip: related entries, sub-entries, recent videos or
;; recent images. ID and ANCHOR keep KYM's fragment links working.
(define* (entry-grid-box block #:key id anchor more)
  (let ((items (and block (caddr block)))
        (gallery (and block (> (length block) 3) (list-ref block 3))))
    (if (or (not block) (and (null? items) (not gallery)))
      '()
      `(div (@ (class "box entry-grid") ,@(if id `((id ,id)) '()))
         (h2 ,@(if anchor `((@ (id ,anchor))) '())
             ,(car block) " " (small ,(cadr block)))
         ,(if (null? items)
            `(p (a (@ (href ,gallery)) "View gallery")
                " (marked by Know Your Meme as not suitable for public viewing)")
            `(,(entry-grid items) ,(or more '())))))))

(define (meme-template body)
  (let ((meme (get-meme body))
        (extras (get-entry-extras body)))
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
             ,(map entry-grid-box (assq-ref extras 'related))
             ,(entry-grid-box (assq-ref extras 'videos)
                              #:id "videos" #:anchor "recent-videos"
                              #:more `(p (a (@ (href ,(string-append (current-base-path) "/videos")))
                                            "View all videos")))
             ,(entry-grid-box (assq-ref extras 'photos)
                              #:id "photos" #:anchor "recent-images"
                              #:more `(p (a (@ (href ,(string-append (current-base-path) "/photos")))
                                            "View all images")))
             ,(if (not (equal? (car (last-pair meme)) '()))
                `(div (@ (class "collection-wrapper"))
                   ,(map (lambda (l)
                           `(div (@ (class "box collection-item"))
                              ,(caar l)
                              (p (a (@ (href ,(car (cadddr l)))) "Source"))
                              (a (@ (href ,(kym->local (caadr l))))
                                (img (@ (src ,(proxy (caaddr l)))
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
             ,(let ((pagination (get-pagination body)))
                (if (null? pagination)
                  '()
                  `(div (@ (class "wide-pagination"))
                     ,(pagination-template pagination))))
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

;; "/users/<name>" when rendering one of a user's galleries, else #f
(define (profile-path)
  (let ((m (string-match "^/users/[^/]+" (current-base-path))))
    (and m (match:substring m 0))))

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
               ,(if (profile-path)
                  `(p (a (@ (href ,(profile-path))) "‹ Back to profile"))
                  '())
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

(define (video-template body)
  (let ((v (get-video body)))
    `(html ,(html-head body)
           (body
             ,heading
             ,(if (not v)
                `(div (@ (class "box meme"))
                   (p "Could not read this video."))
                (let* ((entry (assq-ref v 'entry))
                       (nav (filter pair?
                                    (list (let ((h (assq-ref v 'previous)))
                                            (if h `(a (@ (href ,h)) "◄ Previous") '()))
                                          (let ((h (assq-ref v 'gallery)))
                                            (if h `(a (@ (href ,h)) "View gallery") '()))
                                          (let ((h (assq-ref v 'next)))
                                            (if h `(a (@ (href ,h)) "Next ►") '()))))))
                  `(div (@ (class "box video-page"))
                     ,(if entry `(p (a (@ (href ,(car entry))) ,(cadr entry))) '())
                     (h1 ,(assq-ref v 'title))
                     ,(uploader-line body)
                     ,(or (assq-ref v 'player) '())
                     ,(if (null? nav) '()
                        `(p (@ (class "video-nav"))
                            ,(car nav)
                            ,@(apply append (map (lambda (n) (list " · " n)) (cdr nav))))))))
             ,footer))))

;; "Uploaded by <user>" line for image and video pages, or nothing
(define (uploader-line body)
  (let ((u (get-uploader body)))
    (if u
      `(p (@ (class "uploader"))
          "Uploaded by " (a (@ (href ,(kym->local (cdr u)))) ,(car u)))
      '())))

;; " · " between the elements of LST
(define (interleave lst)
  (if (null? lst) '()
    (cons (car lst)
          (apply append (map (lambda (x) (list " · " x)) (cdr lst))))))

(define (profile-template body)
  (let ((p (get-profile body)))
    (if (not p)
      `(html ,(html-head body)
             (body ,heading
                   (div (@ (class "box meme"))
                     (p "Could not read this profile."))
                   ,footer))
      (let ((field (lambda (k) (assq-ref p k)))
            (since (filter identity (list (assq-ref p 'location)
                                          (assq-ref p 'joined)))))
        `(html ,(html-head (string-append (field 'name) "'s profile"))
               (body
                 ,heading
                 (div (@ (class "box profile"))
                   ,(if (field 'avatar)
                      `(img (@ (class "avatar") (src ,(proxy (field 'avatar)))
                               (alt "") (loading "lazy")))
                      '())
                   (h1 ,(field 'name))
                   ,(if (field 'role) `(p (@ (class "role")) ,(field 'role)) '())
                   ,(if (null? since) '() `(p ,@(interleave since)))
                   ,(if (field 'contributions)
                      `(p (b "Contributions: ") ,(field 'contributions))
                      '())
                   ,(if (null? (field 'links))
                      '()
                      `(p (b "Links: ")
                          ,@(interleave
                              (map (lambda (l)
                                     `(a (@ (href ,(cdr l))
                                            (rel "nofollow noopener noreferrer"))
                                         ,(car l)))
                                   (field 'links)))))
                   ,(if (null? (field 'tabs))
                      '()
                      `(p (@ (class "profile-tabs"))
                          ,@(interleave
                              (map (lambda (t) `(a (@ (href ,(cdr t))) ,(car t)))
                                   (field 'tabs))))))
                 ,(if (field 'about)
                    `(div (@ (class "box meme-body profile-about"))
                       (h2 "About")
                       ,@(field 'about))
                    '())
                 ,footer))))))

(define (photo-template body)
  (let ((photo (get-photo body)))
    `(html ,(html-head body)
           (body
             ,heading
             (div (@ (class "photo-container"))
               (div (@ (class "box photo-header"))
                 ,(car photo)
                 (p "Source: " ,(cadddr photo))
                 ,(uploader-line body)
                 ,(list-ref photo 4)
                 ,(list-ref photo 5))
               (div (@ (class "box photo-body"))
                 (a (@ (href ,(proxy (caddr photo)))
                       (target "_blank"))
                   (img (@ (alt ,(cadr photo))
                           (src ,(proxy (caddr photo)))
                           (loading "lazy"))))))
             ,footer))))
