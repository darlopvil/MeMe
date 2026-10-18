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

(define-module (meme scraping)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 regex)
  #:use-module (web uri)
  #:use-module (ice-9 string-fun)
  #:use-module ((sxml xpath)
                #:renamer (lambda (p)
                            (if (eq? 'filter p)
                              'sxml-filter
                              p)))
  #:export (proxy
            proxyable?
            https-upgrade
            get-editorials
            get-meme
            get-entry-extras
            get-memes
            get-categories
            get-photos
            get-photo
            get-article
            get-pagination
            get-title))

;; The target URL is percent-encoded: image URLs may contain "&", "="
;; or "+", which would otherwise break the query string or be decoded
;; as spaces by uri-decode.
;; KYM still serves some image URLs over plain http (old YouTube
;; thumbnails, for instance). The proxy only accepts https, so upgrade
;; URLs on allowed hosts instead of rejecting them.
(define (https-upgrade url)
  (if (string-prefix? "http://" url)
    (let ((secure (string-append "https://" (substring url 7))))
      (if (proxyable? secure) secure url))
    url))

(define (proxy url)
  (string-append "/proxy?url=" (uri-encode (https-upgrade url))))

(define (not-newline s)
  (not (equal? "\n" s)))

;; Class-based selection helpers. KYM's markup shifts often, so these
;; match on element type plus one class token instead of on absolute
;; positional paths, which break as soon as a wrapper div is added.
(define (attr name node)
  (let ((v ((sxpath `(@ ,name *text*)) node)))
    (if (null? v) #f (car v))))

(define (has-class? class node)
  (let ((c (attr 'class node)))
    (and c (member class (string-tokenize c)) #t)))

(define (select tag class node)
  (filter (lambda (n) (has-class? class n))
          ((sxpath `(// ,tag)) node)))

(define (select-first tag class node)
  (let ((r (select tag class node)))
    (if (null? r) #f (car r))))

(define (text-of nodes)
  (string-join
    (string-tokenize
      (string-join (append-map* (lambda (n) ((sxpath '(// *text*)) n))
                                nodes)
                   " "))
    " "))

(define (append-map* proc lst)
  (apply append (map proc lst)))

(define (decode-nsfw-src s)
  (utf8->string
    (base64-decode (string-delete char-set:whitespace s)
                   base64-alphabet #f #f)))

;; (href title image summary) for each article in the list sections
(define (get-editorials d)
  (map (lambda (a)
         (let ((img ((sxpath '(// img)) a)))
           (list (or (attr 'href a) "")
                 (text-of ((sxpath '(// h3)) a))
                 (if (null? img) "" (or (attr 'src (car img)) ""))
                 (text-of ((sxpath '(// p)) a)))))
       (append-map* (lambda (li) ((sxpath '(a)) li))
                    (append-map* (lambda (s) (select 'li "item" s))
                                 (select 'section "latest" d)))))


(define (modify-list nodeset pred proc)
  (define ret (list-copy nodeset))
  (do ((i 0 (1+ i)))
      ((>= i (length ret)))
    (let ((e (list-ref ret i)))
      (if (pred e)
       (list-set! ret i (proc e))
        (if (list? e)
          (list-set! ret i (modify-list e pred proc))))))
  ret)

;; Hosts the image proxy accepts. proxy-page uses this same list.
(define proxyable-prefixes
  '("https://i.kym-cdn.com/" "https://a.kym-cdn.com/"
    "https://i.ytimg.com/" "https://knowyourmeme.com/"))

(define (proxyable? url)
  (not (null? (filter (lambda (p) (string-prefix? p url))
                      proxyable-prefixes))))

;; Real source of an <img>: the lazy-load data-src, the decoded NSFW
;; source, or a plain src, in that order. Protocol-relative URLs get
;; https so they can be matched against the proxy allowlist.
(define (img-source e)
  (let ((url (cond ((attr 'data-src e) => identity)
                   ((attr 'data-nsfw-src e) => decode-nsfw-src)
                   ((attr 'src e) => identity)
                   (else #f))))
    (cond ((not url) #f)
          ((string-prefix? "//" url) (string-append "https:" url))
          (else (https-upgrade url)))))

;; Privacy frontends for third-party media, configured through env vars
;; (e.g. RIMGO_URL=https://rimgo.example.org). Unset or empty means
;; "not available": that media type falls back to a plain link.
(define (frontend-url var)
  (let ((v (getenv var)))
    (and v (not (string-null? v))
         (if (string-suffix? "/" v) (string-drop-right v 1) v))))

(define rimgo-url     (frontend-url "RIMGO_URL"))
(define proxitok-url  (frontend-url "PROXITOK_URL"))
(define nitter-url    (frontend-url "NITTER_URL"))
(define invidious-url (frontend-url "INVIDIOUS_URL"))

(define (external-link url label)
  `(a (@ (href ,url) (class "external-media"))
      ,(string-append "[" label ": " url "]")))

;; i.imgur.com/<id>.<ext> -> <rimgo>/<id>.<ext>, or #f
(define (imgur->rimgo url)
  (let ((m (string-match "^https?://i\\.imgur\\.com(/[^?#]+)" url)))
    (and m rimgo-url (string-append rimgo-url (match:substring m 1)))))

;; Images on proxyable hosts go through /proxy, imgur images through
;; rimgo when configured. Anything else (embedded Scratch projects, ...)
;; becomes a plain link instead of a broken image, so the browser never
;; contacts third parties on its own.
(define (img-replace nodeset)
  (modify-list
    nodeset (node-typeof? 'img)
    (lambda (e)
      (let ((url (img-source e))
            (alt (or (attr 'alt e) "")))
        (cond ((not url) '())
              ((proxyable? url)
               `(img (@ (src ,(proxy url)) (alt ,alt) (loading "lazy"))))
              ((imgur->rimgo url)
               => (lambda (src)
                    `(img (@ (src ,src) (alt ,alt) (loading "lazy")))))
              (else (external-link url "external media")))))))

;; Embeds. KYM ships TikToks as <lite-tiktok> / blockquote.tiktok-embed,
;; tweets as blockquote.twitter-tweet(-lazy) and YouTube as
;; <lite-youtube> or a YouTube <iframe>, all turned into players by its
;; JavaScript. Each one becomes an iframe to the configured frontend,
;; wrapped in a <figure> so get-meme's top-level filters (which drop bare
;; iframes and paragraph-less divs) leave it alone.
(define (embed-frame kind src)
  `(figure (@ (class ,(string-append "embed embed-" kind)))
     (iframe (@ (src ,src)
                (loading "lazy")
                (frameborder "0")
                (allowfullscreen "allowfullscreen")))))

(define (tiktok-embed id node)
  (cond ((not id) '())
        (proxitok-url
         (embed-frame "tiktok" (string-append proxitok-url "/embed/v2/" id)))
        (else
         (let ((cite ((sxpath '(// @ cite *text*)) node)))
           (external-link (if (null? cite)
                            (string-append "https://www.tiktok.com/embed/v2/" id)
                            (car cite))
                          "TikTok")))))

(define (tweet-status-id node)
  (let loop ((hrefs ((sxpath '(// a @ href *text*)) node)))
    (cond ((null? hrefs) #f)
          ((string-match "/status/([0-9]+)" (car hrefs))
           => (lambda (m) (match:substring m 1)))
          (else (loop (cdr hrefs))))))

(define (tweet-embed node)
  (let ((id (tweet-status-id node)))
    (cond ((not id) node)
          (nitter-url
           (embed-frame "tweet"
                        (string-append nitter-url "/i/status/" id "/embed")))
          (else
           (external-link (string-append "https://twitter.com/i/status/" id)
                          "tweet")))))

;; "a=1&b=2" -> (("a" . "1") ("b" . "2")), values left encoded
(define (query-params str)
  (map (lambda (kv)
         (let ((i (string-index kv #\=)))
           (if i
             (cons (substring kv 0 i) (substring kv (1+ i)))
             (cons kv ""))))
       (filter (lambda (s) (not (string-null? s)))
               (string-split str #\&))))

;; KYM's "Search Interest" widget is a Google Trends iframe, which shows
;; nothing without JavaScript. Turn it into a link to the same query on
;; Google Trends itself. The search terms live in q= and the date range
;; inside the url-encoded eq= parameter; KYM double-escapes the
;; separators, so "&amp;" is normalised first. The paragraph keeps the
;; section's id so the table of contents link still lands on it.
(define (trends-link src)
  (let* ((s (string-replace-substring src "&amp;" "&"))
         (q (string-index s #\?))
         (params (if q (query-params (substring s (1+ q))) '()))
         (eq (assoc-ref params "eq"))
         (inner (if eq
                  (query-params (uri-decode eq #:decode-plus-to-space? #f))
                  '()))
         (terms (or (assoc-ref params "q") (assoc-ref inner "q")))
         (date (or (assoc-ref inner "date") (assoc-ref params "date") "all")))
    (if (or (not terms) (string-null? terms))
      '()
      `(p (@ (id "search-interest") (class "trends-link"))
          (a (@ (href ,(string-append
                         "https://trends.google.com/trends/explore?date="
                         (uri-encode (uri-decode date)) "&q=" terms))
                (target "_blank")
                (rel "noopener noreferrer"))
             "See how interest in this meme evolved on Google Trends")
          " (opens Google Trends)"))))

(define (youtube-id url)
  (let ((m (string-match "youtube(-nocookie)?\\.com/embed/([A-Za-z0-9_-]+)"
                         url)))
    (and m (match:substring m 2))))

(define (youtube-embed id)
  (cond ((not id) '())
        (invidious-url
         (embed-frame "video" (string-append invidious-url "/embed/" id)))
        (else
         (external-link (string-append "https://www.youtube.com/watch?v=" id)
                        "YouTube"))))

(define (embed-node? e)
  (and (pair? e)
       (symbol? (car e))
       (or (memq (car e) '(lite-tiktok lite-youtube iframe embed object))
           (and (eq? (car e) 'blockquote)
                (or (has-class? "tiktok-embed" e)
                    (has-class? "twitter-tweet" e)
                    (has-class? "twitter-tweet-lazy" e))))
       #t))

(define (embed-node->html e)
  (case (car e)
    ((lite-tiktok) (tiktok-embed (attr 'videoid e) e))
    ((lite-youtube) (youtube-embed (attr 'videoid e)))
    ;; Legacy plugin content (mostly dead Flash players): never let the
    ;; browser fetch it, just link to it.
    ((embed object)
     (let ((src (or (attr 'src e) (attr 'data e))))
       (if src (external-link src "external media") '())))
    ((iframe)
     (let ((src (or (attr 'data-src e) (attr 'src e) "")))
       (cond ((youtube-id src) => youtube-embed)
             ((string-null? src) '())
             ((string-contains src "trends.google.") (trends-link src))
             (else (external-link src "external media")))))
    (else
     (if (has-class? "tiktok-embed" e)
       (tiktok-embed (attr 'data-video-id e) e)
       (tweet-embed e)))))

(define (embed-replace nodeset)
  (modify-list nodeset embed-node? embed-node->html))

(define (script-kill nodeset)
  (modify-list
    nodeset (node-typeof? 'script)
    (lambda _ '())))

(define (style-kill nodeset)
  (modify-list
    nodeset (node-typeof? 'style)
    (lambda _ '())))

(define (clean-bodycopy bodycopy)
  (style-kill (script-kill (img-replace (embed-replace bodycopy)))))

(define (get-meme d)
  (define pref
    '(// article // (article (@ class (equal? "entry")))
      (div (@ class (equal? "desktop-only")))))
  (define bodycopy '())
  (define heading '())
  (define has-updated #f)

  (if (not (equal? '() ((sxpath '(html main article h4)) d)))
    (begin
      (set! pref '(html main article))
      (set! bodycopy
        (clean-bodycopy
          ((sxpath
             (append pref '(div (div (@ class (equal? "contents"))) (div 1))))
           d)))
      (set! heading
        (list
          (car (filter not-newline ((sxpath (append pref '(h1 *text*))) d)))
          (car ((sxpath (append pref '(img @ src-large *text*))) d))
          '() '()
          `(p "By "
              ,(car (filter
                      not-newline
                      ((sxpath
                         (append
                           pref '(div (a (@ class (equal? "author"))) *text*)))
                       d)))))))
    (begin
      (set! bodycopy
        (clean-bodycopy
          ((sxpath '(// (section (@ class (equal? "bodycopy"))))) d)))
      (set! has-updated
        (eqv? 2 ((sxpath (append pref '(header section div div p))) d)))
      (set! heading
        (list
          (car (filter not-newline
                       ((sxpath (append pref '(header section h1 *text*))) d)))
          (car ((sxpath (append pref '(header // img @ src *text*))) d))
          ((sxpath (append pref '(header section div h5))) d)
          (if has-updated
            '()
            ((sxpath (append pref '(header section div div (p 1)))) d))
          (if has-updated
            ((sxpath (append pref '(header section div div p))) d)
            ((sxpath (append pref '(header section div div (p 2)))) d))))))
  (define pred
    (lambda (nodeset)
      (equal? '()
              ((sxpath '(@ class (equal? "collection-item"))) nodeset))))
  (append
    (append
      heading
      (list (cdr
        ((sxml-filter
           (lambda (nodeset)
             (and
               (or (not ((node-typeof? 'div) nodeset))
                   (not (equal? '() ((sxpath '(div p))
                                     `(placeholder ,nodeset)))))
               (or (not ((node-typeof? 'h2) nodeset))
                   (equal? '() ((sxpath '(h2 @ id (equal? "search-interest")))
                                `(placeholder ,nodeset))))
               (or (not ((node-typeof? 'p) nodeset))
                   (equal? '() ((sxpath '(p br))
                                `(placeholder ,nodeset))))
               (not ((node-typeof? 'iframe) nodeset)))))
         ((sxml-filter pred) (cdar bodycopy))))))
    (list
      (map
        (lambda (node)
          (list
            ((sxpath '(h2)) node)
            ((sxpath '((p 1) a @ href *text*)) node)
            ((sxpath '(p a img @ src *text*)) node)
            ((sxpath '((p 3) a @ href *text*)) node)
            ((sxpath '((p 5) a @ href *text*)) node)))
        ((sxml-filter (lambda (n) (not (pred n)))) (cdar bodycopy))))))

;; Element children of NODE, without its attribute list.
(define (node-children node)
  (if (and (pair? (cdr node)) (pair? (cadr node)) (eq? (caadr node) '@))
    (cddr node)
    (cdr node)))

(define (by-id tag id d)
  (let ((r ((sxpath `(// (,tag (@ id (equal? ,id))))) d)))
    (if (null? r) #f (car r))))

;; htmlprag leaves character references in attribute values undecoded,
;; so titles taken from title=/alt=/data-title= would show "&#x27;".
(define (decode-entities s)
  (regexp-substitute/global
    #f "&(#x[0-9a-fA-F]+|#[0-9]+|amp|quot|apos|lt|gt);" s
    'pre
    (lambda (m)
      (let ((e (match:substring m 1)))
        (cond ((string=? e "amp") "&")
              ((string=? e "quot") "\"")
              ((string=? e "apos") "'")
              ((string=? e "lt") "<")
              ((string=? e "gt") ">")
              ((string-prefix? "#x" e)
               (string (integer->char (string->number (substring e 2) 16))))
              (else
               (string (integer->char (string->number (substring e 1))))))))
    'post))

;; Thumbnail grid items: (href title image) for every link of class
;; LINK-CLASS inside NODE that carries an image.
(define (grid-items node link-class title-proc)
  (filter (lambda (i) (and (car i) (caddr i)))
          (map (lambda (a)
                 (let ((img ((sxpath '(// img)) a)))
                   (list (attr 'href a)
                         (title-proc a)
                         (and (pair? img)
                              (or (attr 'data-src (car img))
                                  (attr 'src (car img)))))))
               (select 'a link-class node))))

;; KYM's "View Gallery" button of a media section, if any. Sensitive
;; galleries show only this button instead of thumbnails.
(define (gallery-link section)
  (let ((b (select-first 'a "button" section)))
    (and b (attr 'href b))))

;; Title of a thumbnail link: its image's title, else its alt text.
(define (img-title a)
  (let ((i ((sxpath '(// img)) a)))
    (if (null? i) ""
      (decode-entities (or (attr 'title (car i)) (attr 'alt (car i)) "")))))

;; Heading of a section: (title total), e.g. ("Recent Images" "145 total")
(define (section-heading h2)
  (if (not h2)
    (list "" "")
    (list (string-join (string-tokenize
                         (string-join (filter string? (node-children h2)) " "))
                       " ")
          (text-of ((sxpath '(small)) h2)))))

;; Thumbnail strips shown after an entry's body, as an alist with
;;   related  list of (title total items): related entries, sub-entries
;;   videos   (title total items gallery) of the recent videos strip, or #f
;;   photos   (title total items gallery) of the recent images strip, or #f
;; where items are (href title image) and gallery is KYM's "View Gallery"
;; link (the only content of galleries marked as sensitive), or #f.
(define (get-entry-extras d)
  (let ((videos (by-id 'section "videos" d))
        (photos (by-id 'section "photos" d)))
    `((related
       . ,(map (lambda (block)
                 (let ((h2 ((sxpath '(h2)) block)))
                   (append (section-heading (if (null? h2) #f (car h2)))
                           (list (grid-items block "photo" img-title)))))
               (select 'div "related_memes" d)))
      (videos
       . ,(and videos
               (let ((h2 ((sxpath '(h2)) videos)))
                 (append (section-heading (if (null? h2) #f (car h2)))
                         (list (grid-items videos "video"
                                           (lambda (a) (text-of ((sxpath '(// strong)) a))))
                               (gallery-link videos))))))
      (photos
       . ,(and photos
               (let ((h2 ((sxpath '(h2)) photos)))
                 (append (section-heading (if (null? h2) #f (car h2)))
                         (list (grid-items photos "photo" img-title)
                               (gallery-link photos)))))))))

(define (get-memes d)
  (define pref '(html body main article html head article section))
  (define pref2 '(// (section (@ class (equal? "gallery")))
                  (div (@ class (equal? "groups"))) a))
  (list
    ((sxpath (append pref '((div 1) h1 span *text*))) d)
    ((sxpath (append pref '((div 1) p))) d)
    (if (equal? ((sxpath (append pref '((div 2) (div 2)))) d) '())
      '()
      ((sxpath (append pref '((div 2) (div 1) (div 1) (div 1)))) d))
    (if (equal? ((sxpath (append pref '((div 2) (div 2)))) d) '())
      ((sxpath (append pref '((div 2) (div 1) (div 1) (div 2)))) d)
      ((sxpath (append pref '((div 2) (div 2) div (div 2)))) d))

    (map list
         ((sxpath (append pref2 '(@ href *text*))) d)
         ((sxpath (append pref2 '(div div (div 2) img @ src *text*))) d)
         ((sxpath (append pref2 '(@ data-title *text*))) d))))

(define (get-categories d)
  (define categories ((sxpath '(html body (div 2) div (div 3) a)) d))
  (define awful
    (map
      (lambda (n)
        (list
          ((sxpath '(@ href *text*)) n)
          ((sxpath '(@ data-category *text*)) n)
          ((sxpath '(@ data-type *text*)) n)
          ((sxpath '(span *text*)) n)))
      ((sxml-filter (select-kids (node-typeof? 'span))) categories)))
  (define cur '())
  (define subcats '())
  (define intermediate '())
  (define next-intermediate '())
  (define out '())
  (define out1
    (do ((i 0 (1+ i)))
        ((>= i (length awful))
         out)
      (set! cur (list-ref awful i))
      (if (equal? (caddr cur) '())
        (begin
          (set! intermediate next-intermediate)
          (set! next-intermediate
            (list (list (caar cur)
                        (caadr cur)
                        (car (cadddr cur)))))
          (set! intermediate (append intermediate (list subcats)))
          (set! out (append out (list intermediate)))
          (set! subcats '()))
        (set! subcats
          (append
            subcats
            (list (list (caar cur)
                        (caaddr cur)
                        (car (cadddr cur)))))))))
  (append
    ((sxpath '(html body div div div header hgroup h1)) d)
    (list (cdr out1))))

;; Image of a gallery item: the masonry thumbnail, decoded from
;; data-nsfw-src when KYM serves a cover image instead.
(define (gallery-image item)
  (let* ((box (or (select-first 'div "not-vertical-only" item) item))
         (imgs ((sxpath '(// img)) box))
         (url (cond ((null? imgs) #f)
                    ((attr 'data-nsfw-src (car imgs)) => decode-nsfw-src)
                    (else (attr 'src (car imgs))))))
    ;; Some search results carry only the NSFW cover in the masonry image;
    ;; the original is then left in the vertical variant's srcset-disabled.
    (if (and url (string-contains url "image-covers/nsfw"))
      (let ((real (filter (lambda (u) (not (string-contains u "image-covers")))
                          ((sxpath '(// img @ srcset-disabled *text*)) item))))
        (if (null? real) url (car real)))
      url)))

;; (heading tabs sorts items page total)
;;   heading  the <h1> node of the collection header, or #f
;;   tabs     view forms (All / Trending / Templates)
;;   sorts    sort forms (Newest / Oldest / ...), desktop variant only:
;;            the mobile variant is a <select> that needs JavaScript
;;   items    list of (href title image)
;;   page     current page number as reported by KYM
;;   total    total number of results, or #f if not shown
(define (get-photos d)
  (let* ((header (select-first 'div "collection-header" d))
         (h1 (if header ((sxpath '(h1)) header) '()))
         (bars (select-first 'div "bars" d))
         (tab-box (and bars (select-first 'div "scaleless" bars)))
         (sort-box (and bars (select-first 'div "desktop-only" bars)))
         (gallery (select-first 'section "gallery" d))
         (total-span (and header
                          (select-first 'span "pretty_total_results" header)))
         (total-digits (and total-span
                            (string-filter char-set:digit
                                           (text-of (list total-span))))))
    (list
      (if (null? h1) #f (car h1))
      (if tab-box ((sxpath '(form)) tab-box) '())
      (if sort-box ((sxpath '(form)) sort-box) '())
      (if gallery
        (filter caddr
                (map (lambda (a)
                       (list (or (attr 'href a) "")
                             (decode-entities (or (attr 'data-title a) ""))
                             (gallery-image a)))
                     (select 'a "item" gallery)))
        '())
      (or (and gallery (attr 'data-page gallery)
               (string->number (attr 'data-page gallery)))
          1)
      (and total-digits (string->number total-digits)))))

(define (get-photo d)
  (define pref  '(// (* (@ id (equal? "photo_wrapper"))) div a))
  (define pref2 '(// (* (@ id (equal? "content"))) (div 2) (div 2)))
  ;; (title alt src source views date)
  (list
    (car ((sxpath '(// (* (@ id (equal? "media-title"))))) d))
    (car ((sxpath (append pref '(// img @ alt *text*))) d))
    (car ((sxpath (append pref '(@ href *text*))) d))
    (car ((sxpath (append pref2 '((div (@ class (equal? "row")) 4) p a))) d))
    (car ((sxpath (append pref2 '((div (@ class (equal? "row")) 1) p))) d))
    (car ((sxpath (append pref2 '((div (@ class (equal? "row")) 2) p abbr)))
          d))))


;; Editorial article page:
;; (section title image authors published body tags)
;;   section    the <h4> holding the link to the editorial section, or #f
;;   title      article title
;;   image      header image URL, or #f
;;   authors    list of author <a> nodes (may be empty)
;;   published  publication date text, or ""
;;   body       cleaned body <div>
;;   tags       the tags <center> node, or #f
;; Returns #f when the page does not look like an article.
(define (get-article d)
  (let ((art (select-first 'article "article" d)))
    (and art
      (let* ((h4 ((sxpath '(h4)) art))
             (img (select-first 'img "main-image" art))
             (byline (select-first 'div "blue-links" art))
             (date (select-first 'span "date-info" art))
             (body (select-first 'div "body" art)))
        (list
          (if (null? h4) #f (car h4))
          (text-of (select 'h1 "content-title" art))
          (and img (or (attr 'src img) (attr 'src-large img)))
          (if byline ((sxpath '(// a)) byline) '())
          (if date (text-of (list date)) "")
          (if body (car (clean-bodycopy (list body))) '(div))
          (select-first 'center "tags" art))))))

(define (get-pagination d)
  (if (not (equal? '() ((sxpath '(// (* (@ class (equal? "pager"))))) d)))
    (append
      (list (car ((sxpath '(// (* (@ class (equal? "pager"))) li)) d)))
      ((sxpath '(// (* (@ class (equal? "pager"))) span li)) d)
      (list (cadr ((sxpath '(// (* (@ class (equal? "pager"))) li)) d))))
    '()))

(define (get-title d)
  (if (string? d) d
    (string-drop-right
      (car (filter not-newline ((sxpath '(html head title *text*)) d)))
      18)))
