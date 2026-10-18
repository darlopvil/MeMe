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
  #:use-module ((htmlprag) #:select (html->shtml))
  #:use-module ((sxml xpath)
                #:renamer (lambda (p)
                            (if (eq? 'filter p)
                              'sxml-filter
                              p)))
  #:export (proxy
            proxyable?
            https-upgrade
            frontend-href
            get-editorials
            get-meme
            get-entry-extras
            get-video
            get-memes
            get-categories
            get-photos
            get-photo
            get-profile
            get-uploader
            get-comment-count
            get-comments
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
    "https://i.ytimg.com/" "https://knowyourmeme.com/"
    ;; GIFs posted in comments and entries
    "https://media.tenor.com/" "https://c.tenor.com/"))

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
(define eddrit-url    (frontend-url "EDDRIT_URL"))
(define soprano-url   (frontend-url "SOPRANO_URL"))
(define priviblur-url (frontend-url "PRIVIBLUR_URL"))

;; Priviblur URL for a Tumblr HOST and REST (path and query), or #f.
;; Same mapping as Priviblur's own url_handler: media and asset hosts go
;; through its /tblr proxy, <blog>.tumblr.com/post/<id> becomes
;; /<blog>/<id>, and www.tumblr.com paths are kept as they are.
(define (tumblr->priviblur host rest)
  (and priviblur-url
       (or (equal? host "tumblr.com") (string-suffix? ".tumblr.com" host))
       (let ((parts (string-split host #\.)))
         (string-append
           priviblur-url
           (cond ((and (= (length parts) 4) (equal? (cadr parts) "media"))
                  (string-append "/tblr/media/" (car parts) rest))
                 ((equal? host "assets.tumblr.com") (string-append "/tblr/assets" rest))
                 ((equal? host "static.tumblr.com") (string-append "/tblr/static" rest))
                 ((equal? host "a.tumblr.com") (string-append "/tblr/a" rest))
                 ((equal? (car parts) "tumblr") rest)
                 ((string-prefix? "/post" rest)
                  (string-append "/" (car parts) (substring rest 5)))
                 (else (string-append "/" (car parts) rest)))))))

;; Where a link to a third-party site should point: the matching privacy
;; frontend when one is configured, otherwise the original URL. Only the
;; host changes; frontends mirror the original paths.
(define (frontend-href url)
  (let ((m (string-match "^https?://([^/?#]+)(.*)$" url)))
    (if (not m)
      url
      (let* ((host (string-downcase (match:substring m 1)))
             (host (if (string-prefix? "www." host) (substring host 4) host))
             (rest (match:substring m 2))
             (rest (if (string-null? rest) "/" rest)))
        (cond
          ((and nitter-url
                (member host '("twitter.com" "x.com" "mobile.twitter.com"
                               "mobile.x.com")))
           (string-append nitter-url rest))
          ((and invidious-url
                (member host '("youtube.com" "m.youtube.com"
                               "music.youtube.com")))
           (string-append invidious-url rest))
          ((and invidious-url (equal? host "youtu.be"))
           ;; youtu.be/<id>?t=1 -> /watch?v=<id>&t=1
           (let ((v (string-match "^/([A-Za-z0-9_-]+)[?]?(.*)$" rest)))
             (if v
               (string-append invidious-url "/watch?v=" (match:substring v 1)
                              (if (string-null? (match:substring v 2))
                                ""
                                (string-append "&" (match:substring v 2))))
               url)))
          ((and eddrit-url
                (member host '("reddit.com" "old.reddit.com" "new.reddit.com"
                               "np.reddit.com")))
           ;; eddrit serves users under /user/, not /u/
           (string-append eddrit-url
                          (if (string-prefix? "/u/" rest)
                            (string-append "/user/" (substring rest 3))
                            rest)))
          ((and proxitok-url (member host '("tiktok.com" "m.tiktok.com")))
           (string-append proxitok-url rest))
          ((and rimgo-url (member host '("imgur.com" "i.imgur.com" "m.imgur.com")))
           (string-append rimgo-url rest))
          ((and soprano-url (equal? host "tenor.com")
                (string-prefix? "/view/" rest))
           (string-append soprano-url rest))
          ((tumblr->priviblur host rest) => identity)
          (else url))))))

;; Tumblr-hosted image -> the same image through Priviblur, or #f
(define (tumblr-image->priviblur url)
  (and priviblur-url
       (string-match "^https?://[a-z0-9]+\\.media\\.tumblr\\.com/" url)
       (frontend-href url)))

;; Point links in page content at the configured frontends, and drop
;; KYM's JavaScript-only controls ("click to expand" and the like).
(define (link-replace nodeset)
  (modify-list
    nodeset (node-typeof? 'a)
    (lambda (e)
      (let* ((href (attr 'href e))
             (new (and href (frontend-href href))))
        (cond
          ((and href (string-prefix? "javascript:" (string-trim href))) '())
          ((or (not href) (equal? new href)) e)
          (else
           (let ((attrs (if (and (pair? (cdr e)) (pair? (cadr e))
                                 (eq? (caadr e) '@))
                          (cdadr e)
                          '())))
             `(a (@ (href ,new)
                    ,@(filter (lambda (a)
                                (not (and (pair? a) (eq? (car a) 'href))))
                              attrs))
                 ,@(node-children e)))))))))

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
              ((or (imgur->rimgo url) (tumblr-image->priviblur url))
               => (lambda (src)
                    `(img (@ (src ,src) (alt ,alt) (loading "lazy")))))
              (else (external-link url "external media")))))))

;; Embeds. KYM ships TikToks as <lite-tiktok> / blockquote.tiktok-embed,
;; tweets as blockquote.twitter-tweet(-lazy), video tweets as
;; blockquote.twitter-video, Tumblr posts as div.tumblr-post, and YouTube as
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

;; Tumblr posts: div.tumblr-post, turned into an iframe by Tumblr's
;; post.js. The blog and post id come from the link inside it
;; (<blog>.tumblr.com/post/<id>/... or www.tumblr.com/<blog>/<id>).
;; Priviblur serves a compact, script-free view at /<blog>/<id>/embed;
;; theme=dark matches MeMe. Without Priviblur the div stays as it is, and
;; its link is rewritten like any other.
(define (tumblr-post-target node)
  (let loop ((hrefs ((sxpath '(// a @ href *text*)) node)))
    (if (null? hrefs)
      #f
      (let ((m (or (string-match
                     "^https?://(www\\.)?([a-z0-9-]+)\\.tumblr\\.com/post/([0-9]+)"
                     (car hrefs))
                   (string-match
                     "^https?://(www\\.)?tumblr\\.com/([a-z0-9-]+)/([0-9]+)"
                     (car hrefs)))))
        (if (and m (not (equal? (match:substring m 2) "www")))
          (cons (match:substring m 2) (match:substring m 3))
          (loop (cdr hrefs)))))))

(define (tumblr-embed node)
  (let ((target (tumblr-post-target node)))
    (if (and priviblur-url target)
      (embed-frame "tumblr"
                   (string-append priviblur-url "/" (car target) "/" (cdr target)
                                  "/embed?theme=dark"))
      node)))

(define (embed-node? e)
  (and (pair? e)
       (symbol? (car e))
       (or (memq (car e) '(lite-tiktok lite-youtube iframe embed object))
           (and (eq? (car e) 'div) (has-class? "tumblr-post" e))
           (and (eq? (car e) 'blockquote)
                (or (has-class? "tiktok-embed" e)
                    (has-class? "twitter-tweet" e)
                    (has-class? "twitter-tweet-lazy" e)
                    (has-class? "twitter-video" e))))
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
    ((div) (tumblr-embed e))
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
  (link-replace
    (style-kill (script-kill (img-replace (embed-replace bodycopy))))))

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

;; Single video page: an alist with
;;   entry     (href title) of the entry the video belongs to, or #f
;;   title     video title
;;   player    the embed (Invidious / Nitter / ProxiTok frame, or a link
;;             to the source when there is no usable embed), or #f
;;   previous  href of the previous video in the gallery, or #f
;;   next      href of the next video, or #f
;;   gallery   href of the entry's video gallery, or #f
;; Returns #f when the page does not look like a video page.
(define (get-video d)
  (let ((art (select-first 'article "media" d)))
    (and art
      (let* ((header (select-first 'div "collection-header" art))
             (entry-a (and header (let ((a ((sxpath '(a)) header)))
                                    (if (null? a) #f (car a)))))
             (h2 ((sxpath '(// h2)) art))
             (contents (select-first 'div "contents" art))
             (embeds (if contents
                       (filter embed-node? ((sxpath '(// *)) contents))
                       '()))
             (info (select-first 'span "date-info" art))
             (source (and info (attr 'data-via-link info)))
             (link-of (lambda (id)
                        (let ((a (by-id 'a id art)))
                          (and a (attr 'href a))))))
        `((entry . ,(and entry-a
                         (list (attr 'href entry-a) (text-of (list entry-a)))))
          (title . ,(if (null? h2) "" (text-of (list (car h2)))))
          (player . ,(cond ((pair? embeds) (embed-node->html (car embeds)))
                           (source (external-link source "source"))
                           (else #f)))
          (previous . ,(link-of "previous-link"))
          (next . ,(link-of "next-link"))
          (gallery . ,(link-of "gallery-link")))))))

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

;; (title alt src tags views date notes)
;;   tags   list of tag links (may be empty)
;;   views  "N views" paragraph, or '()
;;   date   upload date <abbr>, or '()
;;   notes  cleaned "Notes" nodes (often the source link), or #f
;; Only the image itself is required; images without tags, notes or an
;; alt text are common.
(define (get-photo d)
  (define pref  '(// (* (@ id (equal? "photo_wrapper"))) div a))
  (define pref2 '(// (* (@ id (equal? "content"))) (div 2) (div 2)))
  (define (first-or l default) (if (null? l) default (car l)))
  (list
    (first-or ((sxpath '(// (* (@ id (equal? "media-title"))))) d) '(h1 "Image"))
    (first-or ((sxpath (append pref '(// img @ alt *text*))) d) "")
    (car ((sxpath (append pref '(@ href *text*))) d))
    ((sxpath (append pref2 '((div (@ class (equal? "row")) 4) p a))) d)
    (first-or ((sxpath (append pref2 '((div (@ class (equal? "row")) 1) p))) d)
              '())
    (first-or ((sxpath (append pref2 '((div (@ class (equal? "row")) 2) p abbr)))
               d)
              '())
    (let ((notes (select-first 'div "media-notes" d)))
      (and notes
           (let ((body (clean-bodycopy
                         ;; minus KYM's own "Notes" headings
                         (filter (lambda (n)
                                   (not (and (pair? n)
                                             (or (eq? (car n) 'h6)
                                                 (and (eq? (car n) 'p)
                                                      (equal? (text-of (list n))
                                                              "Notes:"))))))
                                 (node-children notes)))))
             (and (not (string-null? (text-of body))) body))))))

;; User profile (the "wall" page), as an alist:
;;   name           display name
;;   role           e.g. "Sleuth", or #f
;;   avatar         avatar image URL, or #f
;;   location       "Location: ..." text, or #f
;;   joined         "Joined <date>" text, or #f
;;   contributions  "13 images, 1 video, ..." text, or #f
;;   links          ((label . href) ...): social media profiles, websites
;;   about          cleaned nodes of the user's "About" text, or #f
;;   tabs           ((label . href) ...): the Images and Videos tabs
;; The rest of the page (wall comments, karma, rankings, followers) is
;; loaded by JavaScript or not worth showing. Returns #f when the page
;; does not look like a profile.
(define (get-profile d)
  (let ((bio (by-id 'div "profile_bio" d)))
    (and bio
      (let* ((photo (by-id 'div "profile_photo" d))
             (avatar (and photo
                          (let ((i ((sxpath '(// img)) photo)))
                            (and (pair? i) (img-source (car i))))))
             (role (select-first 'h5 "role" bio))
             (location (filter (lambda (p)
                                 (string-prefix? "Location:"
                                                 (text-of (list p))))
                               ((sxpath '(p)) bio)))
             (joined (select-first 'p "joined" bio))
             (contribs (filter (lambda (s) (not (string-null? s)))
                               (map (lambda (p) (text-of (list p)))
                                    (append
                                      (select 'p "contribs_comments" bio)
                                      (select 'p "contribs_edits" bio)))))
             ;; <dl><dt>Website</dt><dd>...</dd><dt>About</dt>... directly
             ;; under the bio (the karma <dl>s live inside a table).
             (fields (append-map*
                       (lambda (dl)
                         (let loop ((dts ((sxpath '(dt)) dl))
                                    (dds ((sxpath '(dd)) dl))
                                    (acc '()))
                           (if (or (null? dts) (null? dds))
                             (reverse acc)
                             (loop (cdr dts) (cdr dds)
                                   (cons (cons (text-of (list (car dts)))
                                               (car dds))
                                         acc)))))
                       ((sxpath '(dl)) bio)))
             (social (by-id 'div "profile-social-media-profiles" d))
             (social-links
               (if social
                 (map (lambda (a)
                        (cons (or (attr 'alt (car ((sxpath '(img)) a)))
                                  (attr 'href a))
                              (attr 'href a)))
                      (filter (lambda (a)
                                (and (attr 'href a)
                                     (pair? ((sxpath '(img)) a))))
                              ((sxpath '(// a)) social)))
                 '()))
             (website-links
               (let ((dd (assoc-ref fields "Website")))
                 (if dd
                   (filter cdr
                           (map (lambda (a)
                                  (cons (text-of (list a)) (attr 'href a)))
                                ((sxpath '(// a)) dd)))
                   '())))
             (about (assoc-ref fields "About"))
             (tabs (by-id 'ul "profile-tabs" d)))
        `((name . ,(text-of ((sxpath '(h1)) bio)))
          (role . ,(and role (text-of (list role))))
          (avatar . ,avatar)
          (location . ,(and (pair? location) (text-of location)))
          (joined . ,(and joined (text-of (list joined))))
          (contributions . ,(and (pair? contribs)
                                 (string-join contribs ", ")))
          (links . ,(append social-links website-links))
          (about . ,(and about (clean-bodycopy (node-children about))))
          (tabs . ,(if tabs
                     (filter (lambda (t)
                               (or (string-suffix? "/photos" (cdr t))
                                   (string-suffix? "/videos" (cdr t))))
                             (map (lambda (a)
                                    (cons (text-of (list a))
                                          (or (attr 'href a) "")))
                                  ((sxpath '(li a)) tabs)))
                     '())))))))


;; Undo JSON string escapes: \" \\ \/ \n \t and \uXXXX.
(define (json-unescape s)
  (let loop ((cs (string->list s)) (acc '()))
    (cond ((null? cs) (list->string (reverse acc)))
          ((and (char=? (car cs) #\\) (pair? (cdr cs)))
           (let ((c (cadr cs)))
             (case c
               ((#\n) (loop (cddr cs) (cons #\newline acc)))
               ((#\t) (loop (cddr cs) (cons #\tab acc)))
               ((#\u)
                (let* ((hex (and (>= (length cs) 6)
                                 (list->string (list-head (cddr cs) 4))))
                       (n (and hex (string->number hex 16))))
                  (if n
                    (loop (list-tail cs 6) (cons (integer->char n) acc))
                    (loop (cddr cs) (cons c acc)))))
               (else (loop (cddr cs) (cons c acc))))))
          (else (loop (cdr cs) (cons (car cs) acc))))))

;; (name . url) of whoever added an image, video or entry, or #f. KYM
;; renders the "Uploaded by" line with JavaScript, but every page carries
;; the author in its schema.org metadata, which is steadier than markup.
(define (get-uploader d)
  (let loop ((scripts (filter (lambda (s)
                                (equal? (attr 'type s) "application/ld+json"))
                              ((sxpath '(// script)) d))))
    (if (null? scripts)
      #f
      (let ((m (string-match
                 (string-append "\"author\":\\{\"@type\":\"Person\","
                                "\"name\":\"((\\\\.|[^\"\\\\])*)\","
                                "\"url\":\"([^\"]*)\"")
                 (text-of* (car scripts)))))
        (if m
          (cons (json-unescape (match:substring m 1))
                (json-unescape (match:substring m 3)))
          (loop (cdr scripts)))))))

;; Raw text content of NODE (no whitespace squashing, unlike text-of).
(define (text-of* node)
  (apply string-append (filter string? ((sxpath '(// *text*)) node))))

;; N in the "Comments (N)" heading of a page's comment section, or 0.
;; Only the section itself: video pages repeat a comments_count span in
;; their toolbar.
(define (get-comment-count d)
  (let* ((section (by-id 'div "comments" d))
         (span (and section (select-first 'span "comments_count" section)))
         (digits (and span (string-filter char-set:digit
                                          (text-of (list span))))))
    (or (and digits (string->number digits)) 0)))

;; Comment HTML is written by users, so after the usual cleanup (images
;; through the proxy, embeds through the frontends, no scripts) only a
;; whitelist of inline/text elements and attributes survives. Unknown
;; elements are unwrapped, dangerous ones dropped with their content.
(define comment-tags
  '(p br a i em b strong u s del ins blockquote q ul ol li code pre span
    sub sup small h3 h4 h5 h6 hr img figure iframe))
(define comment-attrs
  '((a href class) (img src alt loading)
    (iframe src loading frameborder allowfullscreen) (figure class)))
(define comment-dropped
  '(script style form input button textarea select option object embed
    svg math head title meta link noscript template frame frameset))

(define (safe-href? h)
  (or (string-prefix? "/" h) (string-prefix? "#" h)
      (string-prefix? "https://" h) (string-prefix? "http://" h)))

(define (sanitize-comment-node n)
  (cond ((string? n) (list n))
        ((not (pair? n)) '())
        ((eq? (car n) '*ENTITY*) (list n))
        ((eq? (car n) '*TOP*) (append-map* sanitize-comment-node (cdr n)))
        ((not (symbol? (car n))) '())
        ((memq (car n) '(@ *COMMENT* *PI* *DECL*)) '())
        ((memq (car n) comment-dropped) '())
        ((memq (car n) comment-tags)
         (let* ((keep (or (assq-ref comment-attrs (car n)) '()))
                (given (if (and (pair? (cdr n)) (pair? (cadr n))
                                (eq? (caadr n) '@))
                         (cdadr n)
                         '()))
                (attrs (filter (lambda (a)
                                 (and (pair? a) (memq (car a) keep)
                                      (pair? (cdr a)) (string? (cadr a))
                                      (or (not (eq? (car a) 'href))
                                          (safe-href? (cadr a)))))
                               given))
                (attrs (if (and (eq? (car n) 'a) (assq 'href attrs)
                                (string-prefix? "http" (cadr (assq 'href attrs))))
                         (append attrs '((rel "nofollow noopener noreferrer")))
                         attrs)))
           (list `(,(car n) ,@(if (null? attrs) '() `((@ ,@attrs)))
                   ,@(append-map* sanitize-comment-node (node-children n))))))
        (else (append-map* sanitize-comment-node (node-children n)))))

;; The JSON "comment" field is HTML escaped once more ("&lt;p&gt;..."):
;; parsing it yields the real HTML as text, which is parsed again.
(define (comment-body escaped)
  (let* ((html (apply string-append
                      (map (lambda (n)
                             (cond ((string? n) n)
                                   ((and (pair? n) (eq? (car n) '*ENTITY*))
                                    (string-append "&" (caddr n) ";"))
                                   (else "")))
                           (cdr (html->shtml escaped)))))
         (nodes (cdr (html->shtml html))))
    (append-map* sanitize-comment-node (clean-bodycopy nodes))))

;; KYM's comments.json, as parsed by guile-json, into
;; (total . comments), each comment an alist with id, parent, username,
;; user-link, icon, created, likes, dislikes, nsfw? and body (nodes).
(define (get-comments data)
  (define (field c k) (let ((v (assoc-ref c k))) (if (eq? v 'null) #f v)))
  (let ((comments (or (assoc-ref data "comments") #())))
    (cons (or (assoc-ref data "total_count") 0)
          (map (lambda (c)
                 (let ((icon (or (field c "user_icon") "")))
                   `((id . ,(field c "id"))
                     (parent . ,(field c "parent_id"))
                     (username . ,(or (field c "username") "?"))
                     (user-link . ,(field c "user_link"))
                     (icon . ,(cond ((string-prefix? "/" icon)
                                     (string-append "https://knowyourmeme.com" icon))
                                    ((string-null? icon) #f)
                                    (else icon)))
                     (created . ,(or (field c "created_at") ""))
                     (likes . ,(or (field c "likes") 0))
                     (dislikes . ,(or (field c "dislikes") 0))
                     (nsfw? . ,(eq? (field c "nsfw") #t))
                     (body . ,(comment-body (or (field c "comment") ""))))))
               (vector->list comments)))))

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

;; Page title without KYM's " | Know Your Meme" suffix. The title text
;; may be split in several nodes and wrapped in newlines, and some pages
;; (user profiles) have no trailing newline, so never drop a fixed count.
(define (get-title d)
  (if (string? d) d
    (let ((t (string-trim-both
               (apply string-append
                      (filter string?
                              ((sxpath '(html head title *text*)) d)))))
          (suffix " | Know Your Meme"))
      (if (string-suffix? suffix t)
        (string-drop-right t (string-length suffix))
        t))))
