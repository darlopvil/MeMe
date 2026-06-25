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

(define-module (meme scraping)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:use-module ((sxml xpath)
                #:renamer (lambda (p)
                            (if (eq? 'filter p)
                              'sxml-filter
                              p)))
  #:export (proxy
            get-editorials
            get-meme
            get-memes
            get-categories
            get-photos
            get-photo
            get-pagination
            get-title))

(define (proxy url)
  (string-append "/proxy?url=" url))

(define (not-newline s)
  (not (equal? "\n" s)))

(define (get-editorials d)
  (define pref
    '(html main article article div (div 1) div div section ol li a))
  (map list
       ((sxpath (append pref '(@   href      *text*))) d)
       (filter not-newline ((sxpath (append pref '(div h3 *text*))) d))
       ((sxpath (append pref '(div img @ src *text*))) d)
       (filter not-newline ((sxpath (append pref '(div p  *text*))) d))))


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

(define (img-replace nodeset)
  (modify-list
    nodeset (node-typeof? 'img)
    (lambda (e)
      `(img (@ (src ,(proxy
                       (car (append
                              ((sxpath '(@ data-src *text*)) e)
                              (map (lambda (s)
                                     (utf8->string
                                       (base64-decode s base64-alphabet #f #f)))
                                   ((sxpath '(@ data-nsfw-src *text*)) e))))))
               (loading "lazy"))))))

(define (script-kill nodeset)
  (modify-list
    nodeset (node-typeof? 'script)
    (lambda _ '())))

(define (style-kill nodeset)
  (modify-list
    nodeset (node-typeof? 'style)
    (lambda _ '())))

(define (clean-bodycopy bodycopy)
  (style-kill (script-kill (img-replace bodycopy))))

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

(define (get-photos d)
  (define pref '(html main article html head article section))
  (list
    ((sxpath (append pref '((div 1) h1 span *text*))) d)
    (car ((sxpath (append pref '((div 2) (div 1) (div 1) div))) d))
    (let ((form ((sxpath (append pref '((div 2) (div 2) div (div 2)))) d)))
      (if (equal? form '()) '() (car form)))
    (map cons
         ((sxpath (append pref '((div 3) div section div a @ href *text*))) d)
         ((sxpath (append pref '((div 3) div section div a div (div 1) (div 2)
                                 img @ src *text*))) d))))

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
