#! /usr/bin/env chibi-scheme

(import (scheme small)
        (srfi 69)
        (srfi 95)
        (srfi 130)
        (chibi html-parser))

(define (disp . xs)
  (let loop ((xs xs)) (unless (null? xs) (display (car xs)) (loop (cdr xs))))
  (newline))

(define (writeln x)
  (write x)
  (newline))

(define (assoc-get get key alist)
  (let ((pair (assoc key alist)))
    (if pair (get pair) #f)))

(define (lenient-assoc key list_)
  (cond ((null? list_) #f)
        ((and (pair? (car list_))
              (equal? key (caar list_)))
         (car list_))
        (else (lenient-assoc key (cdr list_)))))

(define (sxml-for-each proc elem)
  (let walk ((elem elem))
    (cond ((not (pair? elem)) '())
          ((equal? '@ (car elem)) '())
          (else (proc elem)
                (for-each walk (cdr elem))))))

(define (sxml-attributes elem)
  (if (and (pair? elem)
           (pair? (cdr elem))
           (pair? (cadr elem))
           (equal? '@ (caadr elem)))
      (cdadr elem)
      '()))

(define (missing-a-names html-file)
  (let ((names (make-hash-table))
        (hrefs (make-hash-table)))
    (sxml-for-each (lambda (elem)
                     (when (equal? 'a (car elem))
                       (let ((href (assoc-get cadr 'href (sxml-attributes elem)))
                             (name (assoc-get cadr 'name (sxml-attributes elem))))
                         (when name
                           (hash-table-set! names name #t))
                         (when (and href (string-prefix? "#" href))
                           (hash-table-set! hrefs (substring href 1) #t)))))
                   (call-with-input-file html-file html->sxml))
    (let loop ((hrefs (hash-table-keys hrefs)) (missing-names '()))
      (if (null? hrefs)
          (sort missing-names string<? string-downcase)
          (loop (cdr hrefs)
                (if (hash-table-exists? names (car hrefs))
                    missing-names
                    (cons (car hrefs) missing-names)))))))

(define (handle-file html-file)
  (disp "Missing <a name=\"...\"> attribytes in " html-file ":")
  (let ((names (missing-a-names html-file)))
    (for-each (lambda (name) (disp "  " name))
              (if (null? names) '("(none)") names))
    (newline)))

(define (main)
  (for-each handle-file (cdr (command-line))))

(main)
