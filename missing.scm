#! /usr/bin/env chibi-scheme

(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (srfi 69)
        (srfi 132)
        (chibi html-parser))

(define (disp . xs)
  (for-each display xs)
  (newline))

(define (string-first-char? ch string)
  (and (not (= 0 (string-length string)))
       (char=? ch (string-ref string 0))))

(define (assoc-get get key alist)
  (let ((pair (assoc key alist)))
    (if pair (get pair) #f)))

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
    (sxml-for-each
     (lambda (elem)
       (when (equal? 'a (car elem))
         (let ((href (assoc-get cadr 'href (sxml-attributes elem)))
               (name (assoc-get cadr 'name (sxml-attributes elem))))
           (when name
             (hash-table-set! names name #t))
           (when (and href (string-first-char? #\# href))
             (hash-table-set!
              hrefs (substring href 1 (string-length href)) #t)))))
     (call-with-input-file html-file html->sxml))
    (let loop ((hrefs (hash-table-keys hrefs)) (missing-names '()))
      (if (null? hrefs)
          (list-sort (lambda (a b) (string<? (string-downcase a)
                                             (string-downcase b)))
                     missing-names)
          (loop (cdr hrefs)
                (if (hash-table-exists? names (car hrefs))
                    missing-names
                    (cons (car hrefs) missing-names)))))))

(define (handle-file html-file)
  (disp "Missing <a name=\"...\"> attributes in " html-file ":")
  (let ((names (missing-a-names html-file)))
    (for-each (lambda (name) (disp "  " name))
              (if (null? names) '("(none)") names))
    (newline)))

(define (main)
  (newline)
  (for-each handle-file (cdr (command-line))))

(main)
