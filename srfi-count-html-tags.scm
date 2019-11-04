;;; Display how many times each HTML tag is used in the given SRFIs.

(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (srfi 1)
        (srfi 69)
        (srfi 95))

(cond-expand (chibi  (import (chibi io) (chibi string)))
             (gauche (import (srfi 13) (gauche base))))

(import (chibi html-parser))

(define make-set make-hash-table)
(define set-elems hash-table-keys)
(define (add-to-set elem set) (hash-table-set! set elem #t) set)
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (symbol-prefix? prefix sym)
  (string-prefix? prefix (symbol->string sym)))

(define (displayln . xs)
  (for-each display xs)
  (newline))

(define (tag-body elem)
  (cond ((not (pair? (cdr elem))) '())
        ((and (pair? (cadr elem)) (eqv? '@ (caadr elem)))
         (cddr elem))
        (else (cdr elem))))

(define (tag-names-fold elem kons knil)
  (let do-elem ((elem elem) (acc knil))
    (if (not (pair? elem)) acc
        (let do-list ((elems (tag-body elem)) (acc (kons (car elem) acc)))
          (if (null? elems) acc
              (do-list (cdr elems) (do-elem (car elems) acc)))))))

(define (html-file-tag-names html-file)
  (let* ((html (call-with-input-file html-file port->string))
         (sxml (call-with-input-string html html->sxml))
         (tags (tag-names-fold sxml add-to-set (make-set))))
    (filter (lambda (tag) (not (symbol-prefix? "*" tag)))
            (sort (set-elems tags) symbol<?))))

(define (main html-files)
  (let ((tag-counts (make-hash-table)))
    (for-each (lambda (html-file)
                (for-each (lambda (tag-name)
                            (hash-table-update!/default
                             tag-counts tag-name (lambda (x) (+ x 1)) 0))
                          (html-file-tag-names html-file)))
              html-files)
    (for-each (lambda (tag-name)
                (let ((count (hash-table-ref tag-counts tag-name)))
                  (displayln (list tag-name count))))
              (sort (hash-table-keys tag-counts)
                    (lambda (tag-a tag-b)
                      (> (hash-table-ref tag-counts tag-a)
                         (hash-table-ref tag-counts tag-b)))))))

(main (cdr (command-line)))
