(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (srfi 1)
        (srfi 13)
        (chibi html-parser)
        (chibi sxml))

(cond-expand (gauche (import (gauche base)
                             (gauche process)))
             (else))

(define (deb label x)
  (display label (current-error-port))
  (display " => " (current-error-port))
  (write x (current-error-port))
  (newline (current-error-port))
  x)

(define (find-html-tag sxml)
  (and (pair? sxml)
       (or (and (pair? (car sxml))
                (eqv? 'html (caar sxml))
                (car sxml))
           (find-html-tag (cdr sxml)))))

(define (tag-with-attrs? elem)
  (and (pair? (cdr elem))
       (pair? (cadr elem))
       (eqv? '@ (caadr elem))))

(define (parse-tag elem)
  (if (or (not (pair? elem))
          (not (symbol? (car elem)))
          (eqv? '@ (car elem)))
      (values #f #f #f)
      (let ((name (car elem)))
        (set! elem (cdr elem))
        (let ((attrs (and (pair? elem)
                          (pair? (car elem))
                          (eqv? '@ (caar elem))
                          (cdar elem))))
          (when attrs (set! elem (cdr elem)))
          (let ((body elem))
            (values name attrs body))))))

(define (make-tag name attrs body)
  (if (and attrs (not (null? attrs)))
      (cons* name (cons '@ attrs) body)
      (cons name body)))

(define (remove-subtags body . tag-names)
  (filter (lambda (elem)
            (let-values (((name attrs body) (parse-tag elem)))
              (not (and name (member name tag-names)))))
          body))

(define (remove-subtag-ids body . ids)
  (filter (lambda (elem)
            (let-values (((name attrs body) (parse-tag elem)))
              (not (and attrs (let* ((pair (assoc 'id attrs))
                                     (value (and pair (cadr pair))))
                                (and value (member value ids)))))))
          body))

(define (flatten-subtags body . tag-names)
  (let loop ((oldbody body) (newbody '()))
    (if (null? oldbody)
        newbody
        (let-values (((name attrs elembody) (parse-tag (car oldbody))))
          (if (and name (member name tag-names))
              (loop (append elembody (cdr oldbody))
                    (append newbody (list (make-tag name attrs '()))))
              (loop (cdr oldbody)
                    (append newbody (list (car oldbody)))))))))

(define (sxml-cleanup elem)
  (let-values (((name attrs body) (parse-tag elem)))
    (case name
      ((#f)
       elem)
      ((head)
       (make-tag name #f
                 (map sxml-cleanup
                      (remove-subtags
                       (flatten-subtags body 'meta)
                       'meta 'link 'style))))
      (else
       (make-tag name
                 (and attrs
                      (filter (lambda (attr) (eqv? 'href (car attr)))
                              attrs))
                 (map sxml-cleanup (remove-subtags
                                    (flatten-subtags
                                     (remove-subtag-ids body "footer")
                                     'div)
                                    'div)))))))

(define (run-pipe input command . args)
  (cond-expand
    (gauche (call-with-process-io (cons command args)
                                  (lambda (i o)
                                    (display input o)
                                    (close-output-port o)
                                    (port->string i))))))

(define (handle-file asciidoc-file)
  (unless (string-suffix? ".adoc" asciidoc-file)
    (error "Input file name extension is not .adoc"))
  (let ((html-file (string-append (substring asciidoc-file 0
                                             (- (string-length asciidoc-file)
                                                (string-length ".adoc")))
                                  ".html")))
    (for-each display (list "Converting " asciidoc-file " to " html-file))
    (newline)
    (let ((asciidoc (call-with-input-file asciidoc-file port->string)))
      (let ((html (run-pipe asciidoc
                            "asciidoctor" "-b" "html5" "-o" "-" "-")))
        (let ((sxml (call-with-input-string html html->sxml)))
          (set! sxml (sxml-cleanup (find-html-tag sxml)))
          (set! html (call-with-output-string
                       (lambda (out)
                         (write-string "<!doctype html>" out)
                         (sxml-display-as-html sxml out))))
          (set! html (run-pipe html
                               "tidy" "-q" "--tidy-mark" "no" "-indent"))
          (call-with-output-file html-file
            (lambda (out) (write-string html out))))))))

(define (main args)
  (for-each handle-file (cdr args)))

(main (command-line))
