#lang racket/base


(require (only-in racket/match
                  match
                  )
         (only-in racket/string
                  string-replace
                  string-split
                  string-join
                  string-prefix?
                  )
         (only-in racket/list
                  empty?
                  takef
                  splitf-at
                  )
         (only-in racket/file
                  file->lines
                  )
         (only-in racket/cmdline
                  command-line
                  )
         (only-in xml
                  xexpr->xml
                  write-xml/content
                  )
         )


(provide file->xexpr 
         )



(define symbol-replace-map
  '(("(" . " ( ") (")" . " ) ")
    ("[" . " ( ") ("]" . " ) ")
    ("{" . " ( ") ("}" . " ) ")
    ("#" . " # ")
    ("##" . " ## ")
    ("###" . " ### ")
    ("####" . " #### ")
    ("#####" . " ##### ")
    ("_" . " _ ") ("-" . " - ")
    ("@" . " @ ") ("*" . " * ")
    ("`" . " ` ") ("!" . " ! ")
    ))

(define (pclose? x) (string=? x ")"))
(define notpclose? (compose not pclose?))
(define (emptystr? x) (string=? x ""))
(define notemptystr? (compose not emptystr?))
(define notstring? (compose not string?))
(define (hyperlink? x)
  (string-prefix? (string-downcase x) "http"))

(define (split-grab datum str)
  (define-values (left right)
    (splitf-at datum (λ (item) (not (string=? item str)))))
  (values left right))



(define (evaluate lst #:subelem? [s #f])
  (println (format "Input: ~a" lst))
  (match lst
    ; these are intertwined matchers with intern/extern wildcards
    [(list "*" a ... "*" b ...)
     (cons (cons 'i  (evaluate a #:subelem? #t))
           (evaluate b #:subelem? #t))]

    [(list "*" "*" a ... "*" "*" b ...)
     (cons (cons 'b (evaluate a #:subelem? #t))
           (evaluate b #:subelem? #t))]
    
    [(list "-" a ... "-" b ...)
     (cons (cons 's (evaluate a #:subelem? #t))
           (evaluate b #:subelem? #t))]

    [(list "_" a ...)
     (begin
       (define-values (inner outer)
         (split-grab a "_"))
       (if (empty? inner)
           (evaluate a #:subelem? #t)
           (cons (cons 'u (evaluate inner #:subelem? #t))
                 (evaluate (cdr outer) #:subelem? #t))))]

    ; Internal/external wildcard matching
    ; Match a URL anchor with [text](url)
    [(list "(" a ... ")" "(" b ...)
     (begin
       (displayln "Found potential URL")
       (define-values (left right)
         (splitf-at b notpclose?))
       (define textual (car (evaluate a #:subelem? #t)))
       (define target  (car (evaluate left #:subelem? #t)))
       (cons `(a ([href ,target]) ,textual)
             (evaluate (cdr right) #:subelem? #t)))]

    ; Match an image using ![alt text/description](url)
    [(list "!" "(" a ... ")" "(" b ...)
     (begin
       (displayln "Found potential Image")
       (define-values (left right)
         (splitf-at b notpclose?))
       (define quotstr (evaluate a #:subelem? #t))
       (define imgurl (car (evaluate left #:subelem? #t)))
       ;(when (not (hyperlink? imgurl))
       ;  (error
       ;   (format "Image: given URL not a valid link (given ~a)"
       ;           imgurl)))
       (define output
         (cons `(div ([class "img-div"])
                     (a ([href ,imgurl])
                        (img
                         ,(cons  (list 'src imgurl)
                                 (list (cons 'alt quotstr)))))
                     ,(cons 'p
                            (cons '((class "img-desc"))
                                  quotstr)))
               ;(p ([class "img-desc"]) ,quotstr))
               (evaluate (cdr right) #:subelem? #t)))
       (if (not s)
           (cons 'p output)
           output))]
    
    ; these are start-of-line matchers
    [(list "#" a ...)
     #:when (not s)
     (cons 'h1 (evaluate a #:subelem? #t))]
    [(list "##" a ...)
     #:when (not s)
     (cons 'h2 (evaluate a #:subelem? #t))]
    [(list "###" a ...)
     #:when (not s)
     (cons 'h3 (evaluate a #:subelem? #t))]
    [(list "####" a ...)
     #:when (not s)
     (cons 'h4 (evaluate a #:subelem? #t))]
    [(list "#####" a ...)
     #:when (not s)
     (cons 'h5 (evaluate a #:subelem? #t))]
    [(list "*" a ...)
     #:when (not s)
     (cons 'li (evaluate a #:subelem? #t))]

    ; Pretty much our base-case rules
    [(list a ...)
     #:when (not s)
     (cons 'p  (evaluate a #:subelem? #t))]
    [(list a b ...)
     (begin
       ;(writeln (format "Last branch ~a" a))
       (cons a (evaluate b #:subelem? #t)))]

    ; Last stand: just yield itself
    [else lst]))


(define (post-eval lst)
  ;(displayln (format "pe: ~a" lst))
  (match lst
    [(list a b)
     #:when (and (string? a) (string? b))
     (list (format "~a ~a" a b))]
    [(list a b c ...)
     #:when (and (string? a) (string? b))
     (post-eval (cons (format "~a ~a" a b) c))]
    [(list a b c ...)
     #:when (and (string? a) (notstring? b))
     (cons (format "~a " a) (post-eval (cons b c)))]
    [(list a b c ...)
     #:when (and (notstring? a) (string? b))
     (cons a (post-eval (cons (format " ~a" b) c)))]
    [(list head acc ...)
     (cons (post-eval head) (post-eval acc))]
    [else lst]))


(define (prep-blob lineblob)
  (define (inner str pairs)
    (if (empty? pairs)
        str
        (inner
         (string-replace str
                         (car (car pairs))
                         (cdr (car pairs)))
         (cdr pairs))))
  (filter notemptystr?
          (string-split
           (inner lineblob symbol-replace-map) " ")))


(define (file->xexpr fname)
  (define file-lines (file->lines fname))
  (map (λ (line) (post-eval (evaluate (prep-blob line))))
       (filter notemptystr? file-lines)))


(define (xexpr->file xexpr-t fname)
  (define fp (string->path fname))
  (displayln (format "~a" xexpr-t))
  (call-with-output-file fp
    #:exists 'replace
    (λ (out)
      (parameterize ([current-output-port out])
        (display "<!doctype html>")
        (write-xml/content
         (xexpr->xml
          (cons 'html xexpr-t))))))
  (displayln (format "Wrote to ~a successfully" fp)))


(module+ main
  (command-line
   #:program "Markdownify"
   #:args (fname)
   (xexpr->file (file->xexpr fname) "TEST.html")))

; end
