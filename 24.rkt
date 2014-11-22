#lang racket
(define (all-split l)
  (match l
    [(list x) '()]
    [(cons fst rst) (map (lambda (p) (cons (cons fst (car p)) (cdr p))) 
                         (cons (cons '() rst) (all-split rst)))]))
(define (flat ll) (foldr append '() ll))
(define (flat-map f l) (flat (map f l)))
(define (remove-first l v)
  (match l
    ['() '()]
    [(cons fst rst) #:when (eq? fst v) rst]
    [(cons fst rst) (cons fst (remove-first rst v))]))
(define (member? e l eq?)
  (match l
    ['() #f]
    [(cons fst rst) #:when (eq? fst e) #t]
    [(cons fst rst) (member? e rst eq?)]))
(define (unique l eq?)
  (if (empty? l)
      '()
      (let ([t (unique (cdr l) eq?)])
        (if (member? (car l) t eq?)
            t
            (cons (car l) t)))))
(define (permutation l)
  (match l
    ['() '(())]
    [else (flat-map (lambda (e) 
                      (map (lambda (x) (cons e x)) 
                           (permutation (remove-first l e))))
                    (unique l eq?))]))
(define (cartesian-product l1 l2)
  (flat-map (lambda (v1)
              (map (lambda (v2) (cons v1 v2))
                   l2))
            l1))
(define (all-ast ops values)
  (match values
    [(list x) values]
    [else (flat-map (lambda (p)
                      (let ([l-ast-list (all-ast ops (car p))]
                            [r-ast-list (all-ast ops (cdr p))])
                        (flat-map (lambda (tree-pair)
                                    (map (lambda (op) (list op (car tree-pair) (cdr tree-pair))) ops))
                                  (cartesian-product l-ast-list r-ast-list))))
                    (flat-map all-split (permutation values)))]))

(define ns (make-base-namespace))
(define (eval-with-ns l) 
  (with-handlers ([exn:fail:contract:divide-by-zero?
                   (lambda (exn) +inf.0)])
    (eval l ns)))
(define (is-answer? exp)
  (< (abs (- (eval-with-ns exp) 24)) 1e-6))

(define (first-non-zero l)
  (if (empty? l)
      0
      (if (not (zero? (car l)))
          (car l)
          (first-non-zero (cdr l)))))
(define (symbol-cmp s1 s2)
  (let ([str1 (symbol->string s1)]
        [str2 (symbol->string s2)])
    (if (string<? str1 str2)
        -1
        (if (string>? str1 str2)
            1
            0))))
(define (ast-cmp t1 t2)
  (match (cons t1 t2)
    [(cons (list op1 l1 r1) (list op2 l2 r2))
     (first-non-zero (list (symbol-cmp op1 op2) (ast-cmp l1 l2) (ast-cmp r1 r2)))]
    [(cons v (list _ _ _)) -1]
    [(cons (list _ _ _) _) 1]
    [(cons v1 v2) (- v1 v2)]))
(define (normalize-ast ast)
  (match ast
    [(list op l r)
     (let* ([nl (normalize-ast l)]
            [nr (normalize-ast r)])
       (match op
         ['- (list '- nl nr)]
         ['/ (list '/ nl nr)]
         [_ (match (ast-cmp nl nr)
              [x #:when (< x 0) (list op nl nr)]
              [else (list op nr nl)])]))]
    [else ast]))
(define (ast-eq? t1 t2) (zero? (ast-cmp t1 t2)))

(define (solve-24 l)
  (filter is-answer? (all-ast '(+ - * /) l)))
(solve-24 '(5 5 5 1))