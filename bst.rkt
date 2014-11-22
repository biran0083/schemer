#lang racket

(define (bst-empty cmp) (cons cmp '()))
(define (bst-empty? bst) (empty? (cdr bst)))

(define (find cmp tree v)
  (match tree
    ['() (void)]
    [(list v0 l r) (match (cmp v v0)
                     [x #:when (< x 0) (find l v)]
                     [x #:when (> x 0) (find r v)]
                     [else v0])]))
(define (bst-find bst v)
  (match bst
    [(cons cmp tree) (find cmp tree v)]))

(define (insert cmp tree v)
  (match tree
    ['() (list v '() '())]
    [(list v0 l r) (match (cmp v v0)
                     [x #:when (< x 0) (list v0 (insert cmp l v) r)]
                     [x #:when (> x 0) (list v0 l (insert cmp r v))]
                     [else (list v l r)])]))
(define (bst-insert bst v)
  (match bst
    [(cons cmp tree) 
     (cons cmp (insert cmp tree v))]))

(define (remove-max tree)
  (match tree
    [(list v l '()) (cons v l)]
    [(list v l r) (match (remove-max r)
                    [(cons v-max new-r) (cons v-max (list v l new-r))])]))

(define (bst-remove-max bst)
  (match bst
    [(cons cmp tree)
     (match (remove-max tree)
       [(cons v-max rest-tree) (cons v-max (cons cmp rest-tree))])]))

(define (remove-min tree)
  (match tree
    [(list v '() r) (cons v r)]
    [(list v l r) 
     (match (remove-min l)
       [(cons v-min new-l)
        (cons v-min (list v new-l r))])]))

(define (bst-remove-min bst)
  (match bst
    [(cons cmp tree)
     (match (remove-min tree)
       [(cons v-min rest-tree) 
        (cons v-min (cons cmp rest-tree))])]))

(define (remove cmp tree v)
  (match tree
    [(list v0 l r) 
     (match (cmp v v0)
       [x #:when (< x 0) (match (remove cmp l v)
                           [(cons rm-v new-l) (cons rm-v (list v0 new-l r))])]
       [x #:when (> x 0) (match (remove cmp r v)
                           [(cons rm-v new-r) (cons rm-v (list v0 l new-r))])]
       [0 (if (empty? l) 
              (cons v0 r)
              (match (remove-max l)
                [(cons v-max new-l) (cons v0 (list v-max new-l r))]))])]
    [else (cons (void) tree)]))

(define (bst-remove bst v)
  (match bst
    [(cons cmp tree) 
     (match (remove cmp tree v)
       [(cons v0 rest-tree) (cons v0 (cons rest-tree))])]))

(define (bst-build l cmp)
  (define (iter acc rem)
    (match rem
      ['() acc]
      [(cons fst rst) (iter (bst-insert acc fst) rst)]))
  (iter (bst-empty cmp) l))

(define (bst-sort l cmp)
  (define (helper bst acc)
    (if (bst-empty? bst)
        acc
        (match (bst-remove-max bst)
          [(cons v-max new-bst) (helper new-bst (cons v-max acc))])))
  (helper (bst-build l cmp) '()))
