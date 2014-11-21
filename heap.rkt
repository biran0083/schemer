#lang racket

(define (insert-tree t v path)
  (match path
    ['() (list v '() '())]
    [(cons branch rest-path)  
     (match t
       [(list v0 l r)
        (if (= 0 branch)
            (match (insert-tree l v rest-path)
              [(list lv ll lr) #:when (> v0 lv) 
                               (list lv (list v0 ll lr) r)]
              [nl (list v0 nl r)])
            (match (insert-tree r v rest-path)
              [(list rv rl rr) #:when (> v0 rv) 
                               (list rv l (list v0 rl rr))]
              [nr (list v0 l nr)]))])]))

(define (remove-tree tree path)
  (match tree
    [(list v l r)
     (match path
       ['() (cons v '())]
       [(cons branch rest-path)
        (if (= 0 branch)
            (let ((res (remove-tree l rest-path)))
              (cons (car res) (list v (cdr res) r)))
            (let ((res (remove-tree r rest-path)))
              (cons (car res) (list v l (cdr res)))))])]))

  (define (percolatee-down tree)
    (match tree
      [(list v (list lv ll lr) '())
       #:when (< lv v) 
       (list lv (list v ll lr) '())]
      [(list v (list lv ll lr) (list rv rl rr)) 
       #:when (and (< lv v) (<= lv rv))
       (list lv (percolatee-down (list v ll lr)) (list rv rl rr))]
      [(list v (list lv ll lr) (list rv rl rr)) 
       #:when (and (< rv v) (< rv lv))
       (list rv (list lv ll lr) (percolatee-down (list v rl rr)))]
      [else tree]))

(define empty-heap (list 0 '()))
(define (new-heap size tree) (list size tree))
(define heap-get-size car)
(define heap-get-tree cadr)

(define (int-to-bits n)
  (define (helper acc n)
    (if (= 0 n)
        acc
        (helper (cons (bitwise-and 1 n) acc)
                (arithmetic-shift n -1))))
  (helper '() n))

(define (heap-insert h v)
  (let* ((size (heap-get-size h))
         (tree (heap-get-tree h))
         (path (cdr (int-to-bits (+ size 1)))))
    (new-heap (+ size 1) (insert-tree tree v path))))

(define (heap-remove-min h)
  (let* ((size (heap-get-size h))
         (tree (heap-get-tree h))
         (path (cdr (int-to-bits size))))
    (match (remove-tree tree path)
      [(cons v '()) (cons v empty-heap)]
      [(cons v-last (list v l r))
       (cons v (new-heap (- size 1) (percolatee-down (list v-last l r))))])))

(define (build-heap-naive l)
  (define (iter acc rem)
    (if (empty? rem)
        acc
        (iter (heap-insert acc (car rem)) (cdr rem))))
  (iter empty-heap l))

(define (heap-sort l)
  (define (helper h)
    (if (= 0 (heap-get-size h))
        '()
        (let* ((p (heap-remove-min h))
               (v (car p))
               (new-h (cdr p)))
          (cons v (helper new-h)))))
  (helper (build-heap-naive l)))

(provide heap-sort build-heap-naive heap-remove-min heap-insert empty-heap)