#lang racket
(define (sorted? l)
  (if (or (empty? l) (empty? (cdr l)))
      #t
      (and (<= (car l) (cadr l))
           (sorted? (cdr l)))))

(define (random-float-list n)
  (define (helper acc rem)
    (if (= 0 rem)
        acc
        (helper (cons (random) acc) (- rem 1))))
  (helper '() n))

(define (random-int-list n bound)
  (define (helper acc rem)
    (if (= 0 rem)
        acc
        (helper (cons (random bound) acc) (- rem 1))))
  (helper '() n))

(provide sorted? random-float-list random-int-list)