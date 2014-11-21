#lang racket
(require "heap.rkt")
(require "util.rkt")
(let* ((n 20000)
       (fl (random-float-list n))
      (il (random-int-list n 100)))
  (time (sorted? (heap-sort fl)))
  (time (sorted? (heap-sort il))))