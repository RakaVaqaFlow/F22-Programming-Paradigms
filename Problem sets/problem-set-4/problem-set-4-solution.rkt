#lang racket
; Programming Paradigms Fall 2022 — Problem Set 4
; Name Surname: Vagif Khalilov
; Email: v.khalilov@innopolis.university
; Group: BS20-SD-01

; ======= Ex. 1 =======

; --- 1.a ---

(define (replicate num v)
  (cond
    [(<= num 0) '()]
    [else (cons v (replicate (- num 1) v))]))

(replicate 10 'a) ; '(a a a a a a a a a a)

(replicate 3 '(1 . 2)) ; '((1 . 2) (1 . 2) (1 . 2))

; --- 1.b ---

(define (split lst num)
  (define (helper f-lst r-lst num)
    (cond
      [(equal? num 0) (list f-lst r-lst)]
      [else (helper (append f-lst (list (first r-lst))) (rest r-lst) (- num 1))]
      ))
  (cond
    [(>= num (length lst)) (list lst '())]
    [else (helper '() lst num)]))

(split '(1 2 3 4 5) 2) ; '((1 2) . (3 4 5))

(split '(a b c d) 4) ; '((a b c d) . ())

(split '(a b c) 4) ; '((a b c) . ())

(split '(a b c) 0) ; '(() . (a b c))

; --- 1.c ---

(define (chunks lst size)
  (define (helper f-lst r-lst num)
    (cond
      [(>= num (length r-lst)) (append f-lst (list r-lst))]
      [else (helper (append f-lst (list (first (split r-lst num))) ) (second (split r-lst num)) num)]))
  (cond
    [(>= size (length lst)) lst]
    [else (helper '() lst size)]))

(chunks '(1 2 3 4 5) 2) ; '((1 2) (3 4) (5))

(chunks '(a b c d e f) 3) ; '((a b c) (d e f))

; --- 1.d ---

(define (windows lst size)
  (define (helper f-lst r-lst size)
    (cond
      [(>= (length (first (split r-lst size))) size) (helper (append f-lst (list (first (split r-lst size)))) (rest r-lst) size)]
      [else f-lst]))
  (cond
    [(>= size (length lst)) lst]
    [else (helper '() lst size)]))

(windows '(1 2 3 4 5) 2) ; '((1 2) (2 3) (3 4) (4 5))

(windows '(a b c d e) 3) ; '((a b c) (b c d) (c d e))
; ======= Ex. 2 =======
; we can use apply, map, andmap, ormap, filter, foldl

; --- 2.a ---

(define (pairs lst)
  
(foldl
   (lambda (group-pairs res) (append res group-pairs))
   '() (map
        (lambda (x)
          (map
           (lambda (a)
             (cons x a))
           (remove x lst)))
        lst))
)

(pairs '(a b c d)) ; '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))
; --- 2.b ---

; --- 2.c ---

; --- 2.d ---

; --- 2.e ---


; ======= Ex. 3 =======

; --- 3.a ---
; --- 3.b ---
; --- 3.c ---
; --- 3.d ---
; --- 3.e ---