#lang slideshow
; Khalilov Vagif
; SD20-01
; Programming Paradigms Problem Set 2

; EX. 1

;  1.a

(define (binary-to-decimal lst)
  (define (helper lst val t)
    (cond
      [(empty? lst) val]
      [else (helper (rest lst) (+ val (* t (first lst))) (* t 2))])
    )
  (helper (reverse lst) 0 1))

(binary-to-decimal '(1 0 1 1 0)) ;22

; 1.b

(define (count-zeros lst)
  (define (helper lst cnt flg)
    (cond
      [(empty? lst) cnt]
      [(= (first lst) 1) (helper (rest lst) cnt 1)]
      [(and (= flg 1) (= (first lst) 0)) (helper (rest lst) (+ cnt 1) 1)]
      [else (helper (rest lst) cnt flg)])
    
    )
  (helper lst 0 0))

(count-zeros '(0 0 0 1 0 1 1 0)) ; 2

; 1.c

(define (encode-with-lengths lst1)
  (define (remove-leading-zeros lst2)
    (define (helper lst3)
      (cond
        [(= (first lst3) 1) lst3]
        [else (helper (rest lst3))]))
   (helper lst2))
  (define (count-nums lst2)
    (define (helper lst3 ans cnt flg)
      (cond
        [(empty? lst3) (reverse (cons cnt ans))]
        [(= (first lst3) flg) (helper (rest lst3) ans (+ cnt 1) flg)]
        [else (helper (rest lst3) (cons cnt ans) 1 (first lst3))]))
   (helper (rest lst2) '() 1 1))
  
  (count-nums (remove-leading-zeros lst1))
)

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) ; '(2 1 3 2)

; 1.d

(define (binary-odd? lst)
  (cond
    [(= (first (reverse lst)) 1) #t]
    [else #f]))

(binary-odd? '(1 0 1 1 0)) ; #f
(binary-odd? '(1 0 1 1 1)) ; #t

; 1.e

(define (decrement lst)
  (define (helper lst1 ans)
    (cond
      [(and (= (first lst1) 1) (= (length lst1) 1)) ans]
      [(= (first lst1) 1) (append (reverse (rest lst1)) (cons 0 ans))]
      [else (helper (rest lst1) (cons 1 ans))]))
  (cond
    [(and (= (first lst) 0) (= (length lst) 1)) lst]
    [(and (= (first lst) 1) (= (length lst) 1)) '(1)]
    [else (helper (reverse lst) '())]
  )
  )

(decrement '(1 0 1 1 0)) ; '(1 0 1 0 1)
(decrement '(1 0 0 0 0)) ; '(1 1 1 1)
(decrement '(0)) ; '(0)

; EX. 2

; 2.a

(define (alternating-sum lst)
  (cond
    [(empty? lst) 0]
  [(= (length lst) 1) (first lst)]
  [else (+ (- (first lst) (first (rest lst))) (alternating-sum( rest (rest lst))))])
  )

(alternating-sum (list 6 2 4 1 3 9)) ; 1

; 2.b

; Substitution Model:
; (alternating-sum (list 6 2 4 1 3 9))
; ==> (+ (- 6 2) (alternating-sum (list 4 1 3 9)))
; ==> (+ (- 6 2) (+ (- 4 1) (alternating-sum (list 3 9))))
; ==> (+ (- 6 2) (+ (- 4 1) (alternating-sum (list 3 9))))
; ==> (+ (- 6 2) (+ (- 4 1) (+ (- 3 9) (alternating-sum (list)))))
; ==> (+ (- 6 2) (+ (- 4 1) (+ (- 3 9) 0)))
; ==> (+ (- 6 2) (+ (- 4 1) (+ -6 0)))
; ==> (+ (- 6 2) (+ (- 4 1) -6))
; ==> (+ (- 6 2) (+ 3 -6))
; ==> (+ (- 6 2) -3)
; ==> (+ 4 -3)
; 1

; 2.c

; Actually, I do not know how to apply tail recursion to improve the solution.
; We can add an additional 'helper' function that will take as arguments a list, the sum and the multiplier for the first element of the list:

(define (another-alternating-sum lst)
  (define (helper lst sum flg)
  (cond
    [(empty? lst) sum]
    [else (helper (rest lst) (+ sum (* (first lst) flg)) (* flg -1))])
  )
  (helper lst 0 1))

; However, the number of calls to this function will not change compared to the solution, but it will require more memory,
; from this I can conclude that the use of recursion will negatively affect the solution of the problem.

; EX. 3

(define (dec n) (- n 1))
(define (f n)
(cond
[(<= n 2) (- 10 n)]
[else (* (f (dec (dec n))) (f (dec n)))]))

(f 3) ; 72

; Substitution Model:
; (f 3)
; ==> (* (f (dec (dec 3))) (f (dec 3)))
; ==> (* (f (dec (dec 3))) (f (- 3 1)))
; ==> (* (f (dec (dec 3))) (f 2))
; ==> (* (f (dec (dec 3))) (- 10 2))
; ==> (* (f (dec (dec 3))) (- 10 2))
; ==> (* (f (dec (dec 3))) 8)
; ==> (* (f (dec (- 3 1))) 8)
; ==> (* (f (dec 2)) 8)
; ==> (* (f (- 2 1)) 8)
; ==> (* (f 1) 8)
; ==> (* (- 10 1) 8)
; ==> (* 9 8)
; ==> 72










