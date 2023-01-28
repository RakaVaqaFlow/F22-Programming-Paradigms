; Name Surname: Vagif Khalilov
; Email: v.khalilov@innopolis.university
; Group: SD20-01
; Homework Assignment №1
#lang racket

; Exercise 1.1

; check whether a given expression is a variable
(define (variable? expr)
  (cond
    [(equal? '+ expr) #f]
    [(equal? '- expr) #f]
    [(equal? '* expr) #f]
    [(equal? '/ expr) #f]
    [(equal? '^ expr) #f]
    [(number? expr) #f]
    [(list? expr) #f]
    [else #t]
    ))

; check whether a given expression is a sum
(define (sum? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 3)) #f]
    [(not (equal? '+ (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [(not (or (variable? (second (rest expr))) (number? (second (rest expr))) (second (rest expr)))) #f]
    [else #t]
    ))

; extract first summand from a sum
(define (summand-1 expr)
  (cond
    [(sum? expr) (second expr)]
    [else
     (error "Expected a sum expression of the form (+ <expr> <expr>), but got: " expr)]
    ))

; extract second summand from a sum
(define (summand-2 expr)
  (cond
    [(sum? expr) (second (rest expr))]
    [else
     (error "Expected a sum expression of the form (+ <expr> <expr>), but got: " expr)]
    ))

; check whether a given expression is a product
(define (product? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 3)) #f]
    [(not (equal? '* (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [(not (or (variable? (second (rest expr))) (number? (second (rest expr))) (second (rest expr)))) #f]
    [else #t]
    ))

; extract first multiplier from a product
(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else
     (error "Expected a product expression of the form (* <expr> <expr>), but got: " expr)]
    ))

; extract second multipler from a product
(define (multiplier-2 expr)
  (cond
    [(product? expr) (second (rest expr))]
    [else
     (error "Expected a product expression of the form (* <expr> <expr>), but got: " expr)]
    ))

; Exercise 1.2, 1.6, 1.7

; functions for 1.6 and 1.7

; ----------------------------------------

; check whether a given expression is a exponentiation
(define (exponentiation? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 3)) #f]
    [(not (equal? '^ (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [(not (or (variable? (second (rest expr))) (number? (second (rest expr))) (second (rest expr)))) #f]
    [else #t]
    ))

; extract base from a exponentiation

(define (base expr)
  (cond
    [(exponentiation? expr) (second expr)]
    [else
     (error "Expected a exponentiation expression of the form (^ <expr> <expr>), but got: " expr)]
    ))

; extract power from a exponentiation

(define (power expr)
  (cond
    [(exponentiation? expr) (second (rest expr))]
    [else
     (error "Expected a exponentiation expression of the form (^ <expr> <expr>), but got: " expr)]
    ))

; check whether a given expression is sin
(define (sin? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 2)) #f]
    [(not (equal? 'sin (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [else #t]
    ))

; check whether a given expression is cos
(define (cos? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 2)) #f]
    [(not (equal? 'cos (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [else #t]
    ))

; check whether a given expression is tan
(define (tan? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 2)) #f]
    [(not (equal? 'tan (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [else #t]
    ))

; check whether a given expression is log
(define (log? expr)
  (cond
    [(not (list? expr)) #f]
    [(not (= (length expr) 2)) #f]
    [(not (equal? 'log (first expr))) #f]
    [(not (or (variable? (second expr)) (number? (second expr)) (list? (second expr)))) #f]
    [else #t]
    ))

; extact expression from sin/cos/tan/log
(define (arg expr)
  (cond
    [(or (sin? expr) (cos? expr) (tan? expr) (log? expr)) (second expr)]
    [else (error "Expected a exponentiation expression of the form (<sin/cos/tan/log> <expr>), but got: " expr)]
    )
  )

; check whether a given expression is a polyvariadic sum
(define (polyvariadic-sum? expr)
  (cond
    [(and (list? expr) (> (length expr) 3) (equal? (first expr) '+)) #t]
    [else #f]))

; check whether a given expression is a polyvariadic product
(define (polyvariadic-product? expr)
  (cond
    [(and (list? expr) (> (length expr) 3) (equal? (first expr) '*)) #t]
    [else #f]))

(define (transform-to-non-polyvariadic expr)
  (define (helper expr)
    (cond
      [(sum? expr) expr]
      [(product? expr) expr]
      [(polyvariadic-sum? expr) (list '+ (second expr) (helper (cons '+ (rest (rest expr)))))]
      [(polyvariadic-product? expr) (list '* (second expr) (helper (cons '* (rest (rest expr)))))]
      [else expr])
    )
  (helper expr)
  )
; ----------------------------------------

; recursive function derivative that computes a symbolic
; derivative of a given expression with respect to a given variable

(define (derivative expr var)

  ; summand handler
  (define (derivate-sum summand var)
    (cond
      [(list? summand) (helper summand var )]
      [(equal? summand var) 1]
      [else 0]
     )
    )

  ; multipler handler
  (define (derivate-mult multiplier var)
    (cond
      [(list? multiplier) (helper multiplier var )]
      [(equal? multiplier var) 1]
      [else 0]
     )
    )

  ; exponentiation handler
  (define (derivate-exponentiation expr var)
    (cond
      ; (f(x)^b)' = (b * f(x)^(b-1) * f(x)') 
      [(number? (power expr))
       (list '*
             (list '* (power expr) (list '^ (list '+ (power expr) -1) (base expr)))
             (helper (base expr) var ))]
      ; (b^f(x))' = (b^f(x) * log(b) * f(x)')
      [(number? (base expr))
       (list '*
             (list '* (list '^ (base expr) (power expr)) (list 'log (base power)))
             (helper (power expr) var ))]

      ;f(x)^g(x) = f(x)^(g(x)-1) * g(x) * f(x)' + f(x)^g(x) * (log (f(x)) * g(x)' 
      [else
       (list '+
             (list '* (list '^ (base expr) (list '+ (power expr) -1)) (list '* (power expr) (helper (base expr) var )))
             (list '* (list '^ (base expr) (power expr)) (list '* (list 'log (base expr)) (helper (power expr) var )))
             )]
      )
    )

  ; main helper function
  (define (helper expr var)
    (cond
      
      ; derivate exponentiation
      [(exponentiation? expr)
       (derivate-exponentiation expr var)]
      
      ; derivate sin
      [(sin? expr)
       (list '*
             (helper (arg expr) var)
             (list 'cos (arg expr)))]
      
      ; derivate cos
      [(cos? expr)
       (list '*
             -1
             (list '*
                   (helper (arg expr) var)
                   (list 'sin (arg expr))))]
      
      ; derivate tan
      [(tan? expr)
       (list '/
             (helper (arg expr) var)
             (list '^ (list 'cos (arg expr)) 2))]
      
      ; derivate log
      [(log? expr)
       (list '/
             (helper (arg expr) var)
             (arg expr))]
      
      ; derivate sum expression
      [(sum? expr)

       ; (u+v)' = u' + v'
       (cons '+ (cons (derivate-sum (summand-1 expr) var) (cons (derivate-sum (summand-2 expr) var) '())))] 
      
      ; derivate product expression
      [(product? expr)
       
       ;(u*v)' = u'v + uv'
       (cons '+ (cons (cons '* (cons (derivate-mult (multiplier-1 expr) var) (cons (multiplier-2 expr) '()))) (cons (cons '* ( cons (multiplier-1 expr) (cons (derivate-mult (multiplier-2 expr) var) '()))) '())))] 

      ; derivate polyvariadic-sum
      [(polyvariadic-sum? expr) (helper (transform-to-non-polyvariadic expr) var)]
      
      ; derivate polyvariadic-product
      [(polyvariadic-product? expr) (helper (transform-to-non-polyvariadic expr) var)]
      [(number? expr) 0]
      [(equal? expr var) 1]
    )
  )
 (helper expr var)
  )

(derivative '(+ 1 x) 'x) ; '(+ 0 1)

(derivative '(* 2 y) 'y) ; '(+ (* 0 y) (* 2 1))

(derivative '(* (+ x y) (+ x (+ x x))) 'x) ; '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))

; Exercise 1.3, 1.6, 1.7
(define (simplify expr)
  (define (simplify-at-root expr)
    (cond
      ; sum
      [(sum? expr)
            (cond
               ; 0 + e = e for all expressions e
               [(equal? (summand-1 expr) 0)
                 (summand-2 expr)]
               
               ; e + 0 = e for all expressions e
               [(equal? (summand-2 expr) 0)
                 (summand-1 expr)]
               
               ; c1 + c2 = c3
               [(and
                 (number? (summand-1 expr))
                 (number? (summand-2 expr)))
                 (+ (summand-1 expr) (summand-2 expr))]
               
               [else expr]
              )]
      ; product
      [(product? expr)
                (cond
               ; 0 ∗ e = 0 for all expressions e
               [(equal? (multiplier-1 expr) 0)
                 0]
               
               ; e ∗ 0 = 0 for all expressions e
               [(equal? (multiplier-2 expr) 0)
                 0]
               
               ; 1 ∗ e = e for all expressions e
               [(equal? (multiplier-1 expr) 1)
                 (multiplier-2 expr)]
               
               ; e ∗ 1 = e for all expressions e
               [(equal? (multiplier-2 expr) 1)
                 (multiplier-1 expr)]
               
               ; c1 * c2 = c3
               [(and
                 (number? (multiplier-1 expr))
                 (number? (multiplier-2 expr)))
                 (* (multiplier-1 expr) (multiplier-2 expr))]
               [else expr]
              )]
      ; exponentiation
      [(exponentiation? expr)
       (cond
         ; f(x)^1 = f(x)
         [(equal? (power expr) 1) (base expr)]
         ; f(x)^0 = 1
         [(equal? (power expr) 0) 1]
         ; 0^(f(x)) = 1
         [(equal? (base expr) 0) 1]
         ; 1^(f(x)) = 1
         [(equal? (base expr) 1) 1]
         ; c1^c2 = c3
         [(and (number? (base expr)) (number? (power expr))) (expt (base expr) (power expr))]
         [else expr]
         )
       ]
      
      ; sin
      [(sin? expr)
       (cond
         ; sin(0) = 1
         [(equal? (arg expr) 0) 0]
         
         [else expr]
        )]
      
      ; cos
      [(cos? expr)
       (cond
         ; cos(0)=1
         [(equal? (arg expr) 0) 1]
         
         [else expr])]
      
      ; tan
      [(tan? expr)
       (cond
         ; tan(0) = 0
         [[(equal? (arg expr) 0) 0]]
         
         [else expr])
       ]
      
      ; log
      [(log? expr)
       (cond
         ; log(1) = 0
          [[(equal? (arg expr) 1) 0]]
         ; log(e) = e
         [[(equal? (arg expr) 'e) 'e]]
         
         [else expr])]
      
      [else expr]
      )
    )
  (define (helper expr)
    (cond

      ; simplify sum
      [(sum? expr) (cond
                     ; (expr) + (expr) 
                     [(and
                       (list? (summand-1 expr))
                       (list? (summand-2 expr)))
                       (simplify-at-root (cons '+ (cons (helper (summand-1 expr)) (cons (helper (summand-2 expr)) '()))))]
                     
                     ; (expr) + (const or var)
                     [(and
                       (list? (summand-1 expr))
                       (not (list? (summand-2 expr))))
                       (simplify-at-root (cons '+ (cons (helper (summand-1 expr)) (cons (summand-2 expr) '()))))]
                     
                     ; (const or var) + (expr)
                     [(and
                       (not (list? (summand-1 expr)))
                       (list? (summand-2 expr)))
                       (simplify-at-root (cons '+ (cons (summand-1 expr) (cons (helper (summand-2 expr)) '()))))]
                     
                     ; (const or var) + (const or var)
                     [else (simplify-at-root expr)]
                     )]
      ; simplify product
      [(product? expr) (cond
                     ; (expr) * (expr) 
                     [(and
                       (list? (multiplier-1 expr))
                       (list? (multiplier-2 expr)))
                       (simplify-at-root (cons '* (cons (helper (multiplier-1 expr)) (cons (helper (multiplier-2 expr)) '()))))]
                     
                     ; (expr) * (const or var)
                     [(and
                       (list? (multiplier-1 expr))
                       (not (list? (multiplier-2 expr))))
                       (simplify-at-root (cons '* (cons (helper (multiplier-1 expr)) (cons (multiplier-2 expr) '()))))]
                     
                     ; (const or var) * (expr)
                     [(and
                       (not (list? (multiplier-1 expr)))
                       (list? (multiplier-2 expr)))
                       (simplify-at-root (cons '* (cons (multiplier-1 expr) (cons (helper (multiplier-2 expr)) '()))))]
                     
                     ; (const or var) * (const or var)
                     [else (simplify-at-root expr)]
                     )]
      ; simplify exponentiation
      [(exponentiation? expr)
       (simplify-at-root (list '^ (helper (base expr)) (helper (power expr))))]
      
      ; simplify sin
      [(sin? expr)
       (simplify-at-root (list 'sin (helper (arg expr))))]
      
      ; simplify cos
      [(cos? expr)
       (simplify-at-root (list 'cos (helper (arg expr))))]
      
      ; simplify tan
      [(tan? expr)
       (simplify-at-root (list 'tan (helper (arg expr))))]
      
      ; simplify log
      [(log? expr)
       (simplify-at-root (list 'log (helper (arg expr))))]
      
      ; simplify polyvariadic-sum
      [(polyvariadic-sum? expr) (helper (transform-to-non-polyvariadic expr))]
      
      ; simplify polyvariadic-product
      [(polyvariadic-product? expr) (helper (transform-to-non-polyvariadic expr))]
      [else (error "Invalid expression: " expr)]
      ))
  (helper expr)
  )

(simplify '(+ 0 1)) ; 1

(simplify '(+ (* 0 y) (* 2 1))) ; 2

(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))) ; '(+ (+ x (+ x x)) (* (+ x y) 3))

; Exercise 1.4

; Exercise 1.5

(define (to-infix expr)
  (cond
    [(sum? expr) (cons (to-infix (summand-1 expr)) (cons '+ (cons (to-infix (summand-2 expr)) '()))) ]
    [(product? expr) (cons (to-infix (multiplier-1 expr)) (cons '* (cons (to-infix (multiplier-2 expr)) '())))]
    [else expr]))

(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3))) ; '((x + (x + x)) + ((x + y) * 3)

; Exercise 1.8
(define (variables-of expr) 
  (remove-duplicates (sort (filter (lambda (e) (variable? e)) (flatten expr)) symbol<?)))
(variables-of '(+ 1 x y (* x y z))) ; '(x y z)

; Exercise 1.9

(define (gradient expr vars)
  (map
   (lambda (v) (simplify (derivative expr v)))
   vars))

(gradient '(+ 1 x y (* x y z)) '(x y z)) ; '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))

