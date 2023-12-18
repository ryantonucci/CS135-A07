;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname aexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignbment 7

;;2a
;;An Arithmetic Expression (AExp) is one of:
;; * Num
;; * Sym
;; * OpNode

(define-struct opnode (op args))
;; An Operator Node (OpNode) is a
;; (make-opnode (anyof '* '+) (listof AExp))


;;2b
;;aexp-template: AExp -> Any
(define (aexp-template ex) 
  (cond [(number? ex) ...]
        [(symbol? ex) ...]
        [(opnode? ex) (opnode-template (opnode-op ex)
                                       (opnode-args ex))]))

(define (opnode-template op args)
  (cond [(empty? args) ...]
        [else (... (aexp-tamplate (first args))
                   (opnode-template op (rest args)))]))

;;2c
(define sym-table (list (list 'x 2) (list 'y 3) (list 'z 5)))
(define a-exp1 (make-opnode '+ (list 'y (make-opnode '* (list 'x 1)) (make-opnode '+ (list 'y 'z)))))
(define a-exp2 (make-opnode '* (list 'y (make-opnode '+ (list 'x 1)) (make-opnode '* (list 'y 'z)))))

;;(eval exp symbol-table) takes an expression which could include symbols
;;and evaluates the expression according to the values in the symbol-table.
;;Examples
(check-expect (eval a-exp1 sym-table) 13)
(check-expect (eval a-exp2 sym-table) 135)
;;eval: AExp AL -> Num




(define (eval exp alst)
  (cond [(number? exp) exp]
        [(opnode? exp) (apply-v2 (opnode-op exp)
                                 (opnode-args exp) alst)]))

;;Basic test
(define a-exp
  (make-opnode '+ (list 'x 'y (make-opnode '* (list 5 'z)))))
(define sym-table2 (list (list 'x 1) (list 'y 2) (list 'z 4)))
(check-expect (eval a-exp sym-table2) 23)


(define (apply-v2 op args alst)
  (cond [(empty? args) (cond [(symbol=? op '+) 0]
                             [(symbol=? op '*) 1])]
        [(symbol=? op '+) (+ (cond [(symbol? (first args))
                                    (eval (lookup-al (first args) alst) alst)]
                                   [else (eval (first args) alst)])
                             (apply-v2 op (rest args) alst))]
        [(symbol=? op '*) (* (cond [(symbol? (first args))
                                    (eval (lookup-al (first args) alst) alst)]
                                   [else (eval (first args) alst)])
                             (apply-v2 op (rest args) alst))]))

(define (lookup-al k alst)
  (cond [(empty? alst) false]
        [(symbol=? k (first (first alst))) (second (first alst))]
        [else (lookup-al k (rest alst))]))

(check-expect (lookup-al 'y sym-table) 3)

;;Q2d
;;(simplify ex) simplifies expressions writton as opnode structures
;;Examples
(check-expect (simplify (make-opnode '+ (list 3 'x 7)))
              (make-opnode '+ (list 'x 10)))
(check-expect (simplify (make-opnode '* (list 3 'x 7)))
              (make-opnode '* (list 'x 21)))
;;simplify: AExp -> AExp

#|

(define (simplify/lst lst op acc)
  (cond [(empty? lst) acc]
        [(number? (first lst))
         (simplify/lst (rest lst) op (list (cond [(symbol=? op '+)
                                                  (+ (first lst) (first acc))]
                                                 [(symbol=? op '*) 
                                                  (* (first lst) (first acc))]
                                                 [else (first acc)])))]
        [else (simplify/lst (rest lst) op (append (list (first lst)) acc))]))

(define (simplify/combine op AExp lst acc)
  (cond [(empty? lst) acc]
        [else (append (list AExp) acc)]))

(define (maybe-simplify-constant op lst-of-AExp)
  (make-opnode op (lst-of-AExp)))
  

(check-expect (simplify
               (make-opnode '* (list 3 'x (make-opnode '+ (list 2 'x 3)) 'z 7)))
              (make-opnode '* (list 'x (make-opnode '+ (list 'x 5)) 'z 21)))
|#

(define (simplify ex)
  (cond
    [(number? ex) ex] ; Base case: If it's a number, no further simplification is possible.
    [(opnode? ex) (simplify/combine (opnode-op ex) (opnode-args ex) empty)]
    [else ex]))

;;Test
(check-expect (simplify
               (make-opnode '* (list 3 'x (make-opnode '+ (list 2 'x 3)) 'z 7)))
              (make-opnode '* (list 'x (make-opnode '+ (list 'x 5)) 'z 21)))

(define (simplify/lst lst op acc)
  (cond [(empty? lst) acc]
        [(number? (first lst))
         (simplify/lst (rest lst) op (list (cond [(symbol=? op '+)
                                                   (+ (first lst) (first acc))]
                                                  [(symbol=? op '*) 
                                                   (* (first lst) (first acc))]
                                                  [else (first acc)])))]
        [(opnode? (first lst))
         (simplify/lst (rest lst) op (append (list (opnode-args (first lst))) acc))]
        [else (append (list (first lst)) (simplify/lst (rest lst) op acc))]))

(check-expect (simplify/lst (list 3 'x 7) '+ (list 0)) (list 'x 10))
(check-expect (simplify/lst (list 3 'x 7) '* (list 1)) (list 'x 21))

(define (simplify/combine op args acc)
  (cond [(empty? args)
         (make-opnode op (simplify/lst acc op (cond [(symbol=? op '+) (list 0)]
                                                    [(symbol=? op '*) (list 1)]
                                                    [else (list 1)])))]
        [else (simplify/combine op (rest args) (cons (simplify (first args)) acc))]))

(check-expect (simplify/combine '+ (list 3 5 7) empty) (make-opnode '+ (list 15)))
(check-expect (simplify/combine '* (list 2 3 4) empty) (make-opnode '* (list 24)))
(check-expect (simplify/combine '+ (list 2 'x 3) empty) (make-opnode '+ (list 'x 5)))

(define (contains-constants? lst)
  (cond [(empty? lst) false]
        [(or (number? (first lst)) (contains-constants? (rest lst))) true]
        [else false]))
    






