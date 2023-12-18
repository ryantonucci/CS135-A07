;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname components) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 7 


;;Q1
(define-struct component (name num subcomponents))

(define vehicle (make-component "vehicle" 1
                                (list (make-component "frame" 1 empty)
                                      (make-component "wheel" 4
                                                      (list (make-component "tire" 4 empty)
                                                            (make-component "rim" 4 empty)))
                                      (make-component "powertrain" 1
                                                      (list (make-component "engine" 1 empty)
                                                            (make-component "transmission" 1 empty)
                                                            (make-component "differential" 1 empty)))
                                      (make-component "steering wheel" 1 empty)
                                      (make-component "altinator" 2 empty))))
                                          
;;(contains-component? component name) produces true if the component tree contains name
;;Examples
(check-expect (contains-component? vehicle "handlebar") false)
(check-expect (contains-component? vehicle "engine") true)
;;contains-component?: component Str -> Bool

(define (contains-component? component name)
  (cond [(empty? component) false]
        [(string=? (component-name component) name) true]
        [else (check-list (component-subcomponents component) name)]))


(define (check-list list name)
  (cond [(empty? list) false]
        [else (or (contains-component? (first list) name)
                  (check-list (rest list) name))]))

(check-expect (contains-component? vehicle "hammer") false)
(check-expect (contains-component? vehicle "vehicle") true)
(check-expect (contains-component? vehicle "steering wheel") true)       
(check-expect (contains-component? vehicle "differential") true)
(check-expect (contains-component? vehicle "tire") true)
