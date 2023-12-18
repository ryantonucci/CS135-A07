;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 7 Examples

;;1
(define car (make-component "car" 1
                            (list (make-component "frame" 1 empty)
                                  (make-component "wheel" 4 (list (make-component "tire" 4 empty)
                                                                  (make-component "rim" 4 empty)))
                                  (make-component "powertrain" 1 (list (make-component "engine" 1 empty)
                                                                       (make-component "transmission" empty)
                                                                       (make-component "differential" empty)))
                                  (make-component "steering wheel" 1 empty)
                                  (make-component "altinator" 2))))
                                          

(check-expect (contains-component? car "handlebar") false)
(check-expect (contains-component? car   s "engine") true)

;;2a

