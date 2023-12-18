;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "fs-print.rkt")
;;Ryan Tonucci (21059852)
;;Assignment 7

;;3a
;;(list-files fd) makes a list of file names
;;Example
(check-expect (list-files sample-fs)
              (list "readme.txt" "zahra.jpg" "timbit.jpg"
                    "chonky.jpg" "beach1.jpg" "beach2.jpg"
                    "beach3.jpg" "oreo.jpg" "anna.jpg"
                    "beach1.jpg" "eagles-hotel-california.mp3"
                    "bee-gees-stayin-alive.mp3"
                    "lady-gaga-bad-romance.mp3" "beyonce-single-ladies.mp3"
                    "shopping.txt" "todo.txt"))
;;list-files: FileDir -> (listof Str)

(define (list-files fd)
  (list-files/acc fd empty))

(define (list-files/acc fd acc)
  (cond
    [(dir? fd)
     (append (list-files-helper (dir-contents fd)) acc)]
    [else (append (list-files-helper fd) acc)])) 

(define (list-files-helper contents) 
  (cond [(empty? contents) empty]
        [(dir? (first contents))
         (append (list-files/acc (dir-contents (first contents)) empty)
                 (list-files-helper (rest contents)))]
        [else (append (list (file-name (first contents)))
                      (list-files-helper (rest contents)))]))

;;3b
;;(backup Dir)
;;Examples
(check-expect (backup (make-dir "name" (list (make-file "filedir.rkt" 5400 1698197579))))
              (make-dir "name" (list (make-file "filedir.rkt" 5400 1698197579)
                                     (make-file "filedir.rkt.bak" 5400 1698197579))))
;;backup: FileDir -> FileDir

(define (backup dir)
  (make-dir (dir-name dir) (add-backup (dir-contents dir))))

;;add-backup: (listof (anyof (listof FileDir) Dir)) -> (list (anyof (listof FileDir) Dir))
(define (add-backup lst)
  (cond [(empty? lst) empty]
        [(dir? (first lst)) (append (list (backup (first lst))) (add-backup (rest lst)))]
        [else (append
               (list (first lst))
               (list (make-file (string-append (file-name (first lst)) ".bak")
                                (file-size (first lst))
                                (file-timestamp (first lst))))
               (add-backup (rest lst)))]))

;;3c
;;(get-time FileDir selection)
;;Examples
;;(check-expect (get-time sample-fs "oreo") 1319542066)
;;(check-expect (get-time sample-fs "notes") 1319679565)
;;get-time: FileDir -> Nat

;;(define (get-time FileDir selection)
;;  (cond
;;    [(list? FileDir)
;;     ]
;;    [(dir? file-or-dir)
;;     (get-time-from-dir selection (dir-contents file-or-dir))]
;;    [else false])) 

#|

(define (find-selection FileDir selection)
  (cond [(empty? FileDir) false]
        [(string=? selection (cond [(dir? FileDir)
                                    (dir-name FileDir)]
                                   [else (file-name FileDir)]))
          (cond [(file? FileDir)
                 (make-file (file-time FileDir))]
                [else (max (list-of-timestamps FileDir))])]
        [else ]))

    
(check-expect (find-selection sample-fs "oreo.jpg")
              (make-file "oreo.jpg" 3287000 1319542066))

(define (list-of-timestamps FileDir) ;makes a list of timestamps under a directory
  (cond [(empty? FileDir) empty]
        [else (

|#


