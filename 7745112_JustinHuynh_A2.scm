#lang racket

;Justin Huynh
;7745112
;CSI210 Assignment 2

;QUESTION 1
(define (distanceGPS lat1 lon1 lat2 lon2)
	(let ((r_lat1 (* pi (/ lat1 180)))
		(r_lon1 (* pi (/ lon1 180)))
		(r_lat2 (* pi (/ lat2 180)))
		(r_lon2 (* pi (/ lon2 180))))
          (* 6371.0 (* 2 (asin (sqrt(+ (expt (sin (/ (- r_lat1 r_lat2) 2)) 2)
                                      (* (cos r_lat1) (cos r_lat2)
                                         (expt (sin(/ (- r_lon1 r_lon2) 2)) 2)))))))))

;QUESTION 2

;*******absDiffA(listA listB)*****
;Assumes that the missing elements in the shorter list have the value of 0.
;The result will be of length equal to the longer list.
(define (absDiffA listA listB)
(cond ((< (length listA) (length listB)) (map abs (map - (append-zero listA (- (length listB) (length listA))) listB)))
	  ((> (length listA) (length listB)) (map abs (map - (append-zero listB (- (length listA) (length listB))) listA)))
          ((= (length listA) (length listB))
	  (map abs (map - listA listB)))))


;Num = Number of zeros to append to the end of list L1
(define (append-zero L1 Num)
  (if (= Num 0)
      L1
      (append-zero (append L1 '(0)) (- Num 1))))

;*********absDiffB(listA listB) *****
;Ignores the extra values of the longer list. The result will be of length equal
;to the shorter list
(define (absDiffB listA listB)
(cond ((> (length listA) (length listB)) (map abs (map - (shorten-list listA '() (length listB)) listB)))
      ((> (length listB) (length listA)) (map abs (map - (shorten-list listB '() (length listA)) listA)))
      ((= (length listA) (length listB))
       (map abs (map - listA listB)))))

;Num = Desired length of the list
;L2 = Shortened list to length Num                                                          
(define (shorten-list L1 L2 Num)
  (if (= Num 0)
      L2
      (shorten-list (cdr L1) (append L2 (list(car L1))) (- Num 1))))

;QUESTION 3
(define (duplicatePair L)
  (if (null? L)
      L
      (duplicatePair-aux L '())))

(define (duplicatePair-aux L L2)
  (cond
    ((null? L) L2)
    (else
     (duplicatePair-aux (delete (car L) L)  (append L2 (list(cons (car L) (count (car L) L))))))))


;Deletes all occurences of an item in a list
(define (delete item list) (filter (lambda (x) (not (equal? x item))) list))

;Counts how many times element e appears in list L
(define count
  (lambda (e L)
    (cond
      ((null? L)     0)
      ((equal? e (car L)) (+ 1 (count e (cdr L))))
      (else          (count e (cdr L))))))
  
(duplicatePair '(1 a 5 6 2 b a 5 5))
			
			