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

;Ignores the extra values of the longer list. The result will be of length equal
;to the shorter list
;(define (absDiffB listA listB)
;  )
			
			