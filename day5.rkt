#lang racket
(define (day-5-1 filename)
  (let* [(seat-ids (map seat-id (file->lines filename)))
         (seat-min (apply min seat-ids))
         (seat-max (apply max seat-ids))]
    (filter
     (λ (x) (not (member x seat-ids)))
     (build-list (- seat-max seat-min) (λ (x) (+ x seat-min))))))



(define (calc-encoding s lo hi)
  (cond [(or (empty? s) (= lo hi)) lo]
        [else
         ;(println `(,(first s) ,lo ,hi))
         (if (or (equal? (first s) #\F) (equal? (first s) #\L))
                  (calc-encoding (rest s) lo (floor (/ (+ hi lo) 2)))
                  (calc-encoding (rest s) (+ (floor (/ (+ hi lo) 2)) 1) hi))]))


(define (seat-id encoding)
  (+ (* 8 (calc-encoding (string->list (substring encoding 0 7)) 0 127))
     (calc-encoding (string->list (substring encoding 7)) 0 7)))

;(seat-id "FBFBBFFRLR")


(day-5-1 "input-day5.txt")