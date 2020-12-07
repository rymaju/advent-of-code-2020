#lang racket

(define (day-6-1 filename)
   (apply + (map count-unique (split-list (file->lines filename) ""))))

(define (day-6-2 filename)
   (apply + (map count-all-yes (split-list (file->lines filename) ""))))

(define (count-unique los)
  (length (remove-duplicates (flatten (map string->list los)) equal?)))

(define (count-all-yes los)
  (if (= 1 (length los))
      (length (string->list (first los)))
      (count (λ (x) (andmap (λ (y) (member x (string->list y))) (rest los)))
             (string->list (first los)))))

; from day 4
(define (split-list list delim)
  (cond [(empty? list) '(())]
        [else
         (let [(ans (split-list (rest list) delim))]
           (if (string=? (first list) delim)
               (cons empty ans)
               (cons (append (string-split (first list) " ") (first ans)) (rest ans))))]))



(day-6-1 "input-day6.txt")
(day-6-2 "input-day6.txt")