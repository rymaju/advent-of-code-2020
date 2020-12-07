#lang racket

(define (day1 filename)
  (let* [(numbers (map string->number (file->lines filename)))
         (2020-pair (pair-sum-to-n numbers 2020))
         (2020-triplet (triplet-sum-to-n numbers 2020))]
    
    ;(println 2020-pair)
    (println 2020-triplet)
    ;(when 2020-pair (apply * 2020-pair))
    (when 2020-triplet (apply * 2020-triplet))))

(define (triplet-sum-to-n numbers n)
  (cond [(empty? numbers) #f]
        [(cons? numbers) (let [(res (pair-sum-to-n (rest numbers) (- n (first numbers))))]
                           (if res
                               (cons (first numbers) res)
                               (triplet-sum-to-n (rest numbers) n)))]))

(define (pair-sum-to-n numbers n)
  (cond [(empty? numbers) #f]
        [(cons? numbers) (let [(res (sums-to-n (first numbers) (rest numbers) n))]
                           (if res
                               (list (first numbers)  res)
                               (pair-sum-to-n (rest numbers) n)))]))

(define (sums-to-n x numbers n)
  (cond [(empty? numbers) #f]
        [(cons? numbers) (if (= (+ (first numbers) x) n)
                             (first numbers)
                             (sums-to-n x (rest numbers) n))]))



(day1 "input-day1.txt")