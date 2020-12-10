#lang racket

(define ANS-9-1 18272118)
;(define ANS-9-1 127)

(define (day-9-1 filename)
  (let* [(lines (map string->number (file->lines filename)))]  
    (and-pairsum-map (take lines 25) (drop lines 25))))

(define (and-pairsum-map acc lines)
  (or (empty? lines)
      (and
       (println (first lines)) ;hacky, but i didnt sleep -_-
       (pair-sum-to acc (first lines))
           
           (and-pairsum-map (cons (first lines)
                                  (if (< (length acc) 25)
                                      acc
                                      (take acc 24)))
                            (rest lines)))))

(define (pair-sum-to l n)
  (ormap (λ (x) (ormap (λ (y) (= n (+ x y))) l)) l))

(define (day-9-2 filename)
  (let* [(lines (map string->number (file->lines filename)))]  
    (running-sum lines)))

(define (running-sum l)
  (and (cons? l)
       (let* [(res (running-sum/acc l 0 (first l) (first l)))]
         (or res (running-sum (rest l))))))

(define (running-sum/acc l acc start m)
  (cond 
        [(empty? l) #f]
        [(= (+ acc (first l)) ANS-9-1)
         ;(println `(,start ,m))
         (+ start m)]
        [else (running-sum/acc (rest l) (+ acc (first l)) start (max (first l) m))]))

;(day-9-1 "input-day9.txt")
(day-9-2 "input-day9.txt")