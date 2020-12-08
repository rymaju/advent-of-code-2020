#lang racket
(define (day-8-1 filename)
  (run (tokenize (file->lines filename)) 0 0 '()))

(define (day-8-2 filename)
  (run/v2 (tokenize (file->lines filename)) 0 0 '() #f))

(define (tokenize lines)
  (map (λ (x) (let* [(split (string-split x))
                     (instruction (first split))
                     (amt (string->number (second split)))]
                `(,instruction ,amt))) lines))

(define (run tokens acc i seen)
  ;(println `(,acc ,i ,seen))
  (if (or (>= i (length tokens))
          (member i seen))
      acc
      (match (list-ref tokens i)
        [`("nop" ,x) (run tokens acc (+ 1 i) (cons i seen))]
        [`("acc" ,x) (run tokens (+ acc x) (+ 1 i) (cons i seen))]
        [`("jmp" ,x) (run tokens acc (+ x i) (cons i seen))])))

(define (run/v2 tokens acc i seen swapped)
  ;(println `(,acc ,i ,seen))
  (if (or (>= i (length tokens))
          (member i seen))
      (and (>= i (length tokens)) acc)
      (match (list-ref tokens i)
        [`("nop" ,x) (or (run/v2 tokens acc (+ 1 i) (cons i seen) swapped)
                         (and (not swapped)
                              (run/v2 tokens acc (+ x i) (cons i seen) #t)))]
        [`("acc" ,x) (run/v2 tokens (+ acc x) (+ 1 i) (cons i seen) swapped)]
        [`("jmp" ,x) (or (run/v2 tokens acc (+ x i) (cons i seen) swapped)
                         (and (not swapped)
                              (run/v2 tokens acc (+ 1 i) (cons i seen) #t)))])))
  
(day-8-1 "input-day8.txt")

(day-8-2 "input-day8.txt")
