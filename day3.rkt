#lang racket

(define TREE #\#)

(define (day-3-1 filename)
  (count-trees 3 1 (file->lines filename)))

(define (count-trees x-slope y-slope map)
  (count-trees-help x-slope y-slope 0 0 map (string-length (first map)) (length map)))


(define (count-trees-help x-slope y-slope cur-x cur-y map width height)
  (if (< cur-y height)
      (+ (if (equal? (get-pos cur-x cur-y map width) TREE) 1 0)
         (count-trees-help x-slope y-slope
                           (+ x-slope cur-x)
                           (+ y-slope cur-y)
                           map
                           width
                           height))
      0))

(define (get-pos x y map width)
  (string-ref (list-ref map y) (modulo x width)))

;(day-3-1 "input-day3.txt")

(define (day-3 filename x y)
  (count-trees x y (file->lines filename)))

(apply *
       (list
        (day-3 "input-day3.txt" 1 1)
        (day-3 "input-day3.txt" 3 1)
        (day-3 "input-day3.txt" 5 1)
        (day-3 "input-day3.txt" 7 1)
        (day-3 "input-day3.txt" 1 2)))



