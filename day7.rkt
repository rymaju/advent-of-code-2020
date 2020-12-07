#lang racket


;Graph is a list of (name ((name count) (name count) (name count) ...))

(define (day-7-1 filename)
  (let* [(graph (map parse-line (file->lines filename)))]
    (count (λ (x) (can-hold? graph (first x) "shiny gold")) graph)))

(define (day-7-2 filename)
  (let* [(graph (map parse-line (file->lines filename)))]
    (bag-sum graph "shiny gold")))

(define (parse-line line)
  (let*[(front-and-back (string-split line " contain "))
        (name (substring (first front-and-back) 0 (- (string-length (first front-and-back)) 5)))
        (bag-strings (string-split (substring (second front-and-back)
                                              0
                                              (sub1 (string-length (second front-and-back))))
                                   ", "))
        (bags (filter (λ (x) (not (equal? x '(" other" #f)))) (map parse-bag-desc bag-strings)))]
    `(,name ,bags)))

(define (parse-bag-desc bag)
  (let [(amt (string->number (substring bag 0 1)))]
    (if (and amt (= amt 1))
        `(,(substring bag 2 (- (string-length bag) 4)) ,amt)
        `(,(substring bag 2 (- (string-length bag) 5)) ,amt))))
    
(define (can-hold? graph bag-color bag-color-target)
  (ormap (λ (x) (or (string=? (first x) bag-color-target)
                    (can-hold? graph (first x) bag-color-target)))
         (second (assoc bag-color graph))))

(define (bag-sum graph bag-color)
  (apply +
         (map (λ (x) (+ (second x) (* (second x)
                                      (bag-sum graph (first x)))))
         (second (assoc bag-color graph)))))
  
  

(day-7-1 "input-day7.txt")
(day-7-2 "input-day7.txt")