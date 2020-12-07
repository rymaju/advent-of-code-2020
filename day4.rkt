#lang racket
(define (day-4-1 filename)
  (count (λ(x) (= (length x) 7))
         (map (λ (x) (filter (λ (y) (not (string-prefix? y "cid"))) x))
              (split-list (file->lines filename) ""))))

(define (split-list list delim)
  (cond [(empty? list) '(())]
        [else
         (let [(ans (split-list (rest list) delim))]
           (if (string=? (first list) delim)
               (cons empty ans)
               (cons (append (string-split (first list) " ") (first ans)) (rest ans))))]))


#|
(define (split-list-help list delim list-so-far)
  (cond [(empty? list) list-so-far]
        [else (if (string=? (first list) delim)
                  (split-list-help (rest list) delim (cons empty list-so-far))
                  (split-list-help (rest list) delim (cons (append (string-split (first list) " ")
                                                                   (first list-so-far))
                                                           (rest list-so-far))))]))
|#
;somehow off by one
(define (day-4-2 filename)
  (count valid-passport
         (filter (λ (x) (and (= (length x) 7)))
                 (map (λ (x) (filter (λ (y) (not (string-prefix? y "cid"))) x))
                      (split-list (file->lines filename) "")))))

(define (valid-passport passport)
  (let* [(byr (string->number (second (passport-field "byr" passport))))
         (pid (second (passport-field "pid" passport)))
         (iyr (string->number(second (passport-field "iyr" passport))))
         (eyr (string->number (second (passport-field "eyr" passport))))
         (hgt (second (passport-field "hgt" passport)))
         (hcl (second (passport-field "hcl" passport)))
         (ecl (second (passport-field "ecl" passport)))]
    (and (<= byr 2002) (> byr 1920)
         (<= iyr 2020) (>= iyr 2010)
         (<= eyr 2030) (>= eyr 2020)
         
         (> (string-length hgt) 2)
         
             #|
         (or (and (string-suffix? hgt "in")
                  (between (string->number (substring hgt 0 (- (string-length hgt) 2)))
                           59 76))
             (and (string-suffix? hgt "cm")
                  (between (string->number (substring hgt 0 (- (string-length hgt) 2)))
                           150 193)))
|#
         (regexp-match #px"((1[5-8][0-9]|19[0-3])cm)|((59|6[0-9]|7[0-6])in)" hgt)
         (regexp-match #px"#([0-9]|[a-f]){6}" hcl)
         ;(println ecl)
         (or (string=? ecl "amb")
             (string=? ecl "blu")
             (string=? ecl "brn")
             (string=? ecl "gry")
             (string=? ecl "grn")

             (string=? ecl "hzl")
             (string=? ecl "oth"))
         ;(println "got here")

         (regexp-match #px"[0-9]{9}" pid)
         #t)))

(define (between x min max)
  (and (<= x max) (> x min)))

(define (passport-field field passport)
  (assoc field (map (λ (x) (string-split x ":")) passport)))

(day-4-1 "input-day4.txt")
(day-4-2 "input-day4.txt")

;(valid-passport (string-split "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f" " "))
