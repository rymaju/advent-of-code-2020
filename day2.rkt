#lang racket

(define (day2-1 filename)
  (count valid-line? (file->lines filename)))

(define (valid-line? line)
  (let* [(tokens (string-split line " "))
         (range (map string->number (string-split (first tokens) "-")))
         (occur-min (first range))
         (occur-max (second range))
         (ch (string-ref (second tokens) 0))
         (pswd (third tokens))
         (pswd-list (string->list pswd))
         (ch-occurances (count (λ (pswd-ch) (equal? pswd-ch ch)) pswd-list))]
    (and (>= ch-occurances occur-min)
         (<= ch-occurances occur-max))))


(define (day2-2 filename)
  (count valid-line?/v2 (file->lines filename)))

(define (valid-line?/v2 line)
  (let* [(tokens (string-split line " "))
         (range (map string->number (string-split (first tokens) "-")))
         (pos1 (sub1 (first range)))
         (pos2 (sub1 (second range)))
         (ch (string-ref (second tokens) 0))
         (pswd (third tokens))
         (l (string-length pswd))]
    (and (< pos1 l)
         (< pos2 l)
         (xor (equal? (string-ref pswd pos1) ch)
              (equal? (string-ref pswd pos2) ch)))))

(day2-1 "input.txt")
(day2-2 "input.txt")
