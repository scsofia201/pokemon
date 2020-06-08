(define (num n e f)
 (if (= n e) f (num n (+ e 1) (+ f 1))) )
(num (char->integer #\3) 49 1)

(char->integer #\3)


(define (num2 n e)
  (if (> n 1)
(num2 (- n 1) (+ e 1))
(integer->char e)

      ))(num2 3 49)



