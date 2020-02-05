;;; SICP notes - Ch1

;; Setup is MIT/GNU v9.2 running in Emacs via Geiser
;; ETA switched to Guile scheme for now as it has nicer ,trace

;;; 1.1.2 - at the end of this section
;; 'It should be clear that the possibility of associating values with symbols and
;; later retrieving them means that the interpreter must maintain some sort of memory
;; that keeps track of the name-object pairs. This memory is called the environment'

;;; 1.1.3 Evaluating combinations - cf William Byrd 'Most Beautiful Program' at
;; https://gist.github.com/lazywithclass/6af94f652cd59796e9592a5ea5772d17

;; this evaluates just fine with C-c C-c or C-x C-e etc
(define (square x)
  (* x x))

;; key point is the role of the environment in determining the meaning of symbols
;; Then notes that the special forms like define are not such combinations

(define (sum-of-squares x y)
  (+ (square x) (square y)))


;; ex 1.3
(define (sum-two-largest-squares x y z)
  (cond
   ((and (< x y) (< x z)) (sum-of-squares y z))
   ((and (< y x) (< y z)) (sum-of-squares x z))
   ((and (< z x) (< z y)) (sum-of-squares x y))
   ))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; now do it for cubes

(define (cubert-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cubert-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)) ; Newton's formula for improved approximation of cubert

(define (cube-good-enough? guess x)
  (< (abs (- guess (cube-improve guess x))) 0.001))

(define (cubert x)
  (cubert-iter 1.0 x))

;; 1.9 Peano arithmetic

(define (inc x) (+ 1 x))

(define (dec x) (- x 1))

(define (p+ a b)
  (if (= a 0)
      b
      (inc (p+ (dec a) b)))) ;; if the function is compound, we evaluate the return expression using arguments as params

;; 1.10 Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1)))))) ;; same rule as above, only this one goes all over the place before stack unwinds

;; Ex 1.11

(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))

(define (fi-iter a b c count)
  (if (= count 0)
      a
      ;;  (+ (fi-iter (- n 1)) (* 2 (fi-iter (- n 2))) (* 3 (fi-iter (- n 3))))
      ;; assume we initialise a b c correctly when we call it, which feels ugly
      (fi-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

(define (fi n)
  (fi-iter 0 1 2 n))

(define (pascal row col)
  ;; assume representation where all columns justified to the left margin, so row 1 has 1 column,  row 5 has 5 columns
  (cond ((< row col) 0)
        ((or (= row col) (= col 1)) 1)
        (#t (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
