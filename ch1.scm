;;; SICP notes - Ch1

;; Setup is MIT/GNU v9.2 running in Emacs via Geiser
;; ETA switched to Guile scheme for now as it has nicer debug

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
