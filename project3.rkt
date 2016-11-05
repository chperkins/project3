;;; ========================================
;;;  CSC 173, Fall 2016
;;;  Project 3 file
;;; ========================================

(display "\n         CS173 Project 3, Fall 2016\n")
(display "\n         Charlie Perkins\n\n")

(newline)
(newline)


(display "Problem 1: Printing Perfect Numbers\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contract: (find-factors num) -> integer
;; Input:    num: positive integer >= 2
;; Purpose:  returns the sum of the factors of num other than itself

;; Function definition:
(define find-factors
  (lambda (num)
    ;creates local environment
    (local
      ([define fac-loc
         ;uses num2 and acc1 locally
         ;num2 is a divisor. acc1 is a sum of factors
         ;acc is a form of dynamic programming. it saves the past values.
         (lambda (num1 num2 acc1)
           (cond
             ;base case. if num2> num1/2, then num2 can't be a factor
             ;end the function
             [(> num2 (/ num1 2))
              acc1]
             ;recursive case 1. if num2 divides num1, add to sum of factors
             [(integer? (/ num1 num2)) 
              (fac-loc num1 (+ 1 num2) (+ num2 acc1))]
             ;recursive case 2. if num2 doesn't divide num1, don't add it.
             ;just call fac-loc for num2+1
             [else (fac-loc num1 (+ 1 num2) acc1)]))])
      ;local call
      (fac-loc num 1 0))))

;; Contract: (a-d-or-p num) -> "Abundant", "Deficient", or "Perfect"
;; Input:    num: positive number >= 2
;; Purpose:  returns whether num is abundant, deficient, or perfect

(define a-d-or-p
  (lambda (num)
    ;stores the sum of the factors so that we make this call only once
    ;this is a form of memoization, although a really basic one
    (define val (find-factors num))
    (cond
      ;case 1
      ; sum is greater than number. abundant
      [(> val num)
       "Abundant"]
      ;case 2
      ; sum is less than number. deficient
      [(< val num)
       "Deficient"]
      ;case 3
      ; sum equals number. perfect
      [(= val num)
       "Perfect"])))

;; Contract: (print-perfect-nums num) -> void
;; Input:    num: integer >= 0
;; Purpose:  prints "n: perfect" for the the first "num"
;; perfect numbers.

(define print-perfect-nums
  (lambda (num)
    ;title for function
    (printf "\nThe first ~a perfect numbers \n" num)
    ;creates local environment
    (local
      ([define print-loc
         ;uses num2 locally
         ;num1 is amount of numbers to print
         ;num2 is current number evaluated by a-d-or-p
         (lambda (num1 num2)
           (cond
             ;ends when num1=0. Then no conditions are satisfied.
             
             ;recursive case 1. checks if number is perfect. if so,
             ;prints it.
             [(and (> num1 0) (equal? (a-d-or-p num2) "Perfect"))
              (printf "~a: ~a \n" num2 "Perfect")
              ;recursive call. decrements num1, increments num2
              (print-loc (- num1 1) (+ num2 1))]
             ;recursive case 2. number not perfect. move up one.
             [(> num1 0) (print-loc num1 (+ num2 1))]))])
      ;local call
      (print-loc num 2))))

; Post-function tests:
(print-perfect-nums 0)
(print-perfect-nums 1)
(print-perfect-nums 2)
(print-perfect-nums 3)

(display "\n\nProblem 2: Printing All Numbers\n\n")

;; Contract: (print-all-n top) -> void
;; Input:    top: positive integer >= 2
;; Purpose:  prints "n: quality" for each n<=top
;; where quality is abundant, deficient, or perfect

(define print-all-n
  (lambda (top)
    (cond
      ;only 1 case, when top>=2
      [(>= top 2) 
       ;prints the statement of number
       (printf "~a: ~a \n" top (a-d-or-p top))
       ;recursive call on top-1
       (print-all-n (- top 1))])))

;; Post-function tests:
(print-all-n 1000)

(newline)

(newline)