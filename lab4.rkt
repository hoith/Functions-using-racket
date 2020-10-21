#!/usr/bin/racket
#lang racket

(define (f lst)
; (a) ; defines the function 
(if (null? lst)
	; (b) ; checks if the lst is empty
	'()
	; (c) ; compares it to the empty list
	(cons (+ 1 (car lst))  (f (cdr lst)))))
;output = '(4 2 5 2 6 10)
;the function f adds one to each element of the list 
;'(3 1 4 1 5 9)
;'(4 1 4 1 5 9)
;'(4 2 4 1 5 9)
;'(4 2 5 1 5 9)
;'(4 2 5 2 5 9)
;'(4 2 5 2 6 9)
;'(4 2 5 2 6 10)

(define (member? x lst)
  (cond ((null? lst) #f) ;check if the list is empty
        ((eq? x (car lst)) #t) ; check if x is equal to the first part of the list 
        (#t (member? x (cdr lst))))) ; repeat until the condition isnt satisfied

(define (set?  lst)
	; complete this function definition
  (cond ((null? lst) #t)  ; if this condition is passed then it is true 
        ((member? (car lst) (cdr lst)) #f) ; for second compares the begining to the end ands sees if they are the same or not
        (else (set? (cdr lst))))) ; recursive call for the end 

(define (union lst1  lst2)
  (cond ((null? lst1) lst2) ;check if the list is empty 
  ((member? (car lst1) lst2) 
  (union (cdr lst1) lst2)) ; recursive call between end of list and begiing of list 
  (else (cons (car lst1) (union (cdr lst1) lst2))) ; construct the union with another recursive call
  )
)


(define (difference lst1 lst2)
  (cond ((null? lst1) '()) ; check list compare to empty list 
        ((member? (car lst1) lst2) ; check the memebers to using our function
         (difference (cdr lst1) lst2)) ; recursive call with the end of list 1 with list 2 
        (else (cons (car lst1) (difference (cdr lst1) lst2))))) ; construct the list in similar way to the union function 



(define (intersect lst1 lst2)
; complete this function definition
  (if (null? lst1)'() ; compare with empty 
      (if (member? (car lst1) lst2) ; check members 
          (cons (car lst1) (intersect (cdr lst1) lst2)) ; construct similar to previous 
          (intersect (cdr lst1) lst2)))) ; recursive call  

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(let loop ()
	(define line (read-line (current-input-port) 'any))
	(if (eof-object? line)
		(display "")
		(begin (print (eval (read (open-input-string line)) ns)) (newline) (loop))))
