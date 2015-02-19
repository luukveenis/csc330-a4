
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (s)
         (string-append s suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define cat-then-dog
  (letrec ([f (lambda (x)
                (let* ([next (if (equal? x "cat.jpg") "dog.jpg" "cat.jpg")])
                  (cons x (lambda () (f next)))))])
    (lambda () (f "cat.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x))
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (define (aux count)
    (if (>= count (vector-length vec))
        #f
        (let ([ref (vector-ref vec count)])
          (if (and (pair? ref) (equal? (car ref) v))
              ref
              (aux (+ count 1))))))
  (aux 0))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [current 0])
    (lambda (v)
      (let ([cache-lookup (vector-assoc v cache)])
        (if cache-lookup
            (cdr cache-lookup)
            (let ([result (assoc v xs)])
              (begin (vector-set! cache current (cons v result))
                     (set! current (if (= n (+ current 1)) 0 (+ current 1)))
                     result)))))))
