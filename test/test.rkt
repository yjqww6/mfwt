#lang racket/base
(require "../main.rkt" rackunit racket/port racket/list)
(define-syntax check
  (syntax-rules ()
    [(_ expr ...) (begin (check-equal? expr (mfwt expr)) ...)]))
  
(check (map + '(1 2 3) (filter odd? (range 6))
            (filter even? (map + (range 3) (range 3))))
  
       (filter odd? (map add1 (filter even? (map + (range 3) (range 3)))))

       (map * (filter even? (range 6)) (filter odd? '(1 2 3 4 5 6)))

       (map + '(1 2 3))

       (filter odd? '(1 2 3))

       (map cons
            (map cons
                 (filter (compose even? car) (map cons (range 6) (range 6)))
                 (filter (compose odd? car) (map cons (range 6) (range 6))))
            (map cons
                 (filter (compose odd? car) (map cons (range 6) (range 6)))
                 (filter (compose even? car) (map cons (range 6) (range 6)))))

       (foldl (lambda (a b result)
                (* result (- a b)))
              1
              '(1 2 3)
              '(4 5 6))

       (foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))

       (andmap positive? '(1 2 3))
       (andmap positive? '(1 -2 a))
       (andmap + '(1 2 3) '(4 5 6))

       (ormap eq? '(a b c) '(a b c))
       (ormap positive? '(1 2 a))
       (ormap + '(1 2 3) '(4 5 6))

       )
(check-equal? (with-output-to-string
                  (λ ()
                    (for-each displayln (map add1 (filter odd? (range 10))))))
              (with-output-to-string
                  (λ ()
                    (mfwt (for-each displayln (map add1 (filter odd? (range 10))))))))
