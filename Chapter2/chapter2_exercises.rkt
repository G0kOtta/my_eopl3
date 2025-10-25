#lang eopl

;;;exercise 2.8
(define empty-env?
  (lambda (env)
    (null? env)))

;;;exercise 2.9
(define has-binding?
  (lambda (env s)
    (cond
      ((empty-env? env) #f)
      (else
       (if (eqv? (caar env) s)
           #t
           (has-binding? (cdr env) s))))))

;;;exercise 2.10
(define extend-env*
  (lambda (var-list val-list env)
    (let ((var (car var-list))
          (val (car val-list)))
      (extend-env*
       (cdr var-list)
       (cdr val-list)
       (cons (cons var val) env)))))

;;;exercise 2.15
(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

(define lambda-exp
  (lambda (var exp)
    (list 'lambda-exp (list var) exp)))

(define app-exp
  (lambda (rator rand)
    (list 'app-exp rator rand)))

(define var-exp?
  (lambda (exp)
    (eqv? (car exp) 'var-exp)))

(define lambda-exp?
  (lambda (exp)
    (eqv? (car exp) 'lambda-exp)))

(define app-exp?
  (lambda (exp)
    (eqv? (car exp) 'app-exp)))

(define var-exp->var
  (lambda (exp)
    (cdr exp)))

(define lambda-exp->bound-var
  (lambda (exp)
    (caadr exp)))

(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

(define app-exp->rator
  (lambda (exp)
    (cadr exp)))

(define app-exp->rand
  (lambda (exp)
    (caddr exp)))

(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

