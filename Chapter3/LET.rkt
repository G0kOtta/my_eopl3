#lang eopl

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (list-val
   (lst list?))
  (emptylist-val)
  )

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (emptylist-val () '())
      (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))
  
(define empty-env-record? null?)
  
(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (empty-env-record)))
  
(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of-let
  (lambda (vals env)
    (if (null? vals)
        '()
        (cons (value-of (car vals) env)
              (value-of-let (cdr vals) env)))))

(define extend-envs
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-envs
         (cdr vars)
         (cdr vals)
         (extend-env
          (car vars)
          (car vals)
          env)))))


(define extend-env-let
  (lambda (vars vals env)
    (let ((ls (value-of-let vals env)))
      (let loop ((vars vars) (vals ls) (env env))
        (if (null? vars)
            env
            (loop (cdr vars) (cdr vals) (extend-env (car vars) (car vals) env)))))))

(define extend-env-let*
  (lambda (vars vals env)
    (if (null? vals)
        env
        (let ((val (value-of (car vals) env)))
          (extend-env-let*
           (cdr vars)
           (cdr vals)
           (extend-env (car vars) val env))))))
             

           
            
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (addition-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (num-val
                         (+ num1 num2)))))
      (multi-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 env))
                       (val2 (value-of exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (* num1 num2)))))
      (quotient-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (num-val
                         (quotient num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))           
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))

      (let-exp (vars vals body)
               (let ((vars vars)
                     (vals vals))
                 (value-of body
                           (extend-env-let vars vals env))))

      (let*-exp (vars vals body)
                (let ((vars vars)
                      (vals vals))
                  (value-of body
                            (extend-env-let* vars vals env))))
                      
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (num-val (- num1)))))

      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (= num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))
      (greater?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (> num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))
      (less?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (< num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))

      (cons-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((ls2 (expval->list val2)))
                      (list-val (cons val1 ls2)))))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ls1 (expval->list val1)))
                   (car ls1))))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ls1 (expval->list val1)))
                   (cdr ls1))))
      (null?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((ls1 (expval->list val1)))
                   (if (null? ls1)
                       (bool-val #t)
                       (bool-val #f)))))
      (emptylist-exp ()
                     (emptylist-val))

      (list-exp (exps)
                (list-val (map (lambda (e) (value-of e env)) exps)))

      (unpack-exp (vars exp1 body)
                  (let ((val1 (value-of exp1 env)))
                    (let ((ls1 (expval->list val1)))
                      (value-of body (extend-envs vars ls1 env))))) 

      (cond-exp (exp1 exp2)
                (let loop ((exp1 exp1)
                           (exp2 exp2))
                  (if (null? exp1)
                      (eopl:error 'value-of "All cond tests failed.")
                      (let ((condition (value-of (car exp1) env)))
                        (if (expval->bool condition)
                            (value-of (car exp2) env)
                            (loop (cdr exp1) (cdr exp2)))))))
                

           
                           
 
                   
      )))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))
  
(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("+" "(" expression "," expression ")")
     addition-exp)

    (expression
     ("*" "(" expression "," expression ")")
     multi-exp)

    (expression
     ("/" "(" expression "," expression ")")
     quotient-exp)
      
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)

    (expression
     ("let*" (arbno identifier "=" expression) "in" expression)
     let*-exp)

    (expression
     ("minus" "(" expression ")")
     minus-exp)

    (expression
     ("equal?" "(" expression "," expression ")")
     equal?-exp)

    (expression
     ("greater?" "(" expression "," expression ")")
     greater?-exp)

    (expression
     ("less?" "(" expression "," expression ")")
     less?-exp)
    
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    (expression
     ("car" "(" expression")")
     car-exp)

    (expression
     ("cdr" "(" expression")")
     cdr-exp)

    (expression
     ("null?" "(" expression")")
     null?-exp)

    (expression
     ("emptylist")
     emptylist-exp)

    (expression
     ("list" "(" (arbno expression) ")")
     list-exp)

    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)

    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression)
     unpack-exp)
    
    ))
  
;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
  
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


;;;test
;;;(value-of-program (scan&parse "if 5 then 1 else 0"))
;;;(value-of-program (scan&parse "let x = 4 in list(x -(x,1) -(x,3))"))
                  
      
