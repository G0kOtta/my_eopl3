#lang eopl
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))

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

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar (list-of symbol?))
   (body expression?)
   (saved-env environment?)))

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))


(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (var val saved-env)
                  (if (eqv? search-sym var)
                      val
                      (apply-env saved-env search-sym)))
      (extend-env-rec (p-name b-vars p-body saved-env)
                      (if (eqv? search-sym p-name)
                          (proc-val (procedure b-vars p-body env))          
                          (apply-env saved-env search-sym))))))

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

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

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
        
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))

      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (map (lambda (x) (value-of x env)) rands)))
                  (apply-procedure proc arg)))

      (letrec-exp (p-name b-vars p-body letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-name b-vars p-body env)))

      )))

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env
         (car vars)
         (car vals)
         (extend-env* (cdr vars) (cdr vals) env)))))

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env* var arg saved-env))))))

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
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      identifier "(" (separated-list identifier ",") ")" "=" expression
      "in" expression)
     letrec-exp)
      
    ))
  
  
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
  
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))