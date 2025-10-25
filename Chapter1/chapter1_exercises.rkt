#lang eopl

;;;exercise 1.15
(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))


;;;exercise 1.16
(define invert
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else
       (cons (list (cadr (car ls))
                   (car (car ls)))
             (invert (cdr ls)))))))

;;;exercise 1.17
(define down
  (lambda (ls)
    (map (lambda (x) (list x))
         ls)))

;;;exercise 1.18
(define swapper
  (lambda (s1 s2 ls)
    (cond
      ((null? ls) '())
      ((symbol? (car ls))
       (cond
         ((eq? (car ls) s1)
          (cons s2 (swapper s1 s2 (cdr ls))))
         ((eq? (car ls) s2)
          (cons s1 (swapper s1 s2 (cdr ls))))
         (else
          (cons (car ls) (swapper s1 s2 (cdr ls))))))
      (else
       (cons
        (swapper s1 s2 (car ls))
        (swapper s1 s2 (cdr ls)))))))

;;;exercise 1.19
(define list-set
  (lambda (ls n x)
    (cond
      ((null? ls) '())
      ((eq? n 0)
       (cons x (cdr ls)))
      (else
       (cons (car ls)
             (list-set (cdr ls) (- n 1) x))))))

;;;exercise 1.20
(define count-occurrences
  (lambda (s ls)
    (cond
      ((null? ls) 0)
      ((symbol? (car ls))
       (cond
         ((eq? (car ls) s)
          (+ 1 (count-occurrences s (cdr ls))))
         (else
          (count-occurrences s (cdr ls)))))
      (else
       (+ (count-occurrences s (car ls))
          (count-occurrences s (cdr ls)))))))

;;;exercise 1.21
(define product
  (lambda (s1 s2)
    (define helper
      (lambda (x ls)
        (cond
          ((null? ls) '())
          (else
           (cons (list x (car ls))
                 (helper x (cdr ls)))))))
    (cond
      ((null? s1) '())
      (else
       (append (helper (car s1) s2)
             (product (cdr s1) s2))))))

;;;exercise 1.22
(define filter-in
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      ((pred (car ls))
       (cons (car ls)
             (filter-in pred (cdr ls))))
      (else
       (filter-in pred (cdr ls))))))
    
;;;exercise 1.23
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else
       (let ((v (list-index pred (cdr ls))))
         (if v
             (+ 1 (list-index pred (cdr ls)))
             #f))))))

;;;exercise 1.24
(define every?
  (lambda (pred ls)
    (cond
      ((null? ls) #t)
      (else
       (and (pred (car ls))
            (every? pred (cdr ls)))))))

;;;exercise 1.25
(define exists?
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      (else
       (or (pred (car ls))
           (exists? pred (cdr ls)))))))

;;;exercise 1.26
(define up
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls))
       (cons (caar ls)
             (append (cdar ls)
                     (up (cdr ls)))))
      (else
       (cons (car ls)
             (up (cdr ls)))))))

;;;exercise 1.27
(define flatten
  (lambda (ls)
    (define remove
      (lambda (x ls)
        (cond
          ((null? ls) '())
          ((pair? (car ls))
           (cons (remove x (car ls))
                 (remove x (cdr ls))))
          (else
           (if (eq? x (car ls))
               (remove x (cdr ls))
               (cons (car ls) (remove x (cdr ls))))))))
    (let ((lst (remove '() ls)))
      (cond
        ((null? lst) '())
        ((pair? (car lst))
         (append (flatten (car lst))
                 (flatten (cdr lst))))
        (else
         (cons (car lst) (flatten (cdr lst))))))))

;;;exercise 1.28
(define merge
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) ls2)
      ((null? ls2) ls1)
      ((< (car ls1) (car ls2))
       (cons (car ls1) (merge (cdr ls1) ls2)))
      (else
       (cons (car ls2) (merge ls1 (cdr ls2)))))))
      
;;;exercise 1.29
(define sort
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else
       (let ((lst (sort (cdr ls))))
         (merge (list (car ls)) lst))))))

;;;exercise 1.30
(define sort/predicate
  (lambda (pred ls)
    (define merge
      (lambda ( pred ls1 ls2)
        (cond
          ((null? ls1) ls2)
          ((null? ls2) ls1)
          ((pred (car ls1) (car ls2))
           (cons (car ls1) (merge pred (cdr ls1) ls2)))
          (else
           (cons (car ls2) (merge  pred ls1 (cdr ls2)))))))
    (cond
      ((null? ls) '())
      (else
       (let ((lst (sort/predicate pred (cdr ls))))
         (merge pred (list (car ls)) lst))))))

;;;exercise 1.31
(define interior-node
  (lambda (content lnode rnode)
    (list content lnode rnode)))

(define leaf
  (lambda (content)
    content))

(define leaf?
  (lambda (bintree)
    (not (pair? bintree))))

(define lson cadr)

(define rson caddr)

(define contents-of
  (lambda (bintree)
    (cond
      ((leaf? bintree) bintree)
      (else
       (car bintree)))))

;;;exercise 1.32
(define double-tree
  (lambda (bintree)
    (if (leaf? bintree)
        (* 2 bintree)
        (interior-node
         (contents-of bintree)
         (double-tree (lson bintree))
         (double-tree (rson bintree))))))

;;;exercise 1.33
(define mark-leaves-with-red-depth
  (lambda (bintree)
    (define mark-leaves-with-red-depth-helper
      (lambda (bintree n)
        (cond
          ((leaf? bintree) n)
          ((eq? (contents-of bintree) 'red)
           (interior-node
            'red
            (mark-leaves-with-red-depth-helper (lson bintree) (+ n 1))
            (mark-leaves-with-red-depth-helper (rson bintree) (+ n 1))))
          (else
           (interior-node
            (contents-of bintree)
            (mark-leaves-with-red-depth-helper (lson bintree) n)
            (mark-leaves-with-red-depth-helper (rson bintree) n))))))
    (mark-leaves-with-red-depth-helper bintree 0)))
    
;;;exercise 1.34
(define path
  (lambda (n bst)
    (cond
      ((null? bst) '())
      ((eq? n (car bst)) '())
      ((< n (car bst))
       (cons 'left (path n (lson bst))))
      (else
       (cons 'right (path n (rson bst)))))))

;;;exercise 1.35
(define number-leaves
  (let ((cnt -1))
    (lambda (bintree)
      (cond
        ((leaf? bintree)
         (begin
           (set! cnt (+ cnt 1))
           cnt))
        (else
         (interior-node
            (contents-of bintree)
            (number-leaves (lson bintree))
            (number-leaves (rson bintree))))))))

;;;exercise 1.36
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(define g
  (lambda (pair rest)
    (cons pair
          (map (lambda (p) (list (+ 1 (car p)) (cadr p)))
               rest))))


    