#lang racket/base
(require racket/list
         (for-syntax racket/base racket/function racket/generic
                     racket/list racket/match racket/syntax))
(provide mfwt)

(begin-for-syntax
  (define-generics node
    (node-dep* node)
    (node-out node))

  (struct Map (f in* out dep* children)
    #:methods gen:node
    [(define (node-dep* m) (Map-dep* m))
     (define (node-out m) (Map-out m))])
  (struct Filter (pred in dep* failer children)
    #:methods gen:node
    [(define (node-dep* m) (Filter-dep* m))
     (define (node-out m) (Filter-in m))])
  (struct Source (id me null? car cdr)
    #:methods gen:node
    [(define (node-dep* m) (list (Source-id m)))
     (define (node-out m) (Source-me m))])

  (define (mark stx)
    (syntax-case stx (filter map range)
      [(filter pred ls)
       (let-values ([(failer) (gensym 'failer)]
                    [(p) (gensym 'p)]
                    [(node def src) (mark #'ls)])
         (values (Filter p (node-out node) (node-dep* node) failer node)
                 (cons (cons p #'pred) def)
                 src))]
      [(map f ls ls* ...)
       (let ([p (gensym 'p)]
             [o (gensym 'o)])
         (define-values (def src in dep children)
           (for/fold ([def (list (cons p #'f))]
                      [src '()]
                      [in '()]
                      [dep '()]
                      [children '()])
                     ([ls (in-list (syntax->list #'(ls ls* ...)))])
             (define-values (n d s) (mark ls))
             (values (append d def)
                     (append s src)
                     (cons (node-out n) in)
                     (append (node-dep* n) dep)
                     (cons n children))))
         (values (Map p (reverse in) o (reverse dep) (reverse children))
                 (reverse def)
                 (reverse src)))]
      [(range stop)
       (mark #'(range 0 stop 1))]
      [(range start stop)
       (mark #'(range start stop 1))]
      [(range start stop step)
       (let ([s (gensym 's)]
             [me (gensym 'me)]
             [i (gensym 'i)]
             [e (gensym 'e)]
             [d (gensym 'd)])
         (define here (Source s me #`(>= #,s #,e) s #`(+ 1 #,s)))
         (values here
                 (list (cons i #'start) (cons e #'stop) (cons d #'step))
                 (list (list s i here))))]
      [expr
       (let ([l (gensym 'l)]
             [s (gensym 's)]
             [me (gensym 'me)])
         (define here (Source s me #`(null? #,s) #`(car #,s) #`(cdr #,s)))
         (values here
                 (list (cons l #'expr))
                 (list (list s l here))))]))

  
  (define ((step src) id)
    (cond
      [(assq id src)
       =>
       (λ (slot)
         (match (third slot)
           [(Source _ _ _ _ cdr) cdr]))]))

  (define (generate node src kt kf knull)
    (match node
      [(Filter p in dep* failer node)
       #`(let #,failer #,(map (λ (x) #`(#,x #,x)) dep*)
           #,(generate
              node src
              (λ ()
                #`(if (#,p #,in)
                      #,(kt)
                      (#,failer #,@(map (step src) dep*))))
              kf knull))]
      [(Map f in* out dep* children)
       (let loop ([children children])
         (if (null? children)
             #`(let ([#,out (#,f  #,@in*)]) #,(kt))
             (generate (car children) src
                       (λ () (loop (cdr children)))
                       kf knull)))]
      [(Source _ me null? car _)
       #`(if #,null?
             #,knull
             (let ([#,me #,car]) #,(kt)))]))

  (define (sink src children dep* d comp knull)
    (define/with-syntax ((l . e) ...) d)
    (match-let ([(list (list s i _) ...) src])
           (with-syntax ([(ss ...) s]
                         [(ii ...) i])
             #`(let ([l e] ...)
                 (let all-loop ([ss ii] ...)
                   #,(let cloop ([children children])
                       (if (null? children)
                           (comp #`(all-loop #,@(map (step src) dep*)))
                           (generate (car children)
                                     src
                                     (λ () (cloop (cdr children)))
                                     (const #`(all-loop #,@(map (step src) dep*)))
                                     knull))))))))
  
  (define (trans stx)
    (syntax-case stx (map filter for-each foldl foldr andmap ormap)
      [(map f ls ls* ...)
       (let-values ([(n d src) (mark stx)])
         (match-define (Map fun in* _ dep* children) n)
         (sink src children dep* d (λ (d) #`(cons (#,fun #,@in*) #,d)) #''()))]
      
      [(for-each f ls ls* ...)
       (let-values ([(n d src) (mark #'(map f ls ls* ...))])
         (match-define (Map fun in* _ dep* children) n)
         #`(begin 
             #,(sink src children dep* d (λ (d) #`(begin (#,fun #,@in*) #,d)) #''())
             (void)))]

      [(ormap f ls ls* ...)
       (let-values ([(n d src) (mark #'(map f ls ls* ...))])
         (match-define (Map fun in* _ dep* children) n)
         (sink src children dep* d (λ (d) #`(or (#,fun #,@in*) #,d)) #'#f))]

      [(andmap f ls ls* ...)
       (let-values ([(n d src) (mark #'(map f ls ls* ...))])
         (match-define (Map fun in* _ dep* children) n)
         (define i (gensym 'i))
         (define l (gensym 'l))
         (define s (Source l l #`(not #,l) l #`(#,fun #,@in*)))
         (sink (cons (list l i s) src)
               (cons s children) (cons l dep*) (cons (cons i #'#t) d) values l))]
      
      [(foldr f init ls ls* ...)
       (let-values ([(n d src) (mark #'(map f ls ls* ...))])
         (match-define (Map fun in* _ dep* children) n)
         (define i (gensym 'i))
         (sink src children dep* (cons (cons i #'init) d)
               (λ (d) #`(#,fun #,@in* #,d)) i))]
      
      [(foldl f init ls ls* ...)
       (let-values ([(n d src) (mark #'(map f ls ls* ...))])
         (match-define (Map fun in* _ dep* children) n)
         (define i (gensym 'i))
         (define l (gensym 'l))
         (sink (cons (list l i (Source l l #'#f l #`(#,fun #,@in* #,l))) src)
               children (cons l dep*) (cons (cons i #'init) d) values l))]
      
      [(filter pred ls)
       (trans #'(map values (filter pred ls)))])))

(define-syntax (mfwt stx)
  (syntax-case stx ()
    [(flat m) (syntax-protect (trans #'m))]))