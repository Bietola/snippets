; Let's add some numbers
(+ 2 2)

; Naive factorial (let ()
  (define (fac n)
    (if (= n 0)
      1
      (* n (fac (- n 1)))
      ))
  ; Test
  (fac 3))

; Tail recursive Factorial
(let ()
  (define (fac n)
    (define (fac-impl n acc)
      (if (= n 0)
        acc
        (fac-impl (- n 1) (* acc n))))
    (fac-impl n 1))
  ; Test
  (list (fac 1) (fac 4)))

; Naive `foldl`
; NB. Should be naturally tail recursive... yhei? 
(let ()
  (define (my-foldl fun z lst)
    (if (null? lst)
      z
      (my-foldl fun 
                (fun z (car lst)) 
                (cdr lst))))
  ; Test
  (my-foldl / 1 '(1 2 3 4)))

; Naive `foldr`
; NB. Not tail-recursive
(let ()
  (define (my-foldr fun z lst)
    (if (null? lst)
      z
      (fun
        (car lst)
        (my-foldr fun z (cdr lst)))))
  ; Test
  (my-foldr / 1 '(1 2 3 4)))

; "While loop" using the `let` statement
(let loop ((cnt 10))
  (unless (<= cnt 0)
   (println cnt)
   (loop (- cnt 1))))

; `while` macro
(let ()
 (define-syntax while
   (syntax-rules ()
     ((_ cnd body ...)
      (let loop ()
        (when cnd
          body ...
          (loop))))))
 ; Test
 (define cnt 10)
 (while (> cnt 0)
        (println cnt)
        (set! cnt (- cnt 1))))

; `let*` macro
(let ()
  (define-syntax my-let*
    (syntax-rules ()
      ; Base case
      ((_ ((idn val)) body ...)
       (let ((idn val))
         body ...))
      ; General case
      ((_ ((idn val) . rst) body ...)
       (let ((idn val))
         (my-let* rst
                  body ...)))))
  ; Test
  (my-let* ((x 1)
            (y (+ x 1)))
           (cons x y)))

; `let` macro
(let ()
  (define-syntax my-let
    (syntax-rules ()
      ((_ ((id v) ...) body ...)
       ((lambda (id ...) body ...) v ...))))
  ; Test
  (my-let ((x 1)
           (y 2))
          (cons x y)))

; Example usage of for-each
(for-each (lambda (x) (println x)) '(1 2 3))

; `vector-for-each`
(begin
  (define (vector-for-each fun vec)
    (let loop ((idx 0))
      (when (< idx (vector-length vec))
        (fun (vector-ref vec idx))
        (loop (+ idx 1)))))
  ; Test
  (vector-for-each (lambda (x) (println x)) #(1 2 3)))

; Example `case` usage (O(n) mod 2 predicate)
(begin
  (let loop ((n 21))
    (case n
      ((0) 'even)
      ((1) 'odd)
      (else (loop (- n 2))))))

; `struct` example
(begin
 (struct being (
                name
                (age #:mutable)
                ))
 (define (being-show b)
   (set-being-age! b (+ 1 (being-age b)))
   (display (being-name b))
   (display " (")
   (display (being-age b))
   (display ")"))
 (define me (being "me" 19))
 (being-show me) (newline)
 (being-show me) (newline)
 (being-show me) (newline))

; Inheritance
(begin
  (struct being (
                 name
                 (age #:mutable)
                 ))
  (struct may-being being (
                           (alive? #:mutable)
                           ))
  (define (say-hello b)
    (cond
      [(not (being? b)) "it cannot speak"]
      [(and (may-being? b) (not (may-being-alive? b)))
       "it used to speak"]
      [else "hello!"]))
  (say-hello (cons "John" 10)))

; Closure application: iterator
(begin
  (define (vector-iter vec)
    (let ((idx 0)
          (len (vector-length vec)))
      (lambda ()
        (if (< idx len)
          (let ((next (vector-ref vec idx)))
            (set! idx (+ idx 1))
            next)
          '<<end>>))))
  ; Test
  (define i (vector-iter #(2 4 5)))
  (println (i))
  (println (i))
  (println (i))
  (println (i))
  (println (i)))

; Random continuations things
; Or: 2 + 2 according to big-brother
(begin
  (define double-think null)
  (+
    2
    (call/cc
      (lambda (cc)
        (set! double-think cc)
        2)))
  (double-think 3))

; Slide """"simple"""" continuation example
(begin
  (define saved-cont null)
  (define (test-cont)
    (let ((x 0))
      (call/cc
        (lambda (cc)
          (set! saved-cont cc)))
      ;
      (println x)
      (set! x (+ 1 x))))
  ; Test
  (test-cont)
  (saved-cont)
  (test-cont)
  (saved-cont)
  (saved-cont)
  (define other-cont saved-cont)
  (test-cont)
  (saved-cont)
  (other-cont))

; `For` macro with `break` and `continue`
(begin
  (define-syntax For
    (syntax-rules (from to in break: continue:)
      ((_ itr from beg to end
          break: break-kw
          continue: continue-kw
          in body ...)
       (begin
         (call/cc
           (lambda (break-kw)
             (let loop ((itr beg)
                        (incr (if (> end beg) + -)))
               (call/cc
                (lambda (continue-kw)
                 body ...))
               (unless (= itr end)
                 (loop (incr itr 1) incr)))))))))
  ; Test
  (define acc 0)
  (For i from 0 to 10
       break: get-out 
       continue: get-on
       in
       (set! acc (+ acc i))
       (when (= i 8) (get-out)))
  (println acc))

(begin

  (define *handlers* (list))

  (define (push-handler handler)
    (set! *handlers* (cons handler *handlers*)))

  (define (pop-handler)
    (let ((popped (car *handlers*)))
      (set! *handlers* (cdr *handlers*))
      popped))

  (define (throw throwable)
    (if (pair? *handlers*)
      ((pop-handler) throwable)
      (error throwable)))

  (define-syntax try
    (syntax-rules (catch)
      ([_ tbody ...
          (catch to-catch cbody ...)]
       [(call/cc
          (lambda (cc)
            ; Register handler to substitute try body with catch body
            ; in case of error.
            (push-handler (lambda (throwable)
                            (if (equal? to-catch throwable)
                              (cc (begin
                                    cbody ...))
                              (throw throwable))))
            ; Try body.
            (let ((res
                    (begin
                      tbody ...)))
              ; Local catch handler never used and no longer needed.
              (pop-handler)
              res)))])))

  ; Test
  (try
    (println "hello")
    (catch "hello"
           (println "hello caught!")
           (println "hello caught!")))

  )

; Information hiding implemented with closure.
(begin
  (define (make-guy name)
    (let ((m-name name)
          (m-age 0))
      ; Methods:
      (define (get-age)
        m-age)
      (define (get-name)
        m-name)
      (define (grow-for years)
        (set! m-age (+ m-age years)))
      (define (say-hi)
        (printf "hello, I am ~a, and I am ~a years old!~n" m-name m-age))
    ; Dispatcher:
    (lambda (msg . args)
      (apply
        (case msg
          [(get-age) get-age]
          [(get-name) get-name]
          [(grow-for) grow-for]
          [(say-hi) say-hi]
          [else (error "Unknown method!")])
        args))))
  ; Test:
  (define billy (make-guy "Billy"))
  (billy 'say-hi)
  (billy 'grow-for 2)
  (billy 'say-hi)
  (billy 'grow-for 10)
  (billy 'say-hi))
