
; Let's add some numbers
(+ 2 2)

; Naive factorial
(let ()
  (define (my-fact n)
    (if (= n 0)
      1
      (* n (my-fact (- n 1)))))
  (my-fact 3))

; Tail recursive factorial
(let ()
  (define (my-fact n)
    (define (my-fact-acc n acc)
      (if (= n 0)
        acc
        (my-fact-acc (- n 1) (* acc n))))
    (my-fact-acc n 1))
  (my-fact 3))

; `foldl` vs `foldr` test
(list
  (eqv?
    (foldl / 1 '(1 2 3 4 5 6))
    (foldr / 1 '(1 2 3 4 5 6)))
 (eqv?
    (foldl / 1 '(1 2 3 4 5))
    (foldr / 1 '(1 2 3 4 5))))

; "Fun" way of testing if two lists are even
(let ()
  (define (fun-even? n)
    (TODO)))

(/ 1 (/ 2 (/ 3 (/ 4 5))))

; Naive `foldl`
; NB. also naturally tail recursive... yhei!
(let ()
  (define (my-foldl fun idn lst)
    (if (null? lst)
      idn
      (my-foldl fun (fun (car lst) idn) (cdr lst))))
  (my-foldl / 1 '(1 2 3 4)))

; Naive `foldr`
(let ()
  (define (my-foldr fun idn lst)
    (if (null? lst)
      idn
      (fun (car lst) (my-foldr fun idn (cdr lst)))))
  (my-foldr / 1 '(1 2 3 4)))

; `while` macro
(let ()
  (define-syntax while
    (syntax-rules ()
      [(_ cnd body ...)
       (let continue ()
         (when cnd
           (let ()
             body ...
             (continue))))]))
  (define x 10)
  (while (> x 0)
         (let ()
           (map print `(,x -))
           (set! x (- x 1)))))

; `let*` macro
(let ()
  (define-syntax let*
    (syntax-rules ()
      ; base case
      [(_ ((v i)) b ...)
       ((lambda (v) b ... ) i)]
      [(_ ((v i) . vis) b ...)
       ((lambda (v)
          (let* vis
            b ...))
        i)]))
  (let* ((x 2) (y 1))
    (+ x y)))

; macro scope pollution test
(let ()
  (define result 2)
  (let ()
    (define-syntax let
      (syntax-rules ()
        [(_ lhs rhs)
         (+ lhs rhs)]))
    (set! result (let result 2)))
  (let ((real-result result))
    real-result))

; `let` macro
(let ()
  (define-syntax my-let
    (syntax-rules ()
      [(_ ((v i) ...) b ...)
       ((lambda (v ...)
          b ...)
        i ...)]))
  (my-let ((x 1) (y 2))
          (+ x y)))

; Tail recursive `foldr` (god save me)
(let ()
  (define (my-foldr f i lst)
    (define (foldr-impl f i lst out)
      (if (null? lst)
        (out i)
        (foldr-impl f i (cdr lst)
                    (lambda (x) (f (car lst) (out x))))))
    (foldr-impl f i lst (lambda (x) x)))
  (my-foldr / 1 '(1 2 3 4)))

; `rfoldx`: a `foldl` that actually folds right
(let ()
  (define (rfoldl f i lst)
    (if (null? lst)
      i
      (rfoldl f (f i (car lst)) (cdr lst))))
  (define (rfoldr f i lst)
    (rfoldl f i (reverse lst)))
  (cons (rfoldl remainder 1 '(1 2 3 4))
        (rfoldr remainder 1 '(1 2 3 4))))

; Simple continuation example
(begin
  (define counter #f)
  (define (start-counter)
    (let ((x 0))
     ; Initialize counter by saving a callable
     ; snapshot of the stack at this point in
     ; it
     (call/cc
       (lambda (cc)
         (set! counter cc)))
     ; Actually increment and return counter
     (set! x (+ x 1))
     (println x)))
  (start-counter)
  (counter)
  (counter)
  (counter)
  (counter)
  (define saved-counter counter)
  (start-counter)
  (saved-counter)
  (counter)
  (saved-counter))

;; Bare bone macro with no transformer
(begin
  (define-syntax (say-hello stx)
    (syntax (displayln "hello!")))
  (say-hello))

;; Transforming syntax with `syntax`
(begin
  (define-syntax (rev stx)
    (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
  (rev "Yoda" "am" "I" values))

;; the glorious For macro
(begin
  ; For debugging
  (require macro-debugger/stepper)
  ; The actual macro
  (define-syntax For
    (syntax-rules (from to break: do)
      [(_ i from beg to end break: break-kw do body ...)
       (let* ((beg1 beg)
              (end1 end)
              (i beg1)
              (incr (if (< beg1 end1) + -)))
         (call/cc
           (lambda (break-kw)
             (let loop ()
               body ...
               (set! i (incr i 1))
               (unless (= i end1) (loop))))))]))
  ; Simple example
  (displayln "example 1:")
  (For i from 1 to 10 break: get-out do
       (displayln i)
       (when (= i 5) (get-out)))
  ; Example of why we need `beg1` and `end1`
  (displayln "example 2:")
  (For i
       from (begin
              (displayln "10!")
              10)
       to (begin
            (displayln "0!")
            0)
       break: get-out do
       (displayln i)
       (when (= i 5) (get-out))))

;; For again! Whoo repetition!!
(begin
 (define-syntax For
   (syntax-rules (from to break: do)
     [(_ cntr from beg to end break: break-kw do body ...)
      (let* ((beg* beg)
             (end* end)
             (cntr beg*)
             (incr (if (< beg* end*) + -)))
        (call/cc 
          (lambda (break-kw)
            (let loop ()
              body ...
              (set! cntr (incr cntr 1))
              (unless (= cntr end*) (loop))))))]))
 ; Example
 (For i from 1 to 10 break: get-out do
      (display i)
      (newline)
      (when (= i 5) (get-out))))

;; For with continue keyword
(begin
 (define-syntax For
   (syntax-rules (from to kws: do)
     [(_ cntr from beg to end kws: break-kw continue-kw do body ...)
      (let* (; Boundary and get values of variables 
             (beg* beg)
             (end* end))
        (define continue-kw #f)
        ; Continuation for user continues
        (call/cc
          (lambda (cc)
            (set! continue-kw cc)))
        (let (; Initialize counter
              (cntr beg*)
              ; Choose increment function based on range type
              (incr (if (< beg* end*) + -)))
          ; Continuation for user breaks
          (call/cc 
            (lambda (break-kw)
              ; Start looping
              (let loop ()
                body ...
                (set! cntr (incr cntr 1))
                (unless (= cntr end*) (loop)))))))]))
 ; Example
 (define state 0)
 (For i from 1 to 10 kws: get-out start-over do
      (display i)
      (newline)
      (when (= i (if (= state 0) 5 7))
        (if (= state 0)
          (begin
            (set! state 1)
            (start-over))
          (get-out)))))
