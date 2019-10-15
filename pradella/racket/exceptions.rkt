;;; Exceptions by me ;D

;; The handlers stack
(define *handlers* #f)

;; Pop a handler
(define (pop-handler)
  (let ((res (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    res))

;; Push a handler
(define (push-handler handler)
  (set! *handlers* (cons handler *handlers*)))

;; Test area
(begin
  (let loop ()
    (push-handler (lambda (x) (displayln x)))
    ((pop-handler) 1)
    (loop)))

;; Le macro
(define-syntax try
  [(_ throw: throw-kw try-blk ... catch catch-blk ...)
   (syntax-rules (throw: catch)
     (begin
       (define (handle-throw thrown)
         ; Search for thrown element among catch pairs
         (cadr
           (memf (lambda (pair)
                   ; TODO : check if eqv? is the right choice here
                   (eqv? (car pair) thrown))
                 '(catch-blk ...)))))
     (call/cc
       (lambda (skip-rest-of-try)
         try-blk ...))))]
[])

;; This is the sacred graal...
(try catch-kw: catch
     (displayln "hello")
     (throw 1)
     (displayln "there!")
     catch
     [1 (displayln "who threw this 1?")]
     [2 (displayln "who threw this 2?")]
     [3 (displayln "who threw this 3?")])
