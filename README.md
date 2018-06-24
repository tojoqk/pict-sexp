# pict-sexp
Draw sexp.

![fib.png](https://raw.githubusercontent.com/wiki/tojoqk/pict-sexp/images/fib.png)

# INSALL
```
raco pkg install https://github.com/tojoqk/pict-sexp.git
```

# USAGE
Save as "pict-fib.png".
```
#lang racket
(require pict)
(require pict-sexp)

(define pict-fib
  (pict-sexp
   '(define (fib n)
      (cond
        [(= n 0) 0]
        [(= n 1) 1]
        [else
         (+ (fib (- n 1))
            (fib (- n 2)))]))))

(send (pict->bitmap pict-fib) save-file "pict-fib.png" 'png)
```
