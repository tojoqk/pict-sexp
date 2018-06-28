#lang racket/base
(require pict)
(require racket/gui/base)
(require racket/format)
(require racket/function)
(require racket/match)
(require racket/contract)
(require tojoqk/amb)

(define CELL-SIZE 30)
(define ARROW-LENGTH 40)
(define ARROW-SIZE 15)

(define (square n)
  (rectangle n n))

(define (need-arrow? x)
  (or (pair? x)
      (< 2 (string-length (~a x)))))

(define (pict-cell x)
  (cond
    [(need-arrow? x)
     (cc-superimpose (square CELL-SIZE)
                     (scale (filled-ellipse CELL-SIZE CELL-SIZE) 1/4))]
    [else
     (cc-superimpose (square CELL-SIZE)
                     (text (~a x)))]))

(define (pict-cons-cell pair)
  (hc-append -1
             (pict-cell (car pair))
             (pict-cell (cdr pair))))

(define (cdr-find _ p)
  (values (- (pict-width p) (/ CELL-SIZE 2.0))
          (/ CELL-SIZE 2.0)))

(define (car-find p _)
  (values (/ CELL-SIZE 2.0)
          (/ CELL-SIZE 2.0)))

(define (left-find p p1)
  (values (- (pict-width p) (pict-width p1))
          (/ CELL-SIZE 2.0)))

(define (top-find p p1)
  (values (/ CELL-SIZE 2.0)
          (- (pict-height p) (pict-height p1))))

(define (horizontal-combined p1 p2)
  (ht-append ARROW-LENGTH p1 p2))

(define (vertical-combined n p1 p2)
  (vl-append (- (* (+ CELL-SIZE ARROW-LENGTH) n)
                (pict-height p1))
             p1 p2))

(define (arrow-from-cdr p1 p2)
  (pin-arrow-line ARROW-SIZE
                  (horizontal-combined p1 p2)
                  p1 cdr-find
                  p2 left-find))

(define (arrow-from-car n p1 p2)
  (pin-arrow-line ARROW-SIZE
                  (vertical-combined n p1 p2)
                  p1 car-find
                  p2 top-find))

(define (pict-atom x)
  (cc-superimpose (cellophane (filled-rectangle CELL-SIZE CELL-SIZE) 0)
                  (text (~a x))))

(define (cdr* lst)
  (if (null? lst)
      '()
      (cdr lst)))

(define (%pict-sexp sexp height-list)
  (when (and (pair? height-list)
             (<= 0 (car height-list)))
    (amb))
  (cond
    [(pair? sexp)
     (let*-values ([(p) (pict-cons-cell sexp)]
                   [(p-1 height-list)
                    (cond
                      [(need-arrow? (cdr sexp))
                       (let-values ([(pict-cdr cdr-height-list)
                                     (%pict-sexp (cdr sexp) (cdr* height-list))])
                         (values (arrow-from-cdr p pict-cdr)
                                 (cons 0 cdr-height-list)))]
                      [else (values p (cons 0 (cdr* height-list)))])]
                   [(p-2 height-list)
                    (cond
                      [(need-arrow? (car sexp))
                       (match-let ([(cons len height-list)
                                    (let retry ([len 0]
                                                [height-list height-list])
                                      (amb (cons len height-list)
                                           (retry (add1 len) (map sub1 height-list))))])
                         (let-values ([(pict-car height-list)
                                       (%pict-sexp (car sexp) height-list)])
                           (values (arrow-from-car len p pict-car)
                                   (map (λ (x) (+ x len)) height-list))))]
                      [else (values p height-list)])])
       (values (lt-superimpose p-1 p-2) height-list))]
    [else
     (values (pict-atom sexp) (cons 0 (cdr* height-list)))]))

(define (pict-sexp sexp)
  (call-with-amb
   (thunk
    (define-values (result height-list)
      (%pict-sexp sexp '()))
    result)))
(provide/contract [pict-sexp (any/c . -> . pict?)])
