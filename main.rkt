#lang racket/base
(require racket/gui/base)
(require racket/format)
(require racket/contract)
(require pict)

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

(define (pict-sexp/helper sexp height-list fail)
  (when (and (pair? height-list)
             (<= 0 (car height-list)))
    (fail))
  (cond
    [(pair? sexp)
     (let*-values ([(cell) (pict-cons-cell sexp)]
                   [(cell-1 height-list)
                    (cond
                      [(need-arrow? (cdr sexp))
                       (let-values ([(pict-cdr cdr-height-list)
                                     (pict-sexp/helper (cdr sexp)
                                                       (cdr* height-list)
                                                       fail)])
                         (values (arrow-from-cdr cell pict-cdr)
                                 (cons 0 cdr-height-list)))]
                      [else (values cell (cons 0 (cdr* height-list)))])]
                   [(cell-2 height-list)
                    (cond
                      [(need-arrow? (car sexp))
                       (let retry ([len 1] [height-list (map sub1 height-list)])
                         (let/cc escape
                           (let/cc fail
                             (let-values ([(pict-car height-list)
                                           (pict-sexp/helper (car sexp)
                                                             height-list
                                                             fail)])
                               (escape (arrow-from-car len cell pict-car)
                                       (map (λ (x) (+ x len)) height-list))))
                           (retry (add1 len) (map sub1 height-list))))]
                      [else (values cell height-list)])])
       (values (lt-superimpose cell-1 cell-2) height-list))]
    [else
     (values (pict-atom sexp) (cons 0 (cdr* height-list)))]))

(define (pict-sexp sexp)
  (define-values (result height-list)
    (pict-sexp/helper sexp '() (λ () (error "pict-sexp: implementation error"))))
  result)
(provide/contract [pict-sexp (any/c . -> . pict?)])
