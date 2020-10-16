#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         racket/runtime-path)

(require math/private/bigfloat/mpfr)

(define-runtime-path libmpfi-so "/usr/local/lib/libmpfi.so")

(define mpfi-lib (ffi-lib libmpfi-so))

(define-syntax get-mpfi-fun
  (syntax-rules ()
    [(_ name type)
     (get-ffi-obj name mpfi-lib type)]))

(define-cstruct _mpfi ([left _mpfr] [right _mpfr]))

(define mpfr-table (make-weak-hasheq))

(define (new-mpfi left right)
  (define mpfr1 (bf left))
  (define mpfr2 (bf right))
  (hash-set! mpfr-table mpfr1 mpfr1)
  (hash-set! mpfr-table mpfr2 mpfr2)
  (make-mpfi mpfr1 mpfr2))

(define-syntax iv
  (syntax-rules ()
    [(_ left right) (new-mpfi left right)]
    [(_ single) (new-mpfi single single)]))


(define-syntax-rule (provide-2ary-fun name c-name)
  (begin
    (define cfun
      (get-mpfi-fun c-name (_fun _mpfi-pointer _mpfi-pointer _mpfi-pointer -> _int)))
    (define (name x1 x2)
      (define y (new-mpfi 0 0))
      (cfun y x1 x2)
      y)
    (provide name)))

(define-syntax-rule (provide-2ary-funs [name c-name] ...)
  (begin (provide-2ary-fun name c-name) ...))

(provide-2ary-funs
  [ivadd 'mpfi_add]
  [ivsub 'mpfi_sub]
  [ivmul 'mpfi_mul]
  [ivdiv 'mpfi_div])

(define-syntax-rule (provide-mpfr-mpfi-fun name c-name)
  (begin
    (define cfun
      (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfi-pointer -> _int)))
    (define (name x1)
      (define y (bf 0))
      (cfun y x1)
      y)
    (provide name)))

(provide-mpfr-mpfi-fun ivleft 'mpfi_get_left)
(provide-mpfr-mpfi-fun ivright 'mpfi_get_right)

(ivleft (ivadd (iv 1 2) (iv 3 4)))
(ivright (ivadd (iv 1 2) (iv 3 4)))