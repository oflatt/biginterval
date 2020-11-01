#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         racket/runtime-path)

(require math/private/bigfloat/mpfr)

(provide iv)

(define-runtime-path libmpfi-so "/usr/local/lib/libmpfi.so")

(define mpfi-lib (ffi-lib libmpfi-so))

(define-syntax get-mpfi-fun
  (syntax-rules ()
    [(_ name type)
     (get-ffi-obj name mpfi-lib type)]))

(define-cstruct _mpfi ([left _mpfr] [right _mpfr]))

(define mpfr-table (make-weak-hasheq))

(define (new-mpfi left right)
  (define mpfr1 (if (bigfloat? left) left (bf left)))
  (define mpfr2 (if (bigfloat? right) right (bf right)))
  (define res (make-mpfi mpfr1 mpfr2))
  (hash-set! mpfr-table res (list mpfr1 mpfr2))
  res)

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

(define-syntax-rule (provide-1ary-fun name c-name)
  (begin
    (define cfun
      (get-mpfi-fun c-name (_fun _mpfi-pointer _mpfi-pointer -> _int)))
    (define (name x1)
      (define y (new-mpfi 0 0))
      (cfun y x1)
      y)
    (provide name)))


(define-syntax-rule (provide-2ary-funs [name c-name] ...)
  (begin (provide-2ary-fun name c-name) ...))
(define-syntax-rule (provide-1ary-funs [name c-name] ...)
  (begin (provide-1ary-fun name c-name) ...))

(provide-2ary-funs
  [ivadd 'mpfi_add]
  [ivsub 'mpfi_sub]
  [ivmul 'mpfi_mul]
  [ivdiv 'mpfi_div]
  [ivatan2  'mpfi_atan2]
  [ivhypot  'mpfi_hypot])

(provide-1ary-funs
  [ivneg 'mpfi_neg]
  [ivacos 'mpfi_acos]
  [ivsin 'mpfi_sin]
  [ivsinh 'mpfi_sinh]
  [ivasin 'mpfi_asin]
  [ivasinh 'mpfi_asinh]
  [ivtan    'mpfi_tan]
  [ivatan   'mpfi_atan]
  [ivtanh    'mpfi_tanh]
  [ivatanh   'mpfi_atanh]
  [ivcbrt    'mpfi_cbrt]
  [ivcos     'mpfi_cos]
  [ivcosh    'mpfi_cosh]
  [ivacosh    'mpfi_acosh]
  [ivexp     'mpfi_exp]
  [ivexp2    'mpfi_exp2]
  [ivexpm1    'mpfi_expm1]
  [ivfabs     'mpfi_abs]
  [ivlog       'mpfi_log]
  [ivlog10     'mpfi_log10]
  [ivlog1p   'mpfi_log1p]
  [ivlog2    'mpfi_log2]
  [ivsqrt     'mpfi_sqrt])


(define-syntax-rule (provide-mpfr-mpfi-fun name c-name)
  (begin
    (define cfun
      (get-mpfi-fun c-name (_fun _mpfr-pointer _mpfi-pointer -> _int)))
    (define (name x1)
      (define y (bf 0))
      (cfun y x1)
      y)
    (provide name)))

(provide-mpfr-mpfi-fun ivleft 'mpfi_get_left)
(provide-mpfr-mpfi-fun ivright 'mpfi_get_right)
