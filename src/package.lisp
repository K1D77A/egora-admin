;;;; package.lisp

(defpackage #:egora-admin
  (:use #:cl #:spinneret)
  (:export #:login
           #:*url*
           #:*api*
           #:logged-in-p
           #:M-FORBIDDEN
           #:api-error
           #:logout))
