; vim: ft=lisp et
(in-package :asdf)
(defsystem :fields.test
  :depends-on
  (:jingoh "fields")
  :components
  ((:file "fields"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine)))