; vim: ft=lisp et
(defsystem :fields
  :depends-on ()
  :licence "Public Domain"
  :author "Shinichi Sato"
  :description "Utilities for imperative field oriented programming."
  :components((:file "fields")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "fields"))))
  (append (call-next-method) '((test-op "fields.test"))))
