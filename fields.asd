; vim: ft=lisp et
(defsystem :fields
  :depends-on ()
  :components((:file "fields")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "fields"))))
  (append (call-next-method) '((test-op "fields.test"))))
