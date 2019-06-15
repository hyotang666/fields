; vim: ft=lisp et
(defsystem :fields
  :version "0.0.1"
  :depends-on ()
  :licence "Public Domain"
  :author "Shinichi Sato"
  :description "Utilities for imperative field oriented programming."
  :components((:file "fields")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "fields"))))
  (append (call-next-method) '((test-op "fields.test"))))
(defmethod operate :around(o (c (eql (find-system "fields")))
                             &key ((:compile-print *compile-print*))
                             ((:compile-verbose *compile-verbose*))
                             &allow-other-keys)
  (call-next-method))
