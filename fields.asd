; vim: ft=lisp et
(defsystem :fields
  :depends-on ()
  :components((:file "fields")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "fields"))))
  (test-system :fields.test))