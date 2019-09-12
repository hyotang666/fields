; vim: ft=lisp et
(defsystem :fields
  :version "0.0.3"
  :depends-on ()
  :licence "Public Domain"
  :author "Shinichi Sato"
  :description "Utilities for imperative field oriented programming."
  :components((:file "fields")))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on ((o test-op) (c (eql (find-system "fields"))))
  (append (call-next-method) '((test-op "fields.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "fields")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
