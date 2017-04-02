(in-package :cl-user)
(defpackage :fields(:use :cl)
  (:export
    #:everyf
    #:somef
    #:mapfield
    #:mapf
    #:nmapf
    #:propagatef
    #:setf-if-any
    #:setf-if-null
    #:finc
    #:dofields
    ))
(in-package :fields)

(defmacro everyf(pred &rest fields)
  (let((fn(gensym "FN")))
    `(LET((,fn ,pred))
       (AND ,@(loop :for field :in fields
		    :collect `(FUNCALL ,fn ,field))))))

(defmacro somef(pred &rest fields)
  (let((fn(gensym"FN")))
    `(LET((,fn ,pred))
       (OR ,@(loop :for field :in fields
		   :collect `(FUNCALL ,fn ,field))))))

(define-compiler-macro mapfield(function &rest fields)
  (let((fn(gensym"FN")))
    `(LET((,fn ,function))
       (LIST ,@(loop :for field :in fields :collect `(FUNCALL ,fn ,field))))))

(defun mapfield(function &rest fields)
  (mapcar function fields))

(define-compiler-macro mapf(function &rest fields)
  (let((fn(gensym"FN")))
    `(LET((,fn ,function))
       (TAGBODY ,@(loop :for field :in fields
			:collect `(FUNCALL ,fn ,field))))))

(defun mapf(function &rest fields)
  (map nil function fields))

(defmacro nmapf(function &rest fields)
  (let((fn(gensym"FN")))
    `(LET((,fn ,function))
       (TAGBODY
	 (SETF ,@(loop :for field :in fields
		       :append `(,field (FUNCALL ,fn ,field))))))))

(defmacro propagatef(item &rest places)
  (let((var(gensym"VAR")))
    `(LET((,var ,item))
       (SETF ,@(loop :for place :in places
		     :nconc `(,place ,var))))))

(defmacro setf-if-any(&rest args)
  (labels((REC(list &optional acc)
	    (if(endp list)
	      (nreverse acc)
	      (BODY (first list)(second list)(cddr list) acc)))
	  (BODY(place value rest acc)
	    (REC rest (cond
			((null value)acc)
			((constantp value)
			 (cons `(SETF ,place ,value)
			       acc))
			(T (cons (let((temp(gensym "TEMP")))
				   `(LET((,temp ,value))
				      (WHEN ,temp
					    (SETF ,place ,temp))))
				 acc)))))
	  )
  `(TAGBODY
     ,@(REC args))))

(defmacro setf-if-null(&rest args)
  `(TAGBODY
     ,@(loop :for (place value) :on args :by #'cddr
	     :when value
	     :collect
	     `(UNLESS ,place
		      (SETF ,place ,value)))))

(defmacro dofields ((var (&rest field*) &optional return)&body body)
  (flet((parse(body)
	  (loop :for form :in body
		:when (typep form '(CONS (EQL DECLARE)T))
		:collect form :into declares
		:else :collect form :into bodies
		:finally (return (nconc declares
					`((TAGBODY ,@bodies)))))))
    `(BLOCK()
       ,@(loop :for field :in field*
	       :collect `(SYMBOL-MACROLET((,var ,field))
			   ,@(parse body)))
       (LET(,var)
	 (DECLARE(IGNORABLE ,var))
	 ,return))))

(defmacro finc (place &optional num)
  (let((var(gensym)))
    `(LET((,var ,(or num 1)))
       (PROG1 ,place
	      (INCF ,place ,var)))))

