(defpackage :fields.spec
  (:use :cl :jingoh :fields))
(in-package :fields.spec)
(setup :fields)

(requirements-about EVERYF)

;;;; Description:
; tests every fields are satisfies pred.
#?(let(a b c d e)
    (everyf #'null a b c d e))
=> T

#+syntax
(EVERYF pred &rest field*) ; => result

;;;; Arguments and Values:

; pred := form which generates function, otherwise error. evaluated.
#?(let(a b c)
    (everyf null a b c))
:signals unbound-variable
,:ignore-signals warning

; field := generarized variable. evaluated.
#?(let((a '(1 nil 3))
       b)
    (everyf #'null (second a)b))
=> T

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; evaluate left to right.
; when one generarized variable does not satisfies function,
; short cut is occur.
#?(let((a 1)b c)
    (everyf #'null a (print b) c))
=> NIL

;;;; Exceptional-Situations:

(requirements-about SOMEF)

;;;; Description:
; tests one of field satisfies pred.
#?(let(a b c (d 0))
    (somef #'integerp a b c d))
=> T

#+syntax
(SOMEF pred &rest field*) ; => result

;;;; Arguments and Values:

; pred := form which generates function, otherwise error. evaluated.
#?(let(a b c)
    (somef integerp a b c))
:signals unbound-variable
,:ignore-signals warning

; field := generarized variable, evaluated.
#?(let(a b c)
    (somef #'integerp (princ a)(princ b)(princ c)))
:outputs "NILNILNIL"

; result := boolean

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; When one generarized variable satisfies pred,
; shortcut is occur.
#?(let(a b c)
    (somef #'null a (princ b)(princ c)))
=> T

;;;; Exceptional-Situations:

(requirements-about MAPFIELD)

;;;; Description:
; apply function each fields, return list of results.
; short hand for CL:MAPCAR, and efficient when compiled.
#?(let((a 1)(b 2)(c 3))
    (mapfield #'1+ a b c))
=> (2 3 4)
,:test equal

#+syntax
(MAPFIELD function &rest fields) ; => result

;;;; Arguments and Values:

; function := function, otherwise error.
#?(let(a b c)
    (mapfield "print" a b c))
:signals error
,:ignore-signals warning

; field := T
#?(mapfield #'princ 0 :a "hoge")
:outputs "0Ahoge"

; result := list which contains result of function.
#?(mapfield #'princ 0 :a "hoge")
=> (0 :A "hoge")
,:test equal
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAPF)

;;;; Description:
; apply function each fields.
; shorthand for `(CL:MAP NIL ...)`, and efficient when compiled.
#?(mapf #'princ 0 1 2 3)
:outputs "0123"

#+syntax
(MAPF function &rest field*) ; => result

;;;; Arguments and Values:

; function := function, otherwise error.
#?(mapf "PRINC" 0 1 2 3)
:signals error
,:ignore-signals warning

; field := T

; result := NIL
#?(mapf #'princ 0 1 2 3)
=> NIL
,:stream NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NMAPF)

;;;; Description:
; destructively modifies each field by result of function.
#?(let((a 0)(b 1)(c 2))
    (nmapf #'1+ a b c)
    (values a b c))
:values (1 2 3)

#+syntax
(NMAPF function &rest field*) ; => result

;;;; Arguments and Values:

; function := form which generates function, otherwise error. evaluated.
#?(nmapf princ :a :b :c)
:signals error
,:ignore-signals warning

; field := generarized variable, otherwise error. evaluated.
#?(nmapf #'princ :a)
:signals error
,:ignore-signals warning

; result := nil
#?(let(a b c)
    (nmapf #'princ-to-string a b c))
=> NIL


;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; MAPF is function, but NMAPF is macro.
#?(macro-function 'nmapf) :be-the function

;;;; Exceptional-Situations:

(requirements-about PROPAGATEF)

;;;; Description:
; propagate item to all places.
#?(let(a b c)
    (propagatef 1 a b c)
    (values a b c))
:values (1 1 1)

#+syntax
(PROPAGATEF item &rest places) ; => result

;;;; Arguments and Values:

; item := form which generates value. evaluated.

; places := generarized variable which will be destructively modified. not evaluated.
#?(let(a b c)
    (propagatef 1 a b (princ c)))
:signals error
,:lazy T
,:ignore-signals warning

; result := item
#?(let(a b c)
    #+ccl(declare(ignorable a b c))
    (propagatef 1 a b c))
=> 1

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SETF-IF-ANY)

;;;; Description:
; When value is not NIL, place is setfed.
#?(let((a 1)(b 1)(c 1))
    (setf-if-any a 0 b nil c :hoge)
    (values a b c))
:values (0 1 :HOGE)

#+syntax
(SETF-IF-ANY &rest args) ; => result

;;;; Arguments and Values:

; args := [place value]*
; place := generarized variable
; value := form which generates value

; result := nil
#?(let((a 1)(b 1)(c 1))
    (declare(ignore b))
    #+ccl(declare(ignorable a c))
    (setf-if-any a 0 b nil c 3))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SETF-IF-NULL)

;;;; Description:
; when place is null, value is setfed.
#?(let(a b (c 1))
    (setf-if-null a 0 b 1 c 2)
    (values a b c))
:values (0 1 1)

#+syntax
(SETF-IF-NULL &rest args) ; => result

;;;; Arguments and Values:

; args := [place value]*
; place := generarized variable
; value := form which generates value.

; result := NIL
#?(let(a b c)
    (setf-if-null a 1 b 2 c 3))
=> NIL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FINC)

;;;; Description:
; `CL:INCF` is `++i` in C. `FINC` is `i++` in C.
#?(let((a 0))
    (values a
	    (incf a)
	    a
	    (finc a)
	    a))
:values (0 1 1 1 2)

#+syntax
(FINC place &optional num) ; => result

;;;; Arguments and Values:

; place := generarized variable.

; num := number which increased.
#?(let((a 0))
    (values a (finc a 5) a))
:values (0 0 5)

; result := pre value of place.

;;;; Affected By:
; none

;;;; Side-Effects:
; destructively modify place.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DOFIELDS)

;;;; Description:
; iterate body under context which has local bidings.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c))
      (princ x)))
:outputs "012"

#+syntax
(DOFIELDS (var (field*) &optional return) &body body) ; => result

;;;; Arguments and Values:

; var := symbol, otherwise error
#?(let(a b c)
    (dofields("VAR"(a b c))
      "VAR"))
:signals error
,:ignore-signals warning

; field := T, evaluated.
#?(dofields(x (0 1 2))
    (princ x))
:outputs "012"

; return := form which generates return value.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c)(values a b c))
      (incf x)))
:values (1 2 3)

; body := implicit progn

; result := return value of return form. the default is nil.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c))
      (princ x)))
=> NIL
,:stream nil

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; can use `CL:RETURN` in body.
; in such case, short cut is occur.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b (princ c))(princ :skipped))
      (when(oddp x)
	(return x))))
=> 1

; can use `CL:GO` in body wich tag.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c))
      (when(oddp x)
	(go :end))
      (princ x)
      :end))
:outputs "02"

; can use declare in top of body.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c))
      (declare(type integer x))
      (princ x)))
:outputs "012"

; can see VAR from RETURN, but it bounds by NIL.
#?(let((a 0)(b 1)(c 2))
    (dofields(x (a b c)(princ-to-string x))
      (incf x)))
=> "NIL"
,:test equal

;;;; Exceptional-Situations:

