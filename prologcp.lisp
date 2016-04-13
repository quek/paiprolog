;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.

(in-package "PAIPROLOG")


(defun fail/0 (cont)
  "7.8.1"
  (declare (cl:ignore cont))
  nil)

(defun true/0 (cont)
  "7.8.2"
  (funcall cont))

(defun call/1 (?g cont)
  "7.8.3: Try to prove goal by calling it."
  (deref ?g)
  (cond
    ((or (goal-and-p ?g) (goal-or-p ?g) (goal-if-p ?g))
     ;; FIXME: this use of a temporary predicate name is ugly,
     ;; non-threadsafe and basically evil bad and wrong.
     (funcall (compile-predicate 'call/1-tmp-fun 0 (list ?g)) cont))
    (t (apply (make-predicate (predicate ?g) (length (args ?g)))
              (append (args ?g) (list cont))))))

(defun !/0 (cont)
  "7.8.4"
  (funcall cont))

;;; these should never be called, as CALL/1 and our compiler both
;;; treat them specially.
#+(or) 
(progn
  (defun and/2 (first second cont)
    "7.8.5"
    (call/1 first (lambda () (call/1 second cont))))
  
  (defun or/2 (either or cont)
    "7.8.6 and 7.8.8"
    (if (goal-if-then-p either)
        (let ((old-trail (fill-pointer *trail*)))
          (call/1 (cadr either) (lambda ()
                                  (call/1 (caddr either) cont)
                                  (return-from or/2 nil)))
          (undo-bindings! old-trail)
          (call/1 or cont))
      (let ((old-trail (fill-pointer *trail*)))
	(call/1 either cont)
	(undo-bindings! old-trail)
	(call/1 or cont))))
  
  (defun if/2 (if then cont)
    "7.8.7"
    (let ((old-trail (fill-pointer *trail*)))
      (call/1 if (lambda ()
                   (call/1 then cont)
                   (undo-bindings! old-trail)
                   (return-from if/2 nil)))))

  ;; KLUDGE: this is a convenience function, really; in ISO Prolog,
  ;; this is expressed as (or (if if then) else), and there is special
  ;; handling of the OR/2 in that case.
  (defun if/3 (if then else cont)
    "7.8.8"
    (let ((old-trail (fill-pointer *trail*)))
      (call/1 if (lambda ()
                   (call/1 then cont)
                   (undo-bindings! old-trail)
                   (return-from if/3 nil)))
      (undo-bindings! old-trail)
      (call/1 else cont))))

(define-condition prolog-throw ()
  ((ball :initarg :ball :reader ball)))

(defun catch/3 (?goal ?catch ?recover cont)
  "7.8.9"
  (let ((trail (fill-pointer *trail*)))
    (handler-bind ((prolog-throw (lambda (c)
				   (let ((old-trail (fill-pointer *trail*)))
				     (when (unify! ?catch (ball c))
				       (undo-bindings! trail)
				       (return-from catch/3
					 (call/1 ?recover cont)))
				     (undo-bindings! old-trail)
				     (signal c)))))
      (call/1 ?goal cont))))

(defun throw/1 (?ball cont)
  "7.8.10"
  (signal (make-condition 'prolog-throw :ball (deref ?ball)))
  ;; FIXME: make this a throw eventually, and have a catch around
  ;; top-level-query.
  (error "system error"))

;;; 8.2 term unification

(defun =/2 (?arg1 ?arg2 cont)
  "8.2.1"
  (when (unify! ?arg1 ?arg2)
    (funcall cont)))

(defun unify-with-occurs-check/2 (?arg1 ?arg2 cont)
  "8.2.2"
  (let ((*occurs-check* t))
    (when (unify! ?arg1 ?arg2)
      (funcall cont))))

(defun \\=/2 (?arg1 ?arg2 cont)
  "8.2.3"
  (unless (unify! ?arg1 ?arg2)
    (funcall cont)))

;;; 8.3 type testing

(defun unbound-var-p (exp)
  "Is EXP an unbound var?"
  (and (var-p exp) (not (bound-p exp))))

(defun nonvarp (exp)
  (not (unbound-var-p exp)))

(defun atomicp (exp)
  ;; not ATOM, because we might be implementing unbound VARs as
  ;; something returning true to lisp's ATOM.
  (or (symbolp exp)
      (integerp exp)
      (floatp exp)))

(macrolet ((define-type-testing-predicate (name docstring fun)
               `(defun ,name (x cont)
                 ,docstring
                 (when (,fun (deref x))
                   (funcall cont)))))
  (define-type-testing-predicate var/1 "8.3.1" unbound-var-p)
  (define-type-testing-predicate atom/1 "8.3.2" symbolp)
  (define-type-testing-predicate integer/1 "8.3.3" integerp)
  (define-type-testing-predicate real/1 "8.3.4" floatp)
  (define-type-testing-predicate atomic/1 "8.3.5" atomicp)
  (assert (not (consp (?))))
  (define-type-testing-predicate compound/1 "8.3.6" consp)
  (define-type-testing-predicate nonvar/1 "8.3.7" nonvarp)
  ;; strictly, this should be (OR INTEGERP FLOATP).
  (define-type-testing-predicate number/1 "8.3.8" numberp))

;;; 8.4 term comparison

(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x) (consp y)
           (deref-equal (car x) (car y))
           (deref-equal (cdr x) (cdr y)))))

(defun ==/2 (?x ?y cont)
  "8.4.1: Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (when (deref-equal ?x ?y)
    (funcall cont)))

(defun \\==/2 (?x ?y cont)
  "8.4.2"
  (unless (deref-equal ?x ?y)
    (funcall cont)))

(defun term-precedes (x y)
  (and (not (eql (deref x) (deref y)))
       (typecase x
	 (var (or (not (var-p y))
		    ;; FIXME
		    (evenp (random 2))))
	 ((float) (if (var-p y)
		      nil
		      (or (not (floatp y))
			  (< x y))))
	 ((integer) (if (or (var-p y) (floatp y))
			nil
			(or (not (integerp y))
			    (< x y))))
	 (cl:symbol (if (or (var-p y) (floatp y) (integerp y))
			nil
			(or (not (symbolp y))
			    (string< (string x) (string y)))))
	 ;; "compound term": i.e. not lists, really
	 ((cons) (when (consp y)
		   (or (< (length x) (length y))
		       (and (= (length x) (length y))
			    (or (term-precedes (car x) (car y))
				(and (eql (car x) (car y))
				     (do* ((xis (cdr x) (cdr xis))
					   (xi (car xis) (car xis))
					   (yis (cdr y) (cdr yis))
					   (yi (car yis) (car yis)))
					  ((null xis) nil)
				       (when (term-precedes xi yi)
					 (return t))))))))))))

(defun @</2 (?x ?y cont)
  "8.4.3"
  (when (term-precedes ?x ?y)
    (funcall cont)))

(defun @=</2 (?x ?y cont)
  "8.4.4"
  (when (or (deref-equal ?x ?y)
	    (term-precedes ?x ?y))
    (funcall cont)))

(defun @>/2 (?x ?y cont)
  (when (term-precedes ?y ?x)
    (funcall cont)))

(defun @>=/2 (?x ?y cont)
  (when (or (deref-equal ?y ?x)
	    (term-precedes ?y ?x))
    (funcall cont)))

;;; 8.5 term creation and decomposition

(defun functor/3 (?term ?name ?arity cont)
  "8.5.1"
  (cond
    ((unbound-var-p ?term)
     (assert (not (unbound-var-p ?name)))
     (assert (not (unbound-var-p ?arity)))
     (when (unify! ?term (list* (deref ?name)
                                (loop repeat (deref ?arity) collect (?))))
       (funcall cont)))
    (t (if (atomicp (deref ?term))
           (when (and (unify! ?arity 0)
                      (unify! ?name ?term))
             (funcall cont))
           (when (and (unify! ?arity (length (cdr ?term)))
                      (unify! ?name (or (and(car ?term)))))
             (funcall cont))))))

(defun arg/3 (?n ?term ?arg cont)
  "8.5.2"
  (when (unify! (nth (deref ?n) (deref ?term)) ?arg)
    (funcall cont)))

(defun =.. (?term ?list cont)
  "8.5.3"
  ;; FIXME: have to decide how to represent Prolog lists
  )

(defun make-renamed-copy (term)
  (cond
    ((unbound-var-p term) (?))
    ((var-p term) term)
    ((atom term) term)
    (t (cons (make-renamed-copy (car term))
             (make-renamed-copy (cdr term))))))

(defun copy-term/2 (?term1 ?term2 cont)
  "8.5.4"
  (when (unify! (make-renamed-copy (deref ?term1)) (deref ?term2))
    (funcall cont)))

;;; 8.6 arithmetic evaluation

(defun is/2 (var exp cont)
  "8.6.1"
  ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (when ;; (and (not (find-if-anywhere #'unbound-var-p exp))
        ;;      (unify! var (eval (deref-exp exp))))
      (or (and (not (find-if-anywhere #'unbound-var-p exp))
               (unify! var (eval (deref-exp exp))))
          (let ((var exp)
                (exp var))
           (and (not (find-if-anywhere #'unbound-var-p exp))
                (unify! var (eval (deref-exp exp))))))
    (funcall cont)))

;;; 8.7 arithmetic comparison

(macrolet ((define-arithmetic-comparison-predicate (name op)
	     `(defun ,name (?e1 ?e2 cont)
		"8.7.3"
		(when (and (not (find-if-anywhere #'unbound-var-p ?e1))
			   (not (find-if-anywhere #'unbound-var-p ?e1)))
		  ;; FIXME: CL specifies comparison on (float,integer)
		  ;; by coercing the float to a rational, and
		  ;; comparing.  Prolog doesn't have rationals, and
		  ;; the coercion goes the other way.
		  (when (,op (eval (deref-exp ?e1)) (eval (deref-exp ?e2)))
		    (funcall cont))))))
  (define-arithmetic-comparison-predicate |=:=|/2 =)
  (define-arithmetic-comparison-predicate =\\=/2 /=)
  (define-arithmetic-comparison-predicate </2 <)
  (define-arithmetic-comparison-predicate =</2 <=)
  (define-arithmetic-comparison-predicate >/2 >)
  (define-arithmetic-comparison-predicate >=/2 >=))

;;; FIXME: 8.8 clause retrieval and information

(defun clause/2 (?head ?body cont)
  "8.8.1"
  (let ((clauses (get-clauses (predicate (deref ?head)))))
    (let ((old-trail (fill-pointer *trail*)))
      (dolist (clause clauses)
	(when (unify! `(,?head . ,?body) clause)
	  (funcall cont))
        (undo-bindings! old-trail)))))

(defun current-predicate/1 (?pi cont)
  "8.8.2"
  ;; FIXME: need to refactor *DB-PREDICATES* so that it contains arity
  ;; information
  )
    
;;; 8.9 clause creation and destruction

(defun asserta/1 (?clause cont)
  "8.9.1"
  (let* ((old-trail (fill-pointer *trail*))
	 (?head (?))
	 (?body (?)))
    (unless (unify! (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! old-trail)
      (unify! `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)) :asserta t)
    (funcall cont)))

(defun assertz/1 (?clause cont)
  "8.9.2"
  (let* ((old-trail (fill-pointer *trail*))
	 (?head (?))
	 (?body (?)))
    (unless (unify! (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! old-trail)
      (unify! `(,?head . ,?body) `(,?clause (true))))
    (add-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))

(defun retract/1 (?clause cont)
  "8.9.3"
  (let* ((old-trail (fill-pointer *trail*))
	 (?head (?))
	 (?body (?)))
    (unless (unify! (deref ?clause) `(<- ,?head . ,?body))
      (undo-bindings! old-trail)
      (unify! `(,?head . ,?body) `(,?clause (true))))
    (retract-clause (cons (deref ?head) (deref ?body)))
    (funcall cont)))

(defun abolish/1 (?pi cont)
  "8.9.4"
  ;; FIXME: implement this
  )

;;; 8.10 all solutions

;;; FIXME: I think this is right for FINDALL/3.  BAGOF/3 and SETOF/3
;;; have extra complicated stuff to do with witnesses.
(defun findall/3 (term goal bag cont)
  "8.10.1: Find all solutions to GOAL, and for each solution,
  collect the value of TERM into the list BAG."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-exp term) answers))) 
    (if (and (not (null answers))
             (unify! bag (nreverse answers)))
        (funcall cont))))

(defun bagof/3 (exp goal result cont)
  "8.10.2: Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (unify! result (nreverse answers)))
        (funcall cont))))

(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar #'(lambda (var) (cons (deref var) (?)))
                  (unique-find-anywhere-if #'var-p exp))
          exp))

(defun setof/3 (exp goal result cont)
  "8.10.3: Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal #'(lambda ()
                     (push (deref-exp exp) answers)))
    (if (and (not (null answers))
             (unify! result (delete-duplicates
                              answers
                              :test #'deref-equal)))
        (funcall cont))))

;;; FIXME: findall/3

;;; 8.11 stream selection and control

(defmacro with-stream ((s-var s) &body body)
  (let ((n (gensym)))
    `(let ((,n ,s))
       (let ((,s-var (etypecase ,n
		       ;; Lisp SYMBOL -> Prolog atom -> alias
		       (cl:symbol (get ,n 'stream-alias))
		       (stream ,n))))
	 ,@body))))

;;; FIXME: we probably want *prolog-standard-input* and
;;; *prolog-standard-output*, else things are likely to get confused.

(defun current-input/1 (?stream cont)
  "8.11.2"
  (when (unify! (deref ?stream) *standard-input*)
    (funcall cont)))

(defun current-output/1 (?stream cont)
  "8.11.3"
  (when (unify! (deref ?stream) *standard-output*)
    (funcall cont)))

(defun set-input/1 (?stream-or-alias cont)
  "8.11.4"
  (with-stream (s (deref ?stream-or-alias))
    (setf *standard-input* s)
    (funcall cont)))

(defun set-output/1 (?stream-or-alias cont)
  "8.11.5"
  (with-stream (s (deref ?stream-or-alias))
    (setf *standard-output* s)
    (funcall cont)))

(defun open/4 (?source/sink ?mode ?stream ?options cont)
  "8.11.6"
  (let ((element-type 'character)
	(name (deref ?source/sink))
	(args (case (deref ?mode)
		((read) '(:direction :input))
		((write) '(:direction :output))
		((append) '(:direction :output :if-exists :append))))
	(aliases nil))
    (dolist (option (deref ?options))
      (case (car option)
	;; FIXME: 7.10.2.11 specifies also reposition(Bool) and
	;; eof_action(Action).
	(type (setf element-type (ecase (cadr option)
				   (text 'character)
				   (binary '(unsigned-byte 8)))))
	(alias (push (cadr option) aliases))))
    (let ((stream (apply #'open name :element-type element-type args)))
      (when (unify! ?stream stream)
	(dolist (alias aliases)
	  (setf (get alias 'stream-alias) stream)
	  (push alias (get 'stream-aliases stream)))
	(funcall cont)))))

(defun close/1 (?stream-or-alias cont)
  "8.11.7"
  ;; FIXME: this will fail if we change the representation of the
  ;; empty list.
  (close/2 ?stream-or-alias nil cont))

(defun close/2 (?stream-or-alias ?options cont)
  "8.11.8"
  (with-stream (s (deref ?stream-or-alias))
    ;; FIXME: actually there's all the business about going back to
    ;; user_input, and also handling 7.10.2.12 force(Bool).
    (close s))
  (funcall cont))

(defun flush-output/0 (cont)
  "8.11.9"
  ;; FIXME: check to see if FORCE-OUTPUT is more appropriate
  (finish-output *standard-output*)
  (funcall cont))

(defun flush-output/1 (?stream-or-alias cont)
  "8.11.10"
  (with-stream (s (deref ?stream-or-alias))
    (finish-output s))
  (funcall cont))

;;; FIXME: 8.11.11 stream-property/2.  Need a registry of all open
;;; streams.

(defun at-end-of-stream/0 (cont)
  "8.11.12"
  ;; FIXME: we can fake this for character streams by doing peek-char
  ;; and handling the END-OF-FILE condition, but what about binary streams?
  )

(defun at-end-of-stream/1 (?stream-or-alias cont)
  "8.11.13"
  ;;; FIXME (see at-end-of-stream/0)
  )

(defun set-stream-position/2 (?stream-or-alias ?position cont)
  "8.11.14"
  (with-stream (s (deref ?stream-or-alias))
    (file-position s (deref ?position))
    (funcall cont)))

;;; FIXME: 8.12 character input/output

;;; FIXME: in general, these (8.12 and 8.13) things need to work both
;;; on characters (i.e. length-one symbols) and on character codes
;;; ("integers").  For now, character input/output works on characters
;;; and character-code input/output works on integers.

(defun get-char/1 (?char cont)
  "8.12.1"
  (when (unify! (deref ?char) (intern (string (read-char *standard-input*))))
    (funcall cont)))

(defun get-char/2 (?stream-or-alias ?char cont)
  "8.12.2"
  (with-stream (s (deref ?stream-or-alias))
    (when (unify! (deref ?char) (intern (string (read-char ?stream-or-alias))))
      (funcall cont))))

(defun put-char/1 (?char cont)
  "8.12.3"
  (write-char (character (deref ?char)) *standard-output*)
  (funcall cont))

(defun put-char/2 (?stream-or-alias ?char cont)
  "8.12.4"
  (with-stream (s (deref ?stream-or-alias))
    (write-char (character (deref ?char)) s)
    (funcall cont)))

(defun nl/0 (cont)
  "8.12.5"
  (terpri *standard-output*)
  (funcall cont))

(defun nl/1 (?stream-or-alias cont)
  "8.12.6"
  (with-stream (s (deref ?stream-or-alias))
    (terpri s)
    (funcall cont)))

;;; 8.13 character code input/output

;;; FIXME: see comment about element type for section 8.12

(defun get-code/1 (?code cont)
  (when (unify! (deref ?code) (read-byte *standard-output*))
    (funcall cont)))

(defun get-code/2 (?stream-or-alias ?code cont)
  (with-stream (s (deref ?stream-or-alias))
    (when (unify! (deref ?code) (read-byte s))
      (funcall cont))))

(defun put-code/1 (?code cont)
  (write-byte (deref ?code) *standard-output*)
  (funcall cont))

(defun put-code/2 (?stream-or-alias ?code cont)
  (with-stream (s (deref ?stream-or-alias))
    (write-byte (deref ?code) s)
    (funcall cont)))

;;; FIXME: 8.14 Term input/output

;;; these would probably be OK for lispy prolog; they're probably not
;;; for ISO prolog.
(defun read/1 (exp cont)
  (if (unify! exp (read))
      (funcall cont)))
(defun write/1 (exp cont)
  (write (deref-exp exp) :pretty t)
  (funcall cont))

;;; 8.15 logic and control

(defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
         ,(first body)
         ,@(loop for exp in (rest body)
                 collect '(undo-bindings! old-trail)
                 collect exp))))

(defun fail-if/1 (relation cont)
  "8.15.1: Negation by failure: If you can't prove G, then (not G) true."
  ;; Either way, undo the bindings.
  (with-undo-bindings
    (call/1 relation #'(lambda () (return-from fail-if/1 nil)))
    (funcall cont)))

(defun once/1 (thing cont)
  "8.15.2"
  (with-undo-bindings
    (call/1 thing cont)
    nil))

(defun repeat/0 (cont)
  "8.15.3"
  (loop (funcall cont)))

;;; 8.16 constant processing

(defun atom-length/2 (?atom ?length cont)
  "8.16.1"
  (when (unify! (length (string (deref ?atom))) ?length)
    (funcall cont)))

(defun atom-concat/3 (?atom1 ?atom2 ?atom12 cont)
  "8.16.2"
  (if (unbound-var-p (deref ?atom12))
      (when (unify! ?atom12 (intern (concatenate 'string
                                                 (string (deref ?atom1))
                                                 (string (deref ?atom2)))))
        (funcall cont))
      (let* ((string (string ?atom12))
             (length (length string)))
        (let ((old-trail (fill-pointer *trail*)))
          (deref ?atom1)
          (deref ?atom2)
          (dotimes (i length)
            (when (unify! `(,?atom1 . ,?atom2)
                          (cons (intern (subseq string 0 i))
                                (intern (subseq string i))))
              (funcall cont))
            (undo-bindings! old-trail))))))

(defun sub-atom/4 (?atom ?start ?length ?sub-atom cont)
  "8.16.3"
  (let* ((string (string (deref ?atom)))
         (length (length string)))
    (let ((old-trail (fill-pointer *trail*)))
      (dotimes (s (1+ length))
        (dotimes (l (1+ (- length s)))
          (when (and (unify! ?start (1+ s))
                     (unify! ?length l)
                     (unify! ?sub-atom (intern (subseq string s (+ s l)))))
            (funcall cont))
          (undo-bindings! old-trail))))))

(defun implode (list)
  (intern
   (with-output-to-string (s)
     (dolist (c list)
       (write-char (character c) s)))))

(defun explode (symbol)
  (loop for x across (string symbol) collect (intern (string x))))

(defun atom-chars/2 (?atom ?list cont)
  "8.16.4"
  (if (unbound-var-p (deref ?atom))
      (when (unify! ?atom (implode (deref-exp ?list)))
        (funcall cont))
      (when (unify! (explode ?atom) (deref ?list))
        (funcall cont))))

(defun atom-codes/2 (?atom ?codes cont)
  "8.16.5"
  (when (if (unbound-var-p (deref ?atom))
            (unify! ?atom
                    (intern (coerce (loop for i in (deref-exp ?codes)
                                          collect (code-char i))
                                    'string)))
            (unify! (loop for i across (string ?atom)
                          collect (char-code i))
                    (deref ?codes)))
    (funcall cont)))

(defun atom-characters/2 (?atom ?characters cont)
  "for Common Lisp"
  (when (if (unbound-var-p (deref ?atom))
            (unify! ?atom
                    (intern (coerce (deref-exp ?characters) 'string)))
            (unify! (loop for i across (string ?atom)
                          collect i)
                    (deref ?characters)))
    (funcall cont)))

(defun string-atom/2 (?string ?atom cont)
  "for Common Lisp"
  (when (if (unbound-var-p (deref ?string))
            (unify! ?string (string (deref ?atom)))
            (unify! (intern ?string) (deref ?atom)))
    (funcall cont)))

(defun string-list/2 (?string ?list cont)
  (when (if (unbound-var-p (deref ?string))
            (unify! ?string (coerce (deref-exp ?list) 'string))
            (unify! (coerce ?string 'list) (deref ?list)))
    (funcall cont)))

(defun char-code/2 (?char ?code cont)
  "8.16.6"
  (if (unbound-var-p (deref ?char))
      (when (unify! ?char (intern (string (code-char (deref ?code)))))
        (funcall cont))
      (when (unify! (char-code (character ?char)) (deref ?code))
        (funcall cont))))

(defun number-chars/2 (?number ?list cont)
  "8.16.7"
  (if (unbound-var-p (deref ?number))
      (when (unify! ?number
                    (read-from-string (map 'string 'character (deref-exp ?list))))
        (funcall cont))
      (when (unify! (explode (intern (princ-to-string ?number))) (deref ?list))
        (funcall cont))))

(defun number-codes/2 (?number ?list cont)
  "8.16.8"
  (if (unbound-var-p (deref ?number))
      (when (unify! ?number
                    (read-from-string (map 'string 'code-char (deref-exp ?list))))
        (funcall cont))
      (when (unify! (map 'list 'char-code (princ-to-string ?number))
                    (deref ?list))
        (funcall cont))))

;;; 8.17 implementation-defined hooks

;;(defun lisp/2 (?result exp cont)
;;  "Apply (first exp) to (rest exp), and return the result."
;;  (if (and (consp (deref exp))
;;           (unify! ?result (apply (first exp) (rest exp))))
;;      (funcall cont)))

(def-prolog-compiler-macro lisp (goal body cont bindings)
  "lisp/1 and lisp/2"
  (let ((args (args goal)))
    (case (length args)
      (1                                ; lisp/1
         (let* ((lisp-exp (first args))
                (lisp-args (variables-in lisp-exp)))
           `(progn
              (apply #'(lambda ,lisp-args ,(insert-deref lisp-exp))
                     ,(compile-arg lisp-args bindings))
              ,(compile-body body cont bindings))))
      (2                                ; lisp/2
         (let* ((var (first args))
                (lisp-exp (second args))
                (lisp-args (variables-in lisp-exp)))
           (compile-if
            `(unify! ,(compile-arg var bindings)
                     (apply #'(lambda ,lisp-args ,(insert-deref lisp-exp))
                            ,(compile-arg lisp-args bindings)))
            (compile-body body cont (bind-new-variables bindings goal)))))
      (t :pass))))

#+(or)
(prolog-compile 'foo '(((foo ?x ?y) (lisp ?x (concatenate 'string ?y)))))
;;-> 
;;   (DEFUN FOO/2 (#:?ARG1 #:?ARG2 CONT)
;;     (LET ((?Y (?)))
;;       (IF (UNIFY! #:?ARG1
;;                   (APPLY #'(LAMBDA (?Y) (CONCATENATE 'STRING (DEREF-EXP ?Y)))
;;                          (LIST #:?ARG2)))
;;           (FUNCALL CONT)))) 
;;=> NIL
#+(or)
(progn
  (<-- (foo ?x ?y) (lisp ?x (concatenate 'string ?y ?y)))
  (?- (foo ?x (#\a))))
;;-> 
;;   (DEFUN TOP-LEVEL-QUERY/0 (CONT)
;;     (LET ((?X (?)))
;;       (FOO/2 ?X '(#\a)
;;              #'(LAMBDA () (SHOW-PROLOG-VARS/2 '("?X") (LIST ?X) CONT))))) 
;;   (DEFUN FOO/2 (#:?ARG1 #:?ARG2 CONT)
;;     (LET ((?Y (?)))
;;       (IF (UNIFY! #:?ARG1
;;                   (APPLY
;;                    #'(LAMBDA (?Y)
;;                        (CONCATENATE 'STRING (DEREF-EXP ?Y) (DEREF-EXP ?Y)))
;;                    (LIST #:?ARG2)))
;;           (FUNCALL CONT)))) 
;;   ?X = aa
;;   No.
;;=> 

#+(or)
(progn
  (<- (bar ?x) (lisp (print ?x)))
  (?- (bar bar)))
;;-> 
;;   (DEFUN TOP-LEVEL-QUERY/0 (CONT)
;;     (BAR/1 'BAR #'(LAMBDA () (SHOW-PROLOG-VARS/2 'NIL 'NIL CONT)))) 
;;   (DEFUN BAR/1 (#:?ARG1 CONT)
;;     (LET ((?X (?)))
;;       (PROGN
;;        (APPLY #'(LAMBDA (?X) (PRINT (DEREF-EXP ?X))) (LIST #:?ARG1))
;;        (FUNCALL CONT)))) 
;;   BAR 
;;   Yes
;;   No.
;;=> 


#|
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (length () 0))
(<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))
|#

