;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

(in-package :paiprolog.auxfns)

;;;; Implementation-Specific Details

#+nil ; no longer need this
(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+(or Allegro EXCL)
  (dolist (pkg '(excl common-lisp common-lisp-user))
    (setf (excl:package-definition-lock (find-package pkg)) nil))

  ;; Don't warn if a function is defined in multiple files --
  ;; this happens often since we refine several programs.
  #+Lispworks
  (setq *PACKAGES-FOR-WARN-ON-REDEFINITION* nil)

  #+LCL 
   (compiler-options :warnings nil)
  )

;;;; REQUIRES

;;; The function REQUIRES is used in subsequent files to state dependencies
;;; between files.  The current definition just loads the required files,
;;; assumming they match the pathname specified in *PAIP-DIRECTORY*.
;;; You should change that to match where you have stored the files.
;;; A more sophisticated REQUIRES would only load it if it has not yet
;;; been loaded, and would search in different directories if needed.

#|
(defun requires (&rest files)
  "The arguments are files that are required to run an application."
  (declare (cl:ignore files))
  #+nil
  (mapc #'load-paip-file files))
|#
#|
(defvar *paip-files*
  `("auxfns" "tutor" "examples" 
    "intro" "simple" "overview" "gps1" "gps" "eliza1" "eliza" "patmatch" 
    "eliza-pm" "search" "gps-srch" "student" "macsyma" "macsymar" "unify" 
    "prolog1" "prolog" "prologc1" "prologc2" "prologc" "prologcp" 
    "clos" "krep1" "krep2" "krep" "cmacsyma" "mycin" "mycin-r" "waltz" 
    "othello" "othello2" "syntax1" "syntax2" "syntax3" "unifgram" 
    "grammar" "lexicon" "interp1" "interp2" "interp3" 
    "compile1" "compile2" "compile3" "compopt"))

(defparameter *paip-directory*
  (make-pathname :name nil :type nil
		 :defaults (or (and (boundp '*load-truename*) *load-truename*)
			       (truename ""))) ;;??? Maybe Change this
  "The location of the source files for this book.  If things don't work,
  change it to reflect the location of the files on your computer.")

(defparameter *paip-source* 
  (make-pathname :name nil :type "lisp" ;;???  Maybe Change this
		 :defaults *paip-directory*)) 

(defparameter *paip-binary*
  (make-pathname
   :name nil
   :type (first (list #+LCL (first *load-binary-pathname-types*)
		      #+Lispworks system::*binary-file-type*
		      #+MCL "fasl"
		      #+Allegro excl:*fasl-default-type*
		      #+(or AKCL KCL) "o"
		      #+CMU "sparcf"
		      #+CLISP "fas"
		      "bin"))  ;;???  Maybe Change this
   :directory (append (pathname-directory *paip-source*) '("bin"))
   :defaults *paip-directory*))

(defun paip-pathname (name &optional (type :lisp))
  (make-pathname :name name 
		 :defaults (ecase type
			     ((:lisp :source) *paip-source*)
			     ((:binary :bin) *paip-binary*))))

(defun compile-all-paip-files ()
  (mapc #'compile-paip-file *paip-files*))

(defun compile-paip-file (name)
  (let ((path (paip-pathname name :lisp)))
    (load path)
    (compile-file path :output-file (paip-pathname name :binary))))

(defun load-paip-file (file)
  "Load the binary file if it exists and is newer, else load the source."
  (let* ((src (paip-pathname file :lisp))
	 (src-date (file-write-date src))
	 (bin (paip-pathname file :binary))
	 (bin-date (file-write-date bin)))
    (load (if (and (probe-file bin) src-date bin-date (>= bin-date src-date))
	      bin
	    src))))
|#
;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list .,variables))
	(progn .,body)
	(list 'let
	 ,`(list ,@(mapcar #'(lambda (tmp var)
			       `(list ',tmp ,var))
			   temps variables))
	 (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
		       variables temps)
	   .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (cl:ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (find-anywhere item (first tree))
	    (find-anywhere item (rest tree)))))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )

;;;; Auxiliary Functions

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))

(defun maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array) 
            (max (fill-pointer array) new-length))))

;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~:@(~{~a~}~)" args) "PAIPROLOG"))

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (format nil "~{~a~}" args)))

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

(defun flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; ==============================

(defun member-equal (item list)
  (member item list :test #'equal))

;;; ==============================

(defun compose (&rest functions)
  #'(lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;;; ==============================

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;;;; PATTERN MATCHING FACILITY

(defconstant fail nil)
(defconstant no-bindings (if (boundp 'no-bindings)
			     (symbol-value 'no-bindings)
			     '((t . t))))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;; ==============================

;;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

;;;; Delayed computation:

(defstruct delay value (computed? nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value #'(lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))

;;;; Defresource:

(defmacro defresource (name &key constructor (initial-copies 0)
                       (size (max initial-copies 10)))
  (let ((resource (symbol '* (symbol name '-resource*)))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(progn
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc #',deallocate (loop repeat ,initial-copies 
                                       collect (,allocate))))
       ',name)))

(defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;;; Queues:

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))

(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))

;;;; Other:

(defun sort* (seq pred &key key) 
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;; ==============================

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

;;; ==============================

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

;;; ==============================

(defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(loop for element in elements
             for i from 0
             collect `(defconstant ,element ,i))))

;;; ==============================

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; ==============================

;;;; CLtL2 and ANSI CL Compatibility

(unless (fboundp 'defmethod)
(defmacro defmethod (name args &rest body)
  `(defun ',name ',args ,@body))
)

(unless (fboundp 'map-into)
(defun map-into (result-sequence function &rest sequences)
  "Destructively set elements of RESULT-SEQUENCE to the results
  of applying FUNCTION to respective elements of SEQUENCES."
  (let ((arglist (make-list (length sequences)))
        (n (if (listp result-sequence)
               most-positive-fixnum
               (array-dimension result-sequence 0))))
    ;; arglist is made into a list of args for each call
    ;; n is the length of the longest vector
    (when sequences
      (setf n (min n (loop for seq in sequences
                           minimize (length seq)))))
    ;; Define some shared functions:
    (flet
      ((do-one-call (i)
         (loop for seq on sequences
               for arg on arglist
               do (if (listp (first seq))
                      (setf (first arg)
                            (pop (first seq)))
                      (setf (first arg)
                            (aref (first seq) i))))
         (apply function arglist))
       (do-result (i)
         (if (and (vectorp result-sequence)
                  (array-has-fill-pointer-p result-sequence))
             (setf (fill-pointer result-sequence) 
                   (max i (fill-pointer result-sequence))))))
      (declare (inline do-one-call))
      ;; Decide if the result is a list or vector,
      ;; and loop through each element
      (if (listp result-sequence)
          (loop for i from 0 to (- n 1)
                for r on result-sequence
                do (setf (first r)
                         (do-one-call i))
                finally (do-result i))
          (loop for i from 0 to (- n 1)
                do (setf (aref result-sequence i)
                         (do-one-call i))
                finally (do-result i))))
      result-sequence))

)

(unless (fboundp 'complement)
(defun complement (fn)
  "If FN returns y, then (complement FN) returns (not y)."
  #'(lambda (&rest args) (not (apply fn args))))
)

(unless (fboundp 'with-compilation-unit)
(defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; That way, undefined function warnings that are really
  ;; just forward references will not be printed at all.
  ;; This is defined in Common Lisp the Language, 2nd ed.
  (declare (cl:ignore options))
  `(,(read-time-case
       #+Lispm 'compiler:compiler-warnings-context-bind
       #+Lucid 'with-deferred-warnings
               'progn)
    .,body))
)

;;;; Reduce

#|
(when nil ;; Change this to T if you need REDUCE with :key keyword.

(defun reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq) #'reduce-list #'reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

(defun reduce (function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

(defun reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
           (funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (funcall-if key (aref seq start))
                         (funcall-if key (aref seq (+ start 1)))))))
             (loop for i from (+ start (if init-p 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 2)))
                         (funcall-if key (aref seq (- end 1)))))))
             (loop for i from (- end (if init-p 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (funcall-if key (aref seq i))
                              result)))
             result)))))

(defun reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) t start end
                      key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (pop seq)))
                       (funcall
                         fn
                         (funcall-if key (pop seq))
                         (funcall-if key (pop seq))))))
             (if end
                 (loop repeat (- end (if init-p 1 2)) while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq)))))
                 (loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq))))))
             result))))
)
|#
