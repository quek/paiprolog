(in-package "PAIPROLOG")

(defun retract-same-arity-clause (clause)
  "Retract a clause from the data base"
  (let* ((head (clause-head clause))
         (pred (predicate head))
         (arity (1-  (length head))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *uncompiled*)
    (setf (get pred 'clauses)
	  (delete-if (lambda (x)
                       (let* ((h (clause-head x))
                              (p (predicate h))
                              (a (1- (length h))))
                         (and (eq p pred)
                              (= a arity))))
                     (get-clauses pred)))
    pred))

(defmacro <-- (&rest clause)
  "Retract a same arity clause from the data base,
and add a clause to the data base."
  (let ((clause (replace-?-vars clause)))
    `(progn
       (retract-same-arity-clause ',clause)
       (add-clause ',clause))))

(defun insert-deref (exp)
  (if (atom exp)
      (if (variable-p exp)
          `(deref-exp ,exp)
          exp)
      (cons (insert-deref (car exp))
            (insert-deref (cdr exp)))))

(defun prolog-translate-goals (goals)
  (mapcar (lambda (goal)
            (if (starts-with goal 'lisp)
                (let ((vars (variables-in (last goal))))
                  ``(,@',(butlast goal)
                         (apply ,(lambda (,@vars)
                                   ,@(insert-deref (last goal)))
                                 ,',vars)))
                `',goal))
          goals))

(defmacro prolog (&rest goals)
  "Run Prolog in the surrounding Lisp environment
which is accessed from lisp functor.

(let ((x 100) y)
  (prolog (lisp ?a x)
          (= ?a ?b)
          (lisp (setf y (+ ?b ?b x 1))))
  y)
;;=> 301
"
  (let ((goals (replace-?-vars goals)))
    `(block prolog
       (clear-predicate 'top-level-query)
       (add-clause `((top-level-query)
                     ,,@(prolog-translate-goals goals)))
       (run-prolog 'top-level-query/0 #'ignore))))
