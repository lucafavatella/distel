;; -*- comment-column: 32 -*-

(require 'cl)

(put 'mcase 'lisp-indent-function 1)
(put 'pmatch 'lisp-indent-function 2)

(defmacro mcase (object &rest clauses)
  "Pattern-matching case expression.
The syntax is like the normal `case':

  (mcase EXPR
    (PATTERN . BODY)
    ...)

The body of the first matching pattern is executed, with pattern
variables bound to their matching values. If no patterns match, an
error is signaled.

See `pmatch' for a description of pattern syntax."
  `(mcase* ,object ',clauses))

(defmacro pmatch (pattern object &rest body)
  "Match PATTERN with OBJECT, and execute BODY with all bindings.
The pattern syntax is:

Trivial: t, nil, [], 1, ...
  Always `equal' to the matching value.
Pattern variable: Var
  Variable that the pattern should bind. If the same variable
  appears several times in a pattern, then all of its bindings must
  match.
  NOTE: Pattern variable names are converted to lowercase and bound to
  lisp variables before calling BODY.
Constant: 'foo, (quote foo)
  Quoted constant, matched with `equal'.
Bound variable: ,var
  Pre-bound Lisp variable, matched by value.
Wild card: _ (underscore)
  Matches anything, with no binding.
Sequence: (pat1 ...), [pat1 ...]
  Matches the \"shape\" of the pattern, as well as each individual
  subpattern."
  `(mcase ,object
     (,pattern ,@body)
     (_        (signal 'erl-exit-signal
		       (list (tuple 'badmatch ',pattern ,object))))))

(defun mcase* (object clauses)
  "Clauses = ((TEST . ACTIONS) ...)"
  (if (null clauses)
      (signal 'erl-exit-signal '(case-clause))
    (let* ((clause  (car clauses))
	   (pattern (car clause))
	   (action  (cdr clause))
	   (result  (patmatch pattern object)))
      (if (eq result 'fail)
	  (mcase* object (cdr clauses))
	(eval `(let ,(alist-to-list result)
		 ,@action))))))

(defun patmatch (pattern object &optional bindings)
  "Match OBJECT with PATTERN, and return an alist of bindings."
  (if (eq bindings 'fail)
      'fail
    (cond ((pmatch-wildcard-p pattern)
	   bindings)
	  ((pmatch-constant-p pattern) ; '(x)
	   (pmatch-constant pattern object bindings))
	  ((pmatch-bound-var-p pattern)	; ,foo
	   (pmatch-match-var pattern object bindings))
	  ((pmatch-unbound-var-p pattern) ; Foo
	   (pmatch-bind-var pattern object bindings))
	  ((pmatch-trivial-p pattern) ; nil, t, any-symbol
	   (if (equal pattern object) bindings 'fail))
	  ((sequencep pattern)
	   (if (eq (type-of pattern) (type-of object))
	       (patmatch (subseq pattern 1) (subseq object 1)
		       (patmatch (elt pattern 0) (elt object 0) bindings))
	     'fail))
	  (t
	   'fail))))

(defun pmatch-wildcard-p (pat)
  (eq pat '_))

(defun pmatch-trivial-p (pat)
  "Test for patterns which can always be matched literally with `equal'."
  (and (atom pat)
       (or (not (vectorp pat))
	   (equal pat []))
       (not (pmatch-unbound-var-p pat))))

(defun pmatch-constant-p (pat)
  "Test for (quoted) constant patterns.
Example: (QUOTE QUOTE)"
  (and (consp pat)
       (= (length pat) 2)
       (eq (car pat) 'quote)))

(defun pmatch-constant-value (pat)
  "The value of a constant pattern.
(QUOTE X) => X"
  (cadr pat))

(defun pmatch-constant (pat object bindings)
  "Match OBJECT with the constant pattern PAT."
  (if (equal (pmatch-constant-value pat) object)
      bindings
    'fail))

(defun pmatch-unbound-var-p (obj)
  "Unbound variable symbols start with an uppercase letter."
  (when (symbolp obj)
    (let ((char1 (elt (symbol-name obj) 0)))
      (eq char1 (upcase char1)))))

(defun pmatch-unbound-var-symbol (sym)
  (intern (downcase (symbol-name sym))))

(defun pmatch-bind-var (pat object bindings)
  "Add a binding of pattern variable VAR to OBJECT in BINDINGS."
  (let* ((var (pmatch-unbound-var-symbol pat))
	 (binding (assoc var bindings)))
    (cond ((null binding)
	   (acons var object bindings))
	  ((equal (cdr binding) object)
	   bindings)
	  (t
	   'fail))))

(defun pmatch-match-var (var object bindings)
  "Match the value of the Lisp variable VAR with OBJECT."
  (if (equal (symbol-value (pmatch-bound-var-name pattern)) object)
      bindings
    'fail))

(defun pmatch-bound-var-p (obj)
  (and (symbolp obj)
       (eq (elt (symbol-name obj) 0) ?,)))

(defun pmatch-bound-var-name (sym)
  (intern (substring (symbol-name sym) 1)))

(defun alist-to-list (alist)
  "Convert an alist into a normal list, e.g. ((A . B)) => ((A B))"
  (mapcar (lambda (cell)
	    (list (car cell) (cdr cell)))
	  alist))

(defun pmatch-alist-keysort (alist)
  (sort alist (lambda (a b)
		(string< (symbol-name (car a))
			 (symbol-name (car b))))))

;;; Test suite

(defun pmatch-expect (pattern object expected)
  "Assert that matching PATTERN with OBJECT yields EXPECTED.
EXPECTED is either 'fail or a list of bindings (in any order)."
  (let ((actual (patmatch pattern object)))
    (if (or (and (eq actual 'fail)
		 (eq actual expected))
	    (and (listp expected)
		 (listp actual)
		 (equal (pmatch-alist-keysort actual)
			(pmatch-alist-keysort expected))))
	t
      (error "Patmatch: %S %S => %S, expected %S"
	     pattern object actual expected))))

(defun pmatch-test ()
  "Test the pattern matcher."
  (interactive)
  (pmatch-expect t t ())
  (pmatch-expect '(t nil 1) '(t nil 1) ())
  (let ((foo 'foo))
    (pmatch-expect '(FOO ,foo 'foo [FOO]) '(foo foo foo [foo])
		   '((foo . foo))))
  (pmatch-expect 1 2 'fail)
  (pmatch-expect '(x x) '(1 2) 'fail)
  (pmatch-expect '_ '(1 2) 'nil)
  (assert (equal 'yes
		 (mcase '(call 42 lists length ((1 2 3)))
		   (t 'no)
		   (1 'no)
		   ((call Ref 'lists 'length (_))
		    'yes)
		   (_ 'no))))
  t)

;; Automatically run test
(pmatch-test)

(provide 'patmatch)

