(defconstant fail nil
  "Pattern matching fails")

(defconstant no-bindings '((t . t))
  "A match, that happens to have no bindings")

(defun variable? (atm)
  (and (symbolp atm)
       (equal (elt (symbol-name atm) 0)
              #\?)))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-val (binding)
  (cdr binding))

(defun binding-var (binding)
  (car binding))

(defun lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun make-binding (var val)
  (cons var val))

(defun extend-bindings (var val bindings)
  (cons (make-binding var val)
        (if (equal bindings no-bindings)
            nil
          bindings)))

(defun all (lst)
  (notany #'null lst))

(defun any (lst)
  (notevery #'null lst))

(defmacro if-let (bindings then-form &optional (else-form nil))
  (let ((let-vars (cons 'list (mapcar #'car bindings))))
    `(let ,bindings
       (if (all ,let-vars)
           ,then-form
         ,else-form))))

(defun match-variable (var input bindings)
  (if-let ((binding (get-binding var bindings)))
    (if (equal input (binding-val bindings))
        bindings
      fail)
    (extend-bindings var input bindings)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  (cond ((equal bindings nil) fail)
        ((variable? pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input)) (pat-match (rest pattern)
                                                        (rest input)
                                                        (pat-match (first pattern)
                                                                   (first input)
                                                                   bindings)))
        (t fail)))

(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                   (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun extract-variables (pattern)
  (remove-if-not #'variable? (flatten pattern)))

(defun make-let-list (variables alist)
  (mapcar #'(lambda (var)
              `(,var (cdr (assoc ',var ,alist))))
          variables))

(defmacro let-match ((pattern input) &body body)
  (let ((alist (gensym "ALIST")))
    `(let ((,alist (pat-match ',pattern ,input)))
       (let ,(make-let-list (extract-variables pattern) alist)
         (if ,alist
             (values (progn ,@body) t)
           (values nil nil))))))

(defun clause-pattern (clause)
  (car clause))

(defun clause-body (clause)
  (cdr clause))

(defun expand-clauses (form clauses)
  (if (null clauses)
      nil
    (let ((first (car clauses))
          (rest (cdr clauses)))
      `(multiple-value-bind (ret-val match?) (let-match (,(clause-pattern first) ,form)
                                               ,@(clause-body first))
         (if match?
             ret-val
           ,(expand-clauses form rest))))))

(defmacro cond-match (form &body clauses)
  (expand-clauses form clauses))

(defmacro defun-match (name &rest clauses)
  `(defun ,name (&rest args)
     (cond-match args
       ,@clauses)))