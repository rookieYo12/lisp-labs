(defun add_new_prev_specified (List)
  (format t "Input element: ")
  (_add_new_prev_specified List (read-char)))

(defun _add_new_prev_specified (List Elem)
  (cond
    ((null List) nil)
    ((eql Elem (car List)) (rplaca (rplacd List List) Elem))
    (t (_add_new_prev_specified (cdr List) Elem))))

(defun f (List)
  (cond
    ((null List) nil)
    (t (f (cddr (rplaca (rplacd List List) 'n))))))
