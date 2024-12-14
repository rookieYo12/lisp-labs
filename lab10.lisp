(defun f (List Elem)
  (cond
    ((null List) nil)
    ((eql Elem (car List)) (f (cddr (rplaca (rplacd List (cons (car List) (cdr List))) 'n)) Elem))
    (t (f (cdr List) Elem))))
