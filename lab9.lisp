(defun main (List)
  (max_nesting List 0 0))

(defun max_nesting (List Counter Maximum)
  (cond
    ((null (car List)) (max_num Counter Maximum))
    ((atom (car List)) (max_nesting (cdr List) Counter Maximum))
    (t (max_nesting (cdr List) Counter (max_nesting (car List) (+ Counter 1) Maximum)))))

(defun max_num (Num1 Num2)
  (cond
    ((> Num1 Num2) Num1)
    (t Num2)))
