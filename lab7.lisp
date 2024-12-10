(defun main ()
  (format t "Result is ~a~%" (func (read-int-var "a") (read-int-var "n") 0.0)))

(defun func (a n res)
  (if (>= n 1)
      (func a (- n 1) (+ res (/ (* n a) (+ n a))))
      res))

(defun read-int-var (name)                     
  (format t "Input ~a (integer): " name)  
  (handler-case
      (parse-integer (read-line))
    (error (e)
      (format t "Error! Try again.~%")
      (read-int-var name))))
