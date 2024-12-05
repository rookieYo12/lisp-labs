(defun main (List)
  (left_middle_right List (min_num_index List) (max_num_index List)))

(defun max_num_index (List)
  (_max_num_index List 0 0))

(defun _max_num_index (List Index Counter)
  (cond
    ((null List) -1) ; If list is empty
    ((null (cdr List)) Index) ; Base condition: if tail is null, return head
    ; If head > next number skip number
    ((> (car List) (cadr List)) (_max_num_index (cons (car List) (cddr List)) Index (+ Counter 1)))
    (t (_max_num_index (cdr List) (+ Counter 1) (+ Counter 1))))) ; Else skip head

(defun min_num_index (List)
  (_min_num_index List 0 0))

(defun _min_num_index (List Index Counter)                                                         
  (cond                                                                                            
    ((null List) -1) ; If list is empty                                                      
    ((null (cdr List)) Index) ; If tail is null return index, list checked                       
    ; If head > next number skip number
    ((< (car List) (cadr List)) (_min_num_index (cons (car List) (cddr List)) Index (+ Counter 1)))
    (t (_min_num_index (cdr List) (+ Counter 1) (+ Counter 1))))) ; Else skip head                 

(defun left_middle_right (List Index1 Index2)
  (cond
    ((> Index2 Index1)
     (plus_middle_right (get_part List (+ Index1 1)) (- Index2 (+ Index1 1))))
    (t (rreverse (plus_middle_right (get_part List (+ Index2 1)) (- Index1 (+ Index2 1)))))))

(defun rreverse (List)
  (cond
    ((null List) nil)
    (t (append (rreverse (cdr List)) (cons (car List) nil)))))

; Returns list: first element is rest, second is result
(defun get_part (List Index)
  (_get_part List Index nil))

(defun _get_part (Rest Index Res)
  (cond
    ((null Rest) (list Rest Res))
    ((eql Index 0) (list Rest Res))
    (t (_get_part (cdr Rest) (- Index 1) (cons (car Rest) Res)))))

; Left part plus another, reverse left because it is reversed
(defun plus_middle_right (List Index)
  (append (rreverse (cadr List)) (plus_right (get_part (car List) Index))))

; Middle part plus right, middle is reversed
(defun plus_right (List)
  (append (cadr List) (car List)))

; Example
; (main (list 5 4 3 2 -1 6 7 8 9 10 0 1))