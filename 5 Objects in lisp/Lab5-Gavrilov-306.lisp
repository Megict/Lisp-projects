;;функции из лекций

(defclass polynom ()
 ((var-symbol :initarg :vari :reader vari)
  ;; Разреженный список термов в порядке убывания степени
  (term-list :initarg :terms  :reader terms)))

(defun make-term (&key order coeff)
  (list order coeff))


(defun order (term) (first term))   ; степень
(defun coeff (term) (second term))  ; коэффициент

(defgeneric zerop1 (arg)
 (:method ((n number))   ; (= n 0)
  (zerop n)))

(defgeneric minusp1 (arg)
 (:method ((n number))   ; (< n 0)
  (minusp n)))

(defmethod print-object ((p polynom) stream)
  (format stream "[МЧ (~s) ~:{~:[~:[+~;-~]~d~[~2*~;~s~*~:;~s^~d~]~;~]~}]"
    (vari p)
    (mapcar (lambda (term)
      (list (zerop1 (coeff term))
        (minusp1 (coeff term))
        (if (minusp1 (coeff term))
          (abs (coeff term))
          (coeff term))
        (order term)
        (vari p)
        (order term)))
    (terms p))))

;;моя функция:

(defun der-polynom (pl) 
  (make-instance 'polynom 
    :vari 
      (vari pl)
    :terms 
      (mapcar (lambda (term)
        (list (- (order term) 1)
              (* (coeff term) (order term))))
        (terms pl))))


        
(defvar poly (make-instance 'polynom
  :vari 'x
  :terms (list (make-term :order 2 :coeff 5)
               (make-term :order 1 :coeff 3.3)
               (make-term :order 0 :coeff -7))))