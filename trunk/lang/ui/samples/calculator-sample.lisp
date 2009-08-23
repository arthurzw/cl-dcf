(in-package :com.google.catharsis.ui)

(def-var operand (int))
(def-var new-value (optional (int)))
(def-var op (optional (enum :+ :- :* :/)))
(def-var display-value (int)
  (lambda () (or new-value operand)))

(defun calculate ()
  (setf operand
        (funcall (ecase op (:+ #'+) (:- #'-) (:* #'*) (:/ #'/))
                 operand
                 new-value))
  (setf new-value nil))

(def-proto calculator-key
    (button (width (em 2))))

(def-template num-key (num)
  (calculator-key (label num)
                  color:light-gray
                  (handler ((type ui:clicked))
                           (setf new-value
                                 (+ (* 10 (or new-value 0))
                                    num)))))

(def-template op-key (op~)
  (calculator-key (label (ecase op~ (:+ "+") (:- "-") (:* "*") (:/ "/")))
                  color:light-cyan
                  (handler ((type ui:clicked))
                           (if op (calculate) (setf operand new-value))
                           (setf op op~)
                           (setf new-value nil))))

(def-template enter-key ()
  (calculator-key (label "=")
                  color:light-cyan
                  (handler ((type ui:clicked))
                           (calculate)
                           (setf op nil))))

(def-form calculator (title "Calculator")
  (vertical-layout
   (def-text display (value (ref display-value)) (border))
   (grid
    (row (num-key 7) (num-key 8) (num-key 9) (op-key :/))
    (row (num-key 4) (num-key 5) (num-key 6) (op-key :*))
    (row (num-key 1) (num-key 2) (num-key 3) (op-key :-))
    (row (num-key 0) (spacer)    (enter-key) (op-key :+)))))
