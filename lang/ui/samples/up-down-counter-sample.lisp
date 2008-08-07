(in-package :com.google.catharsis.ui)

(def-var counter-value (int) (default-value 0))

(def-form up-down-counter-sample
  (title "Up/Down Counter -- Sample")

  (horizontal-layout
    (def-text display (value counter-value))

    (def-button up (label "Up"))
    (def-button down (label "Down"))
    (def-button reset (label "Reset")
      (handler ((type ui:clicked))
	(setf counter-value 0)))

    (handler ((ui:widget up) (type ui:clicked))
      (incf counter-value))
    (handler ((ui:widget down) (type ui:clicked))
      (decf counter-value))
    (handler ()
      (log "This illustrates a catch-all handler."))))
