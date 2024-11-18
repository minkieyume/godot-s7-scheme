(define amplitude 42)
(define elapsed-time 0.0)

(define (_process delta)

  (inc! elapsed-time :by delta)

  ;; animate sprite
  (let ((x (amplitude-of sin 2.0))
        (y (amplitude-of cos 1.5)))
    (! ($ Sprite2D) 'set_position (Vector2 (+ 500 x) (+ 280 y)))))

(define (amplitude-of f speed)
  "Projects amplitude using f at the current time."
  (+ amplitude (* amplitude (f (* elapsed-time speed)))))
