(define (print args)
  (if (not (null? args))
      (begin
	(display (car args))
	(print (cdr args)))))


(let ((eid1 (make-entity))
      (eid2 (make-entity)))

  (add-component! eid1 'position (cons 0 0))
  (add-component! eid1 'velocity (cons 5 0))

  (register-system '(position velocity)
                   (lambda (eid)
                     (let* ((pos (get-component eid 'position))
                            (vel (get-component eid 'velocity)))
                       (print (list "Updating entity: " eid " " "position: " pos " " " velocity:" vel)))))

  (update-systems))
