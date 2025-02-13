(use-modules (tiny-ecs))

(clear-entities)
(clear-systems)

(define (print args)
  (if (not (null? args))
      (begin
	(display (car args))
	(print (cdr args)))))


(define eid1 (make-entity))
(define eid2 (make-entity))

(add-component! eid1 'position (cons 0 0))
(add-component! eid1 'velocity (cons 5 0))

(register-system '(position velocity)
                 (lambda (eid)
                   (let* ((pos (get-component eid 'position))
                          (vel (get-component eid 'velocity)))
                     (print
                      (list "Updating entity: "
                            eid
                            " "
                            "position: "
                            pos
                            " "
                            " velocity:"
                            vel))
                     (newline))))

(define *attacks* (list))

(define (get-attack eid)
  (define (inner attacks)
    (if (null? attacks)
        '()
        (if (eq? (caar attacks) eid)
            (cdar attacks)
            (inner (cdr attacks)))))
  (inner *attacks*))

(define (attack eid dmg)
  (set! *attacks* (cons (cons eid dmg) *attacks*)))

(define monster (make-entity))
(add-component! monster 'health 25)

(register-system '(health)
                 (lambda (eid)
                   (let* ((health (get-component eid 'health))
                          (attack (get-attack eid)))
                     (if (not (null? attack))
                         (add-component! eid 'health (- health attack))))))

(update-systems)
(display "monster health before attack: ")
(display (get-component monster 'health))
(newline)
(attack monster 12)
(attack eid1 10)
(update-systems)
(newline)
(display "monster health after attack: ")
(display (get-component monster 'health))
(newline)



