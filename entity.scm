(define-module (entity)
  #:export (make-entity add-component! get-component! register-system update-systems))

(use-modules (srfi srfi-1))

(define *entity-registry* (make-hash-table))

(define (hash-keys h)
  (hash-map->list (lambda (key value) key) h))

(define (make-entity)
  (let ((eid (gensym "entity")))
    (hash-set! *entity-registry* eid (make-hash-table))
    eid))

(define (add-component! eid comp-type data)
  (let ((components (hash-ref *entity-registry* eid #f)))
    (if components
        (hash-set! components comp-type data)
        (error "Entity does not exist"))))

(define (get-component eid comp-type)
  (let ((components (hash-ref *entity-registry* eid #f)))
    (if components
        (hash-ref components comp-type #f)
        #f)))

(define *systems* '())

(define (register-system required-components process-function)
  (set! *systems*
        (append *systems*
                `((,required-components . ,process-function)))))

(define (update-systems)
  (for-each
   (lambda (system)
     (let* ((required-components (car system))
            (proc (cdr system)))
       (for-each
        (lambda (eid)
          (if (every (lambda (comp) (get-component eid comp)) required-components)
              (proc eid)))
        (hash-keys *entity-registry*))))
   *systems*))



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
