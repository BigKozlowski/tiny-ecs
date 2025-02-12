(define-module (tiny-ecs)
  #:export (
            make-entity
            add-component!
            get-component
            register-system
            update-systems
            all-entities
            get-entity
            ))

(use-modules (srfi srfi-1))

(define *entity-registry* (make-hash-table))

(define (all-entities)
  (hash-keys *entity-registry*))

(define (hash-keys h)
  (hash-map->list (lambda (key value) key) h))

(define (make-entity)
  (let ((eid (gensym "entity")))
    (hash-set! *entity-registry* eid (make-hash-table))
    eid))

(define (get-entity eid)
  (let ((entity (hash-ref *entity-registry* eid)))
    (if entity
	entity
	(error "Entity does not exist"))))

(define (add-component! eid comp-type data)
  (let ((components (get-entity eid)))
    (if components
        (hash-set! components comp-type data)
        (error "Entity does not exist"))))

(define (get-component eid comp-type)
  (let ((components (get-entity eid)))
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

