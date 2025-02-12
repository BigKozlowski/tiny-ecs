(use-modules (entity) (component))

(define ecs (make-vtable "pwpwpw"))

(define (entities) (struct-ref *ecs* 0))
(define (components) (struct-ref *ecs* 1))
(define (systems) (struct-ref *ecs* 2))
(define *ecs* '())

(define (make-ecs)
  (make-struct/no-tail ecs (make-hash-table) (make-hash-table) (make-hash-table)))

(define (init-ecs)
  (let ((id 0))
    (set! *ecs* (make-ecs))
    (define (new-id)
      (+ id 1))
    (values)))

