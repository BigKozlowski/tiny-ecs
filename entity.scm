(use-modules (ice-9 hash-table) (srfi srfi-1))

(define-module (entity)
  #:export (add-entity all-entities entity-attr))

(define (make-entity)
  (list 'components '() 'attributes '() 'tags '()))

(define (entity-components entity) (cadr entity))
(define (set-entity-components! entity value) (set-car! (cdr entity) value))

(define (entity-attributes entity) (cadddr entity))
(define (set-entity-attributes! entity value) (set-car! (cdddr entity) value))

(define (entity-tags entity) (cadddr (cdr entity)))
(define (set-entity-tags! entity value) (set-car! (cdddr (cdr entity)) value))

(define ecs-entities (make-hash-table))

(define (new-id)
  (gensym "entity-"))

(define (add-entity prototype . components)
  (let* ((id (new-id))
         (entity (make-entity)))
    (hash-set! ecs-entities id entity)
    (when (hash-ref ecs-entities prototype #f)  ;; Ensure prototype is valid
      (copy-prototype (hash-ref ecs-entities prototype) id))
    (for-each (lambda (comp) (add-component id (car comp) (cdr comp)))
              components)
    id))

(define (all-entities)
  (map car (hash-map->list cons ecs-entities)))

(define (entity-tags id)
  (entity-tags (hash-ref ecs-entities id)))

(define (set-entity-tags! id value)
  (set-entity-tags! (hash-ref ecs-entities id) value))

(define (all-tags? id . tags)
  (every (lambda (tag) (member tag (entity-tags id))) tags))

(define (some-tags? id . tags)
  (any (lambda (tag) (member tag (entity-tags id))) tags))

(define (add-tags id . tags)
  (let ((all (entity-tags id)))
    (set-entity-tags! id (append tags all))))

(define (remove-tags id . tags)
  (set-entity-tags! id (filter (lambda (tag) (not (member tag tags)))
                               (entity-tags id))))

(define (entity-attrs id)
  (entity-attributes (hash-ref ecs-entities id)))

(define (set-entity-attrs! id value)
  (set-entity-attributes! (hash-ref ecs-entities id) value))

(define (entity-attr id field)
  (let ((attrs (entity-attrs id)))
    (if (list? attrs)
        (assoc-ref attrs field)
        #f)))

(define (set-entity-attr! id field value)
  (let ((attrs (entity-attrs id)))
    (set-entity-attrs! id (cons (cons field value) attrs))))

(define (remove-entity-attr id field)
  (set-entity-attrs! id (filter (lambda (pair) (not (eq? (car pair) field)))
                                (entity-attrs id))))

(define (copy-prototype from to)
  (when (and from (pair? from))  ;; Ensure 'from' is a pair before using cdr
    (set-entity-components! to (map (lambda (x) x) (entity-components from)))
    (set-entity-attrs! to (map (lambda (x) x) (entity-attrs from)))))

(define (remove-entity id)
  (hash-remove! ecs-entities id))

(define (add-entity prototype . components)
  (let* ((id (new-id))
         (entity (make-entity)))
    (hash-set! ecs-entities id entity)
    (when (hash-ref ecs-entities prototype #f)  ;; Ensure prototype is valid
      (copy-prototype (hash-ref ecs-entities prototype) id))
    (for-each (lambda (comp) (add-component id (car comp) (cdr comp)))
              components)
    id))


(define (add-component id name attrs)
  (let ((entity (hash-ref ecs-entities id)))
    (set-entity-components! entity (cons (cons name attrs) (entity-components entity)))))

(define (every pred ls . lists)
  (if (null? lists)
      (every1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #t)
	      ((any1 null? (map cdr lists))
	       (apply pred (map car lists)))
	      (else
	       (and (apply pred (map car lists)) (lp (map cdr lists))))))))

(define (every1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #t)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (and (pred (car ls)) (lp (cdr ls)))))))
  
(define (any pred ls . lists)
  (if (null? lists)
      (any1 pred ls)
      (let lp ((lists (cons ls lists)))
	(cond ((any1 null? lists)
	       #f)
	      ((any1 null? (map cdr lists))
	       (apply pred (map car lists)))
	      (else
	       (or (apply pred (map car lists)) (lp (map cdr lists))))))))

(define (any1 pred ls)
  (let lp ((ls ls))
    (cond ((null? ls)
	   #f)
	  ((null? (cdr ls))
	   (pred (car ls)))
	  (else
	   (or (pred (car ls)) (lp (cdr ls)))))))
