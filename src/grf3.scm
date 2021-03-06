;; Copyright © 2016 Amirouche BOUBEKKI <amirouche@hypermove.net>
(define-module (grf3))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (srfi srfi-41))

(use-modules (ice-9 match))
(use-modules (ice-9 receive))

(use-modules (plain))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))
(use-modules (ukv))


(define-record-type* <vertex> uid assoc)

(export vertex-assoc vertex-uid)

(define-record-type* <edge> uid assoc)

(export edge-uid edge-assoc)

(define-public VERTEX 0)
(define-public EDGE 1)

(define-public (get uid)
  (let ((assoc (ukv-ref* uid)))
    (if (eq? (assoc-ref assoc '%kind) VERTEX)
        (make-vertex uid assoc)
        (make-edge uid assoc))))

(define-public (create-vertex assoc)
  (let ((assoc (acons '%kind VERTEX assoc)))
    (let ((uid (ukv-add! assoc)))
      (make-vertex uid assoc))))

(define-public (vertex-set vertex key value)
  (let* ((assoc (vertex-assoc vertex))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field vertex (vertex-assoc) assoc)))

(define-public (vertex-ref vertex key)
  (assoc-ref (vertex-assoc vertex) key))

(define-public (create-edge start end assoc)
  (let* ((assoc (acons '%kind EDGE assoc))
         (assoc (acons '%start (vertex-uid start) assoc))
         (assoc (acons '%end (vertex-uid end) assoc)))
    (let ((uid (ukv-add! assoc)))
      (make-edge uid assoc))))

(define-public (edge-start edge)
  (assoc-ref (edge-assoc edge) '%start))

(define-public (edge-end edge)
  (assoc-ref (edge-assoc edge) '%end))

(define-public (edge-set edge key value)
  (let* ((assoc (edge-assoc edge))
         (assoc (acons key value (alist-delete key assoc))))
    (set-field edge (edge-assoc) assoc)))

(define-public (edge-ref edge key)
  (assoc-ref (edge-assoc edge) key))

(define-public (save vertex-or-edge)
  (let ((uid (if (vertex? vertex-or-edge) (vertex-uid vertex-or-edge) (edge-uid vertex-or-edge)))
        (assoc (if (vertex? vertex-or-edge) (vertex-assoc vertex-or-edge) (edge-assoc vertex-or-edge))))
    (ukv-update! uid assoc))
  vertex-or-edge)

;;; traversi streams

(define-public (list->traversi lst)
  (let loop ((lst lst))
    (lambda ()
      (if (null? lst)
          '()
          (cons (cons (car lst) '()) (loop (cdr lst)))))))

(define-public (traversi->list traversi)
  (let loop ((traversi traversi)
             (out '()))
    (match (traversi)
      ('() (reverse out))
      ((item . next) (loop next (cons (car item) out))))))

(define-public (traversi-car traversi)
  (match (traversi)
    ('() (throw 'traversi "traversi is empty"))
    ((item . next) (car item))))

(define-public (traversi-cdr traversi)
  (match (traversi)
    ('() (throw 'traversi "traversi is empty"))
    ((item . next) next)))

(define-public (traversi-map proc traversi)
  (let loop ((traversi traversi))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (cons (cons (proc (car item)) item) (loop next)))))))

(define-public (traversi-for-each proc traversi)
  (let loop ((traversi traversi))
    (let ((value (traversi)))
      (unless (null? value)
        (proc (caar value))
        (loop (cdr value))))))

(define-public (traversi-filter proc traversi)
  (let loop1 ((traversi traversi))
    (lambda ()
      (let loop2 ((traversi traversi))
        (match (traversi)
          ('() '())
          ((item . next) (if (proc (car item))
                             (cons item (loop1 next))
                             (loop2 next))))))))

(define-public (traversi-backtrack traversi)
  (let loop ((traversi traversi))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next)
         (let ((parents (cdr item)))
           (if (null? parents)
               (throw 'traversi "item has no parent")
               (cons parents (loop next)))))))))

(define-public (traversi-take count traversi)
  (let loop ((traversi traversi)
             (count count))
    (lambda ()
      (if (eq? count 0)
          '()
          (match (traversi)
            ('() '())
            ((item . next) (cons item (loop next (1- count)))))))))

(define-public (traversi-drop count traversi)
  (let loop ((traversi traversi)
             (count count))
    (lambda ()
      (match (traversi)
        ('() '())
        ((item . next) (if (eq? count 0)
                           (cons item (loop next 0))
                           ((loop next (1- count)))))))))


(define-public (traversi-paginator count traversi)
  (throw 'grf3 "not implemented error"))

(define-public (traversi-length traversi)
  (let loop ((traversi traversi)
             (count 0))
    (match (traversi)
      ('() count)
      ((item . next) (loop next (1+ count))))))

(define-public (traversi-scatter traversi)
  "Take a traversi of lists and returns a traversi made of all the
   elements of all the lists. parents are inherited."
  (let loop ((traversi traversi)
             (lst '())
             (parents '()))
    (lambda ()
      (if (null? lst)
          (match (traversi)
            ('() '())
            ((item . next)
             (let ((lst (car item))
                   (parents (cdr item)))
               (if (null? lst)
                   ((loop next '() '()))
                   (cons (cons (car lst) parents)
                         (loop next (cdr lst) parents))))))
          (cons (cons (car lst) parents)
                (loop traversi (cdr lst) parents))))))

(define-public (traversi-unique traversi)
  (let ((seen '()))
    (let loop1 ((traversi traversi))
      (lambda ()
        (let loop2 ((traversi traversi))
          (match (traversi)
            ('() '())
            ((item . next) (if (list-index (cut equal? <> (car item)) seen)
                               (loop2 next)
                               (begin (set! seen (cons (car item) seen))
                                      (cons item (loop1 next)))))))))))

(define (hash-increment ht key)
  (let ((value (hash-ref ht key)))
    (if (not value)
        (hash-set! ht key 1)
        (hash-set! ht key (1+ value)))))

(define-public (traversi-group-count traversi)
  (let ((groups (make-hash-table)))
    (let loop ((traversi traversi))
      (match (traversi)
        ('() (sort (hash-map->list cons groups) (lambda (a b) (> (cdr a) (cdr b)))))
        ((item . next)
         (hash-increment groups (car item))
         (loop next))))))

;;; traversi helpers

(define-public (vertices)
  (list->traversi (ukv-index-ref '%kind VERTEX)))

(define-public (edges)
  (list->traversi (ukv-index-ref '%kind EDGE)))

(define-public (from key value)
  (list->traversi (ukv-index-ref key value)))

(define-public (where? key value)
  (lambda (uid)
    (equal? (ukv-ref uid key) value)))

(define-public (key name)
  (lambda (uid)
    (ukv-ref uid name)))

(define-public (key? name value)
  (lambda (uid)
    (catch 'wiredtiger
      (lambda () (equal? (ukv-ref uid name) value))
      (lambda _ #f))))

(define-public incomings
  (lambda (uid)
    (ukv-index-ref '%end uid)))

(define-public outgoings
  (lambda (uid)
    (ukv-index-ref '%start uid)))

(define-public start
  (lambda (uid)
    (ukv-ref uid '%start)))

(define-public end
  (lambda (uid)
    (ukv-ref uid '%end)))

;;; other helpers

(define-public (get-or-create-vertex key value)
  (let ((uids (traversi->list (traversi-filter (where? '%kind VERTEX) (from key value)))))
    (if (null? uids)
        (values #true (create-vertex (acons key value '())))
        (values #false (get (car uids))))))

;;; tests

(use-modules (test-check))


(when (or (getenv "CHECK") (getenv "CHECK_GRF3"))
  (format #t "* Testing grf3\n")

  (test-check "open database"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      42)
    42)

  (test-check "create vertex"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let ((vertex (create-vertex '((a . 42)))))
        (vertex-ref (get (vertex-uid vertex)) 'a)))
    42)

  (test-check "create edge"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((start (create-vertex '()))
             (end (create-vertex '()))
             (edge (create-edge start end '((a . 42)))))
        (edge-ref (get (edge-uid edge)) 'a)))
    42)

  (test-check "get-or-create-vertex new true"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new vertex) (get-or-create-vertex 'a 42)
        new))
    #true)

  (test-check "get-or-create-vertex new false"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (receive (new vertex) (get-or-create-vertex 'a 42)
        (receive (new vertex) (get-or-create-vertex 'a 42)
          new)))
    #false)

  (test-check "save vertex"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((a (create-vertex '((a . 42))))
             (a (vertex-set a 'b 1337)))
        (save a)
        (vertex-ref (get (vertex-uid a)) 'b)))
    1337)

  (test-check "save edge"
    (with-env (env-open* "/tmp/wt" (list *ukv*))
      (let* ((start (create-vertex '()))
             (end (create-vertex '()))
             (edge (create-edge start end '((a . 42)))))
        (save (edge-set edge 'b 1337))
        (edge-ref (get (edge-uid edge)) 'b)))
    1337)

  (test-check "traversi basics"
    (traversi->list (list->traversi (iota 5)))
    '(0 1 2 3 4))


  (test-check "traversi-car"
    (traversi-car (list->traversi (iota 5)))
    0)

  (test-check "traversi-cdr"
    (traversi->list (traversi-cdr (list->traversi (iota 5))))
    '(1 2 3 4))

  (test-check "traversi-unique"
    (traversi->list (traversi-unique (list->traversi '(1 1 2 2 3 3))))
    '(1 2 3))

  (test-check "traversi-map"
    (traversi->list (traversi-map 1+ (list->traversi (iota 5))))
    '(1 2 3 4 5))

  (test-check "traversi-filter"
    (traversi->list (traversi-filter odd? (list->traversi (iota 5))))
    '(1 3))

  (test-check "traversi-backtrack"
    (traversi->list (traversi-backtrack (traversi-filter odd? (traversi-map 1+ (list->traversi (iota 5))))))
    '(0 2 4))

  (test-check "traversi-take"
    (traversi->list (traversi-take 2 (list->traversi (iota 5))))
    '(0 1))

  (test-check "traversi-group-count"
    (traversi-group-count (list->traversi '(1 1 1 2 2 3)))
    '((1 . 3) (2 . 2) (3 . 1)))

  (test-check "traversi-drop"
    (traversi->list (traversi-drop 2 (list->traversi (iota 5))))
    '(2 3 4))

  (test-check "traversi-length"
    (traversi-length (list->traversi (iota 5)))
    5)

  (test-check "traversi-scatter 1"
    (traversi->list (traversi-scatter (list->traversi (list (iota 5)))))
    '(0 1 2 3 4))

  (test-check "traversi-scatter 2"
    (traversi->list (traversi-scatter (list->traversi (list (iota 2) (iota 2)))))
    '(0 1 0 1))

  (test-check "traversi-scatter 3"
    (traversi->list
     (traversi-filter odd?
                      (traversi-scatter (traversi-map iota (list->traversi (iota 3))))))
    '(1))

  (test-check "traversi-scatter 4"
    (traversi->list
     (traversi-backtrack
      (traversi-filter odd?
                       (traversi-scatter (traversi-map iota (list->traversi (iota 3)))))))
    '(2))

  )
