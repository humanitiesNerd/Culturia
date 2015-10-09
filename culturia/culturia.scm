(define-module (culturia))

(use-modules (wiredtiger))

(use-modules (rnrs hashtables))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-19))  ;; date
(use-modules (srfi srfi-26))  ;; cut
(use-modules (srfi srfi-41))  ;; stream

(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))


;; helper for managing exceptions

(define (make-exception name)
  "Generate a unique symbol prefixed with NAME"
  (gensym (string-append "culturia-" name "-")))

(define *exception* (make-exception "exception"))

(define (raise message . rest)
  "shorthand to throw EXCEPTION with MESSAGE formated with REST"
  (throw *exception* (apply format (append (list #false message) rest))))

;; well, i'm too lazy to create other error messages
(define (Oops!)
  (raise "Oops!"))

;; ---

;;;
;;; srfi-99
;;;
;;
;; macro to quickly define immutable records
;;
;;
;; Usage:
;;
;;   (define-record-type <abc> field-one field-two)
;;   (define zzz (make-abc 1 2))
;;   (abc-field-one zzz) ;; => 1
;;

(define-syntax define-record-type*
  (lambda (x)
    (define (%id-name name) (string->symbol (string-drop (string-drop-right (symbol->string name) 1) 1)))
    (define (id-name ctx name)
      (datum->syntax ctx (%id-name (syntax->datum name))))
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ rname field ...)
       (and (identifier? #'rname) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'rname #'make- (id-name #'rname #'rname)))
                     (pred (id-append #'rname (id-name #'rname #'rname) #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f (id-name #'rname #'rname) #'- f))
                                        #'(field ...))))
         #'(define-record-type rname
             (cons field ...)
             pred
             (field getter)
             ...))))))

;; ---

(define (string->scm value)
  "serialize VALUE with `read` as scheme objects"
  (with-input-from-string value (lambda () (read))))

(define (scm->string value)
  "Write VALUE in a string and return it"
  (with-output-to-string (lambda () (write value))))

;; --

;;; <culturia> is handle over the underlying backing store

(define-record-type* <culturia>
  connection
  session
  ;; <atom> cursors
  atoms  ;; main cursor all the atoms used for direct access via uid
  atoms-append  ;; secondary cursor for insert
  ;; <arrow> cursor
  arrows
  arrows-append
  arrows-outgoings ;; cursor for fetching outgoings set
  arrows-incomings
  ;; indexers
  indexers
  )


(set-record-type-printer! <culturia>
                          (lambda (record port)
                            (format port
                                    "<culturia ~s>"
                                    (culturia-connection record))))

;; ---

;;; user defined indices

(define-record-type* <indexer>
  keys
  cursor
  cursor-append
  cursor-rindex§


(define (indexers->assoc indexers)
  (map (lambda (indexer)
         (cons (indexer-keys indexer) indexer))
       indexers))


(define-public (culturia-create-indexer name keys)
  (lambda (session)
    (let ((table (string-append "table:" name))
          (reverse-index (string-append "index:" name ":reverse")))

      (session-create session
                      table
                      "key_format=r,value_format=uQ,columns=(key,znumber,uid)")

      (session-create session
                      reverse-index
                      "columns=(znumber,uid)")

      (make-indexer name
                    keys
                    (cursor-open session table)
                    (cursor-open session table "append")
                    (cursor-open session (string-append reverse-index "(key)"))))))


(define (culturia-indexer-ref culturia keys)
  (assoc-ref (culturia-indexers culturia) keys))


(define (atom-ref* atom keys)
  "Retrieve all KEYS from atom or nothing"
  (let loop ((keys keys)
             (out (list)))
    (if (null? keys)
        out
        (if (atom-ref (car keys))
            (loop (cdr keys) (append values (list (atom-ref (car names)))))
            (list)))))


(define (cursor-ref* cursor . key)
  (with-cursor cursor
    (apply cursor-key-set (append (list cursor) key))
    (if (cursor-search cursor)
        (cursor-value-ref cursor)
        #nil)))


(define (cursor-remove* cursor . key)
  (with-cursor cursor
    (apply cursor-key-set (list cursor) key)
    (cursor-search cursor)
    (cursor-remove cursor)))


(define (cursor-insert* cursor key value)
  (apply cursor-key-set (list cursor) key)
  (apply cursor-value-set (list cursor) value)
  (cursor-insert cursor))


(define-public (culturia-index-remove culturia keys atom)
  "Remove previous reference to ATOM in the index NAME"
  (let* ((session (culturia-session culturia))
         (indexer (culturia-indexer-ref culturia name))
         (znumber (zpack* indexer))
         (uid (atom-uid atom)))
    ;; remove entry
    (let ((key (cursor-ref* (indexer-rindex indexer) znumber uid)))
      (when key
        (cursor-remove* (indexer-cursor indexer) key)))))


(define-public (culturia-index-insert culturia keys atom)
  "Insert atom inside the indexer NAME"
  (let* ((session (culturia-session culturia))
         (indexer (culturia-indexer-ref culturia keys))
         (znumber (zpack* (atom-ref* atom keys)))
         (uid (atom-uid atom)))
    (cursor-insert* (indexer-cursor indexer) (list znumber uid) (list uid))))


;; culturia-index-ref

(define (cursor-search-near* cursor key)
  "Search near KEY on CURSOR and prepare a forward range"
  (apply cursor-key-set (append (list cursor) key))
  (let ((code (cursor-search-near cursor)))
    (if (not code)
        #false
        (if (eq? (code -1))
            (if (cursor-next cursor)
                #true
                #false)
            #true))))


(define (cursor-search-near* cursor key)
  (apply cursor-key-set (append (list cursor) key))
  (cursor-search-near cursor))


(define (cursor-map* next? proc cursor)
  (let loop ((out (list))
             (next #true))
    (if next
        (loop (cons (proc (cursor-key-ref cursor) (cursor-value-ref cursor)) out)
              (next?))
        out)))


(define (prefix? key other)
  "Return #true if OTHER has KEY as prefix"
  ;; filter "empty" values from the key
  (define (empty? x) (or (null? x) (equal? x "") (eq? x #vu8())))
  (define (predicate a b) (not (or (empty? a) (equal? a b))))
  (not (any predicate key other)))


(define (cursor-near* cursor . key)
  "Return CURSOR range association where keys match PREFIX"
  (define (next?)
    (if (cursor-next cursor)
        (prefix? key (cursor-key-ref cursor))
        #false))

  (with-cursor cursor
    (if (apply cursor-search* (append (list cursor) prefix))
        (cursor-map* next? cons cursor)
        (list))))


(define-public (culturia-index-ref culturia name assoc)
  (let* ((keys (map car assoc))
         (indexer (culturia-indexer-ref culturia keys))
         (cursor (indexer-cursor-rindex indexer))
         (znumber (zpack (map cdr assoc))))
    (if (cursor-search-near* cursor znumber 0)
        (cursor-range* cursor znumber 0)))


;; ---

(define-public (open-culturia path indexers)
  (let* ((connection (connection-open path "create"))
         (session (session-open connection)))
    ;; create a main table to store <atom>
    (session-create session
                    "table:atoms"
                    (string-append "key_format=r,"
                                   "value_format=S,"
                                   "columns="
                                   "(uid,assoc)"))

    ;; create a main table to store <arrow>
    (session-create session
                    "table:arrows"
                    (string-append "key_format=r,"
                                   "value_format=QQ,"
                                   "columns=(uid,start,end)"))
    ;; this index is useful to traverse outgoing set
    (session-create session "index:arrows:outgoings" "columns=(start)")
    ;; this index is useful to traverse incoming set
    (session-create session "index:arrows:incomings" "columns=(end)")

    (make-culturia connection
                   session
                   ;; <atom> cursors
                   (cursor-open session "table:atoms")
                   (cursor-open session "table:atoms" "append")
                   ;; <arrow> cursor
                   (cursor-open session "table:arrows")
                   (cursor-open session "table:arrows" "append")
                   (cursor-open session "index:arrows:outgoings(uid,end)")
                   (cursor-open session "index:arrows:incomings(uid,start)")
                   ;; indexers
                   (let next ((indexers indexers)
                              (out (list)))
                     (if (null? indexers)
                         out
                         (next (cdr indexers)
                               (cons out ((car indexers) session))))))))



;; ---

;;; transactions procedures

(define-public (culturia-close culturia)
  (connection-close (culturia-connection culturia)))


(define-public (culturia-begin culturia)
  (session-transaction-begin (culturia-session culturia)))


(define-public (culturia-commit culturia)
  (session-transaction-commit (culturia-session culturia)))


(define-public (culturia-rollback culturia)
  (session-transaction-rollback (culturia-session culturia)))


(define-syntax-rule (with-transaction culturia e ...)
  (begin
    (culturia-begin culturia)
    e ...
    (culturia-commit culturia)))


(export with-transaction)


;; ---

;;; <atoms>


(define-record-type* <atom> culturia uid assoc)


(export atom-uid atom-assoc)


(define-public (culturia-ref culturia uid)
  (let ((cursor (culturia-atoms culturia)))
    (cursor-key-set cursor uid)
    (when (not (cursor-search cursor))
      (Oops!))
    (let ((assoc (string->scm (car (cursor-value-ref cursor)))))
      (make-atom culturia uid assoc))))


;;

(define (atom-insert atom)
  (let ((cursor (culturia-atoms-append (atom-culturia atom))))
    (cursor-value-set cursor (scm->string (atom-assoc atom)))
    (cursor-insert cursor)
    ;; return a new version of the <atom>
    (make-atom (atom-culturia atom) (car (cursor-key-ref cursor)) (atom-assoc atom))))


(define (atom-update atom)
  (let ((cursor (culturia-atoms (atom-culturia atom))))
    (cursor-key-set cursor (atom-uid atom))
    (when (not (cursor-search cursor))
      (Oops!))
    (cursor-value-set cursor (scm->string (atom-assoc atom)))
    (cursor-update cursor)
    atom))


;;


(define*-public (create-atom culturia #:optional (assoc (list)))
  (atom-insert (make-atom culturia #nil assoc)))


(define-public (atom-set atom key value)
  (let* ((assoc (atom-assoc atom))
         (assoc (alist-delete key assoc))
         (assoc (acons key value assoc)))
    (atom-update (make-atom (atom-culturia atom) (atom-uid atom) assoc))))


(define-public (atom-ref atom key)
  (assoc-ref (atom-assoc atom) key))


(define-public (atom-link atom other)
  (let ((cursor (culturia-arrows-append (atom-culturia atom))))
    (cursor-value-set cursor (atom-uid atom) (atom-uid other))
    (cursor-insert cursor)))


(define-public (atom-unlink atom other)
  (let ((cursor (culturia-arrows (atom-culturia atom))))
    (cursor-key-set cursor (atom-uid atom))
    (when (not (cursor-search cursor))
      (Oops!))
    (let loop ((next #true))
      (if (not next)
          (Oops!)
          (if (eq? (atom-uid other) (car (cursor-value-ref cursor)))
              (cursor-remove cursor)
              (loop (cursor-next)))))))


;; arrows stream

(define (atom-arrow atom cursor)
  (let ((uid (atom-uid atom)))
    (with-cursor cursor
      (cursor-key-set cursor uid)
      (if (cursor-search cursor)
          (let loop ((atoms (list))
                     (next #true))
            (if next
                ;; check that we did not go behind the current atom arrows
                (if (eq? (car (cursor-key-ref cursor)) uid)
                    (loop (cons (cadr (cursor-value-ref cursor)) atoms)
                          (cursor-next cursor))
                    atoms)
                atoms))
          (list)))))


(define-stream (uids->stream ref uids)
  (if (null? uids)
      stream-null
      (stream-cons (ref (car uids))
                   (uids->stream ref (cdr uids)))))


(define-public (atom-outgoings atom)
  "Return a stream of <gremlin> of the outgoings arrows of ATOM"
  (let* ((cursor (culturia-arrows-outgoings (atom-culturia atom)))
         (uids (atom-arrow atom cursor))
         (ref (cut culturia-ref (atom-culturia atom) <>)))
    (uids->stream ref uids)))


(define-public (atom-incomings atom)
  "Return a stream of <gremlin> of the incomings arrows of ATOM"
  (let* ((cursor (culturia-arrows-incomings (atom-culturia atom)))
         (uids (atom-arrow atom cursor))
         (ref (cut culturia-ref (atom-culturia atom) <>)))
    (uids->stream ref uids)))


;; define atom delete


(define (remove-arrows uid cursor)
  (let ((arrows (culturia-arrows culturia)))
    (cursor-key-set cursor uid)
    (when (cursor-search cursor)
      (let loop ((next #true))
        (when (eq? (and next (car (cursor-key-ref cursor)) uid))
          (cursor-key-set arrows (car (cursor-value-ref cursor)))
          (cursor-search arrows)
          (cursor-remove arrows)
          (loop (cursor-next next)))))))


(define-public (atom-delete atom)
  (let* ((culturia (atom-culturia atom))
         (atoms (culturia-atoms culturia))
         (outgoings (culturia-arrows-outgoings culturia))
         (incomings (culturia-arrows-incomings culturia)))

    ;; remove atom entry
    (cursor-key-set atoms (atom-uid atom))
    (when (not (cursor-search atoms))
      (Oops))
    (cursor-remove atoms)

    ;; remove outgoings arrows
    (remove-arrows (atom-uid atom) outgoings)
    (remove-arrows (atom-uid atom) incomings)))


;; ---

;;; traverse framework

;; very much inspired from gremlin
;; http://tinkerpop.incubator.apache.org/docs/3.0.0-incubating/
;;
;; traverser procedures that takes a stream of <gremlin> as input
;; and return another stream of <gremlin> possibly of different type.
;; Most of the time those <gremlin> value is atoms uid ie. integers.
;;
;; traverser procedures are prefixed with ":" character