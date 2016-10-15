;; guile-wiredtiger - 0.4 - 2016/07/07

;; Copyright © 2014-2016 Amirouche BOUBEKKI <amirouche at hypermove net>

;; guile-wiredtiger is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) or version 3.

;; guile-wiredtiger is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with guile-wiredtiger.  If not, see <http://www.gnu.org/licenses/>

;;; Comment:
;;
;; Tested with wiredtiger develop branch
;;

(define-module (wiredtiger))

(use-modules (srfi srfi-9))  ;; records
(use-modules (srfi srfi-9 gnu))  ;; set-record-type-printer!
(use-modules (srfi srfi-26)) ;; cut

(use-modules (rnrs bytevectors))

(use-modules (ice-9 iconv))  ;; string->bytevector
(use-modules (ice-9 match))
(use-modules (ice-9 format))
(use-modules (ice-9 optargs))  ;; lambda*
(use-modules (ice-9 receive))

(use-modules (system foreign))  ;; ffi

;;;
;;; plain
;;;
;;
;; macro to quickly define records
;;
;;
;; Usage:
;;
;;   (define-record-type <car> seats wheels)
;;   (define smart (make-car 2 4))
;;   (car-seats smart) ;; => 2
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

;;; ffi helpers

(define NULL %null-pointer)
(define POINTER '*)

(define* (dynamic-link* #:optional library-name)
  (let ((shared-object (if library-name (dynamic-link library-name) (dynamic-link))))
    (lambda (return-value function-name . arguments)
      (let ((function (dynamic-func function-name shared-object)))
        (pointer->procedure return-value function arguments)))))

(define (pointer->procedure* return-type function-pointer . args_types)
  (pointer->procedure return-type function-pointer args_types))

(define (make constructor pointer size)
  (let* ((pointer (make-pointer (array-ref pointer 0)))
         (struct (map make-pointer (u64vector->list (pointer->bytevector pointer size 0 'u64)))))
    (apply constructor (cons pointer struct))))

;;;
;;; wiredtiger bindings
;;;

(define wiredtiger (dynamic-link* "/gnu/store/fm49ypz5w65gav6xfhx1qj9p8ab2fln8-wiredtiger-next-20160924.d85e189/lib/libwiredtiger.so"))

(define* wiredtiger-string-error
  (let ((function (wiredtiger POINTER "wiredtiger_strerror" int)))
    (lambda (code)
      (pointer->string (function code)))))

(define (check code)
  (unless (eq? code 0)
    (throw 'wiredtiger (wiredtiger-string-error code))))

;;;
;;; Connection
;;;

(define-record-type* <*connection>
  pointer

  async-flush
  async-new-op
  close
  reconfigure
  get-home
  configure-method
  is-new
  open-session
  load-extension
  add-data-source
  add-collator
  add-compressor
  add-encryptor
  add-extractor
  get-extension-api)

(set-record-type-printer! <*connection>
                          (lambda (record port)
                            (format port
                                    "<connection 0x~x>"
                                    (pointer-address (*connection-pointer record)))))

(define-public connection-open
  (let ((function (wiredtiger int "wiredtiger_open" POINTER POINTER POINTER POINTER)))
    (lambda (home config)
      (let* (;; init a double pointer
             (pointer (u64vector 0))
             (double-pointer (bytevector->pointer pointer))
             ;; convert arguments to c types
             (home (string->pointer home))
             (config (string->pointer config)))
      ;; FIXME: add support for error_handler
      (check (function home NULL config double-pointer))
      (make make-*connection pointer 15)))))


(define*-public (connection-close connection #:optional (config ""))
  "Close CONNECTION. Any open sessions or cursors will be closed. CONFIG
   must be a string. It accept a single option `leak_memory` a boolean flag
   which default value is `false`. If `leak_memory` is set to `true` memory
   will not be freed during close"
  (let ((function (pointer->procedure* int (*connection-close connection) POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; convert arguments to c types
           (config (string->pointer config)))
           ;; FIXME: add support for error_handler
      (check (function (*connection-pointer connection) config)))))

(define (make-collator format proc)
  (lambda (collator session key other cmp)
        (let* ((key (pointer->bytevector key 2 0 'u64))
                   (key (%wiredtiger-struct-unpack session (make-pointer (array-ref key 0)) (array-ref key 1) format)))
          (let* ((other (pointer->bytevector other 2 0 'u64))
                 (other (%wiredtiger-struct-unpack session (make-pointer (array-ref other 0)) (array-ref other 1) format)))
                        (s32vector-set! (pointer->bytevector cmp 1 0 's32) 0 (proc key other))
                        0))))

(define-public (connection-add-collator connection name format proc)
  "Aedd PROC as a collator named NAME against CONNECTION. PROC must be
   compare its two arguments and return `-1` if `key < other`, `0` if
   `key == other`, `1` if `key > other`."
  (let* ((function (pointer->procedure* int (*connection-add-collator connection) '* '* '* '*))
                 (collator (pointer-address (procedure->pointer int (make-collator format proc) (list '* '* '* '* '*))))
                 (collator (bytevector->pointer (u64vector collator 0 0))))
        (check (function (*connection-pointer connection)
                                         (string->pointer name)
                                         collator
NULL))))

;;;
;;; Session
;;;

(define-record-type* <*session>
  pointer

  connection
  app-private
  close
  reconfigure
  string-error
  cursor-open
  create
  compact
  drop
  join
  log-flush
  log-printf
  rebalance
  rename
  reset
  salvage
  truncate
  upgrade
  verify
  transaction-begin
  transaction-commit
  transaction-rollback
  checkpoint
  snapshot
  transaction-pinned-range
  transaction-sync)

(set-record-type-printer! <*session>
                          (lambda (record port)
                            (format port
                                    "<session 0x~x>"
                                    (pointer-address (*session-pointer record)))))

(define (session-string-error* session code)
  (let ((function (pointer->procedure* POINTER (*session-string-error session) POINTER int)))
    (pointer->string (function (*session-pointer session) code))))

(define (session-check session code)
  (unless (eq? code 0)
    (throw 'wiredtiger (session-string-error* session code))))

(define*-public (session-open connection #:optional (config ""))
  "Open a session against CONNECTION. CONFIG accepts `isolation` as
   only options. It can be `read-uncommited`, `read-commited` or
   `snapshot`."
  (let ((function (pointer->procedure* int (*connection-open-session connection) POINTER POINTER POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; convert arguments to c types
           (config (string->pointer config))
           ;; call the foreign function
           ;; FIXME: add support for error_handler
           (code (function (*connection-pointer connection) NULL config double-pointer)))
      (check  code)
      (make make-*session pointer 26))))

(define-public (session-create session name config)
  "Create a table, column group, index or file.

   cf. http://source.wiredtiger.com/develop/struct_w_t___s_e_s_s_i_o_n.html#a358ca4141d59c345f401c58501276bbb"
  (let ((function (pointer->procedure* int (*session-create session) POINTER POINTER POINTER)))
    (let* (;; convert arguments to c types
           (name (string->pointer name))
           (config (string->pointer config))
           ;; call the foreign function
           (code (function (*session-pointer session) name config)))
      (session-check session code))))

(define-public (session-close session)
  "Close the session handle. This will release the resources
   associated with the session handle, including rolling back any active
   transactions and closing any cursors that remain open in the session."
  (let ((function (pointer->procedure* int (*session-close session) POINTER)))
    (session-check session (function (*session-pointer session)))))

(define*-public (session-transaction-begin session #:optional (config ""))
  "Start a transaction. A transaction remains active until ended."
  (let ((function (pointer->procedure* int (*session-transaction-begin session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

(define*-public (session-transaction-commit session #:optional (config ""))
  "Commit the current transaction. A transaction must be in progress when this method is called.

   If the transaction was rolledback, it will throw a `wiredtiger` exception"
  (let ((function (pointer->procedure* int (*session-transaction-commit session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

(define*-public (session-transaction-rollback session #:optional (config ""))
  "Rollback the current transaction. A transaction must be in progress
   when this methods called. All cursors are reset."
  (let ((function (pointer->procedure* int (*session-transaction-rollback session) POINTER POINTER)))
    (session-check session (function (*session-pointer session) (string->pointer config)))))

;;;
;;; Cursor
;;;

(define-record-type* <*cursor>
  pointer
  session
  config

  session-pointer
  uri
  key-format
  value-format
  key-ref
  value-ref
  key-set
  value-set
  compare
  equals
  next
  previous
  reset
  search
  search-near
  insert
  update
  remove
  close
  reconfigure
  ;; XXX: other fields are defined in the header
  ;;      those are only useful to implement a new cursor type
  ;;      and as such are not part the record
  )

(set-record-type-printer! <*cursor>
                          (lambda (record port)
                            (format port
                                    "<cursor 0x~x uri=~s key=~s value=~s config=~s>"
                                    (pointer-address (*cursor-pointer record))
                                    (pointer->string (*cursor-uri record))
                                    (cursor-key-format record)
                                    (cursor-value-format record)
                                    (*cursor-config record))))

(define (cursor-key-format cursor)
  ;; FIXME: cache this value
  (pointer->string (*cursor-key-format cursor)))

(define (cursor-value-format cursor)
  ;; FIXME: cache this value
  (pointer->string (*cursor-value-format cursor)))

(define*-public (cursor-open session uri #:optional (config ""))
  "Open a new cursor on a data source.

   cf. http://source.wiredtiger.com/develop/struct_w_t___s_e_s_s_i_o_n.html#afb5b4a69c2c5cafe411b2b04fdc1c75d"
  (let ((function (pointer->procedure* int (*session-cursor-open session) POINTER POINTER POINTER POINTER POINTER)))
    (let* (;; init a double pointer
           (pointer (u64vector 0))
           (double-pointer (bytevector->pointer pointer))
           ;; call the foreign function
           (code (function (*session-pointer session)
                           (string->pointer uri)
                           NULL
                           (string->pointer config)
                           double-pointer)))
      (session-check session code)
      ;; make cursor record
      (let* ((size 20)
             (pointer (make-pointer (array-ref pointer 0)))
             (struct (map make-pointer (u64vector->list (pointer->bytevector pointer size 0 'u64)))))
        (apply make-*cursor (cons* pointer session config struct))))))

;;; key/value set/ref

(define (item->string bv)
  (pointer->string (make-pointer (array-ref bv 0))))

(define (item->integer bv)
  (array-ref bv 0))

(define (item->signed-integer bv)
  (s32vector-ref bv 0))

(define (item->bytes item)
  (let* ((size (s32vector-ref item 2)))
    (if (zero? size)
        #vu8()
        (bytevector-copy (pointer->bytevector (make-pointer (s64vector-ref item 0)) size 0 'u8)))))

(define *item->value* `((#\S . ,item->string)
                        (#\Q . ,item->integer)
                        (#\q . ,item->signed-integer)
                        (#\r . ,item->integer)
                        (#\u . ,item->bytes)))

(define (pointers->scm formats pointers)
  (let loop ((formats (string->list formats))
             (pointers pointers)
             (out '()))
    (cond
     ((and (null? formats) (null? pointers)) out)
     ((or (null? formats) (null? pointers))
      (throw 'wiredtiger "failed to ref cursor value due to format error"))
     (else (loop (cdr formats)
                 (cdr pointers)
                 (append out (list ((assoc-ref *item->value* (car formats)) (car pointers)))))))))

(define-public (cursor-key-ref cursor)
  "Retrieve the current key"
  (let* ((args (map (lambda _ (u64vector 0 0)) (string->list (cursor-key-format cursor))))
         (args* (cons (*cursor-pointer cursor) (map bytevector->pointer args)))
         (signature (map (lambda _ POINTER) args*))
         (function (pointer->procedure int (*cursor-key-ref cursor) signature)))
    (apply function args*)
    (pointers->scm (cursor-key-format cursor) args)))

(define-public (cursor-value-ref cursor)
  "Retrieve the current value"
  (let* ((args (map (lambda _ (u64vector 0 0)) (string->list (cursor-value-format cursor))))
         (args* (cons (*cursor-pointer cursor) (map bytevector->pointer args)))
         (signature (map (lambda _ POINTER) args*))
         (function (pointer->procedure int (*cursor-value-ref cursor) signature)))
    (apply function args*)
    (pointers->scm (cursor-value-format cursor) args)))

;;; set procedures

(define make-string-pointer
  (compose bytevector->pointer
           (cut string->bytevector <> "utf-8")
           (cut string-append <> "\0")))

(define (make-bytes-item bv)
  (let ((item  (u64vector 0 0)))
    (s32vector-set! item 2 (bytevector-length bv))
    (u64vector-set! item 0 (pointer-address (bytevector->pointer bv)))
    (bytevector->pointer item)))


(define *format->pointer* `((#\S . ,make-string-pointer)
                            (#\Q . ,make-pointer)
                            (#\q . ,make-pointer)
                            (#\r . ,make-pointer)
                            (#\u . ,make-bytes-item)))

(define (formats->items formats values)
  (let loop ((formats (string->list formats))
             (values values)
             (out '()))
    (cond
     ((and (null? formats) (null? values)) out)
     ((or (null? formats) (null? values))
      (throw 'wiredtiger "failed to set cursor due to format error"))
     (else (loop (cdr formats)
                 (cdr values)
                 (append out (list ((assoc-ref *format->pointer* (car formats)) (car values)))))))))

(define-public (cursor-key-set cursor . key)
  "Set the key for the next operation. If an error occurs during this operation,
   a flag will be set in the cursor, and the next operation to access the value
   will fail. This simplifies error handling in applications.

   KEY must consistent with the format of the current object key."
  (let* ((args (cons (*cursor-pointer cursor) (formats->items (cursor-key-format cursor) key)))
         (signature (map (lambda ignore POINTER) args))
         (function (pointer->procedure int (*cursor-key-set cursor) signature)))
    (apply function args)))

(define-public (cursor-value-set cursor . value)
  "Set the value for the next operation. If an error occurs during this operation,
   a flag will be set in the cursor, and the next operation to access the
   value will fail. This simplifies error handling in applications.

   VALUE must consistent with the format of the current object value."

  (let* ((args (cons (*cursor-pointer cursor) (formats->items (cursor-value-format cursor) value)))
         (signature (map (lambda ignore POINTER) args))
         (function (pointer->procedure int (*cursor-value-set cursor) signature)))
    (apply function args)))

(define-public (cursor-reset cursor)
  "Reset the position of the cursor. Any resources held by the cursor are released,
   and the cursor's key and position are no longer valid. A subsequent
   iteration with `cursor-next` will move to the first record, or with
   `cursor-prev` will move to the last record."
  (let ((function (pointer->procedure* int (*cursor-reset cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-next cursor)
  (let ((function (pointer->procedure* int (*cursor-next cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-previous cursor)
  (let ((function (pointer->procedure* int (*cursor-previous cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-search cursor)
  "On sucess move the cursor to the record matching the key. The key
   must first be set.

   To minimize cursor resources, the `cursor-reset` method should be
   called as soon as the record has been retrieved and the cursor no
   longer needs that position."
  (let ((function (pointer->procedure* int (*cursor-search cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-search-near cursor)
  "Return the record matching the key if it exists, or an adjacent record.
   An adjacent record is either the smallest record larger than the key
   or the largest record smaller than the key (in other words, a
   logically adjacent key).  The key must first be set.

   On success, the cursor ends positioned at the returned record; to minimize
   cursor resources, the cursor-reset method should be called as soon as the record
   has been retrieved and the cursor no longer needs that position."
  (let ((function (pointer->procedure* int (*cursor-search-near cursor) POINTER POINTER)))
    (let* ((integer (s32vector 0))
           (pointer (bytevector->pointer integer)))
      (session-check (*cursor-session cursor) (function (*cursor-pointer cursor) pointer))
      (s32vector-ref integer 0))))

(define-public (cursor-insert cursor)
  "Insert a record and optionally update an existing record."
  (let ((function (pointer->procedure* int (*cursor-insert cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-update cursor)
  "Update a record and optionally insert an existing record."
  (let ((function (pointer->procedure* int (*cursor-update cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-remove cursor)
  "Remove a record. The key must be set."
  (let ((function (pointer->procedure* int (*cursor-remove cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-public (cursor-close cursor)
  "Close the cursor. This releases the resources associated with the cursor handle.
   Cursors are closed implicitly by ending the enclosing connection or
   closing the session in which they were opened."
  (let ((function (pointer->procedure* int (*cursor-close cursor) POINTER)))
    (session-check (*cursor-session cursor) (function (*cursor-pointer cursor)))))

(define-syntax-rule (with-cnx connection e ...)
  (let ((cnx connection)
        (out (begin e ...)))
    (connection-close cnx)
    out))

(export with-cnx)

;;; helpers

(define (wiredtiger-struct-size session format . args)
  (let* ((size (u64vector 0))
         (items (formats->items format args))
         (signature (map (lambda _ '*) args))
         (function (apply wiredtiger (cons* int "wiredtiger_struct_size" '* '* '* signature))))
    (check (apply function (cons* (*session-pointer session)
                                  (bytevector->pointer size)
                                  (string->pointer format)
                                  items)))
    (u64vector-ref size 0)))

(define-public (wiredtiger-struct-pack session format . args) 
  (let* ((size (apply wiredtiger-struct-size (cons* session format args)))
         (buffer (apply u8vector (iota size)))
         (items (formats->items format args))
         (signature (map (lambda _ '*) args))
         (function (apply wiredtiger (cons* int "wiredtiger_struct_pack" '* '* size_t '* signature))))
    (check (apply function (cons* (*session-pointer session)
                                  (bytevector->pointer buffer)
                                  size
                                  (string->pointer format)
                                  items)))
    buffer))
    
(define (%wiredtiger-struct-unpack session buffer size format)
  ;; session must the raw pointer, not the record
  (let* ((args (map (lambda _ (u64vector 0)) (string->list format)))
                 (args* (map bytevector->pointer args))
                 (signature (map (lambda _ POINTER) args*))
                 (function (apply wiredtiger (cons* int "wiredtiger_struct_unpack" '* '* size_t '* signature))))
        (apply function (cons* session buffer size (string->pointer format) args*))
(pointers->scm format args)))

(define-public (wiredtiger-struct-unpack session buffer format)
  (%wiredtiger-struct-unpack (*session-pointer session)
                             (bytevector->pointer buffer)
                             (bytevector-length buffer)
                             format))
;;; tests

(use-modules (test-check))

(when (or (getenv "CHECK") (getenv "CHECK_WIREDTIGER"))
  (format #true "* testing wiredtiger\n")

  (test-check "create and close database"
              (with-cnx (connection-open "/tmp/wt" "create") #true)
              #true)

  (test-check "table with index, insert and index value"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                (session-create session "index:nodes:index" "columns=(b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (session-transaction-begin session "isolation=\"snapshot\"")
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "a" "b")
                  (cursor-insert cursor)
                  (session-transaction-commit session)
                  (let ((index (cursor-open session "index:nodes:index(a)")))
                    (cursor-next index)
                    (with-cnx cnx
                      (list (cursor-key-ref index) (cursor-value-ref index))))))
              (list (list "a" "b") (list 42)))

  (test-check "cursor search on empty table"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (catch 'wiredtiger
                      (lambda () (cursor-search cursor) #false)
                      (lambda (key value) #true)))))
              #true)

  (test-check "cursor search"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                ;; create a table
                (session-create session "table:nodes" "key_format=Q,value_format=SS,columns=(a,b,c)")
                ;; open a cursor over that table
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "b" "c")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (catch #true
                      (lambda () (cursor-search cursor) #true)
                      (lambda _ #false)))))
              #true)

  (test-check "cursor search near on empty table"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                            (catch #true
                              (lambda () (cursor-search-near cursor) #false)
                              (lambda _ #true)))))
              #true)

  (test-check "cursor search near below"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 43)
                  (with-cnx cnx
                    (cursor-search-near cursor))))
              -1)

  (test-check "cursor search near above"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 41)
                  (cursor-value-set cursor "another number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 45)
                  (cursor-value-set cursor "random number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 43)
                  (with-cnx cnx
                    (< 0 (cursor-search-near cursor)))))
              #true)

  (test-check "cursor search near exact match"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:nodes" "key_format=Q,value_format=S,columns=(a,b)")
                (let ((cursor (cursor-open session "table:nodes")))
                  ;; prepare
                  (cursor-key-set cursor 41)
                  (cursor-value-set cursor "another number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 42)
                  (cursor-value-set cursor "magic number")
                  (cursor-insert cursor)
                  (cursor-key-set cursor 45)
                  (cursor-value-set cursor "random number")
                  (cursor-insert cursor)
                  ;; test
                  (cursor-key-set cursor 42)
                  (with-cnx cnx
                    (cursor-search-near cursor))))
              0)

  (test-check "record table, insert and retrieve key"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:terms" "key_format=r,value_format=S")
                (let ((cursor (cursor-open session "table:terms" "append")))
                  (cursor-value-set cursor "timesink")
                  (cursor-insert cursor)
                  (with-cnx cnx (car (cursor-key-ref cursor)))))
              1)

  (test-check "raw item, insert and retrieve value"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (session-create session "table:terms" "key_format=r,value_format=u")
                (let ((cursor (cursor-open session "table:terms" "append")))
                  (cursor-value-set cursor (u8vector 1 2 3 4))
                  (cursor-insert cursor)
                  (cursor-reset cursor)
                  (cursor-next cursor)
                  (with-cnx cnx
                    (cursor-value-ref cursor))))
              '(#vu8(1 2 3 4)))

  (test-check "wiredtiger-struct-size"
              (let* ((cnx (connection-open "/tmp/wt" "create"))
                     (session (session-open cnx)))
                (with-cnx cnx
                  (wiredtiger-struct-size session "qQS" 1 42 "héllo")))
              '9)

  (test-check "wiredtiger-struct-pack/unpack"
    (let* ((cnx (connection-open "/tmp/wt" "create"))
           (session (session-open cnx)))
      (with-cnx cnx
        (wiredtiger-struct-unpack session 
                                  (wiredtiger-struct-pack session "qQS" 1 42 "héllo")
                                  "qQS")))
    '(1 42 "héllo"))

  ;; (test-check "create table with scheme collator"
  ;;   (receive (cnx ctx) (wiredtiger-open* "/tmp/wt" '(table
  ;;                                                    ((key . record))
  ;;                                                    ((scheme . string))
  ;;                                                    ((reversed (scheme) (key)))))

  ;;     (connection-add-collator cnx "ci" "Sr" (lambda (key other)
  ;;                                              (if (string=? (car key) (car other))
  ;;                                                  0
  ;;                                                  (if (string-ci<? (car key) (car other)) -1 1))))
  ;;     (session-create session "table:terms" "key_format=r,value_format=S,columns=(a,b)")
  ;;     (session-create session "index:terms:reversed" "columns=(b),collator=ci")

  ;;     (let ((cursor (cursor-open session "table:terms" "append")))
  ;;       (cursor-value-set cursor "a")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "A")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "b")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "B")
  ;;       (cursor-insert cursor))

  ;;     (let ((cursor (cursor-open session "index:terms:reversed")))
  ;;       (with-cnx cnx
  ;;         (let loop ((next? (cursor-next cursor))
  ;;                    (out '()))
  ;;           (if next?
  ;;               (let ((key (cursor-key-ref cursor)))
  ;;                 (loop (catch 'wiredtiger
  ;;                         (lambda () (cursor-next cursor) #true)
  ;;                         (lambda ignore #false))
  ;;                       (cons key out)))
  ;;               out)))))
  ;;   '(("B") ("b") ("A") ("a")))
  ;; (test-check "create table with collator"
  ;;   (let* ((cnx (connection-open "/tmp/wt" "create"))
  ;;          (session (session-open cnx)))
  ;;     (connection-add-collator cnx "ci" "Sr" (lambda (key other)
  ;;                                              (if (string=? (car key) (car other))
  ;;                                                  0
  ;;                                                  (if (string-ci<? (car key) (car other)) -1 1))))
  ;;     (session-create session "table:terms" "key_format=r,value_format=S,columns=(a,b)")
  ;;     (session-create session "index:terms:reversed" "columns=(b),collator=ci")

  ;;     (let ((cursor (cursor-open session "table:terms" "append")))
  ;;       (cursor-value-set cursor "a")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "A")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "b")
  ;;       (cursor-insert cursor)
  ;;       (cursor-value-set cursor "B")
  ;;       (cursor-insert cursor))
  ;;     (let ((cursor (cursor-open session "index:terms:reversed")))
  ;;       (with-cnx cnx
  ;;         (let loop ((next? (cursor-next cursor))
  ;;                    (out '()))
  ;;           (if next?
  ;;               (let ((key (cursor-key-ref cursor)))
  ;;                 (loop (catch 'wiredtiger
  ;;                         (lambda () (cursor-next cursor) #true)
  ;;                         (lambda ignore #false))
  ;;                       (cons key out)))
  ;;               out)))))
  ;;   '(("B") ("b") ("A") ("a")))
  )
