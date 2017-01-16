(define-module (npmjs)
  #:use-module (json)
  #:use-module (http)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (grf3)
  #:use-module (wiredtigerz)
  #:use-module (ukv)
  #:use-module (srfi srfi-9)  ;; records
  #:use-module (srfi srfi-9 gnu);; set-record-type-printer! and set-field.   
  ;#:use-module (guix records)  
  ;#:use-module (srfi srfi-9 gnu)
  #:export (npm-package jquery)
  )



;(define* (dynamic-link* #:optional library-name)
;  (let ((shared-object (if library-name (dynamic-link library-name)
;                           (dynamic-link))))
;    (lambda (return-value function-name . arguments)
;      (let ((function (dynamic-func function-name shared-object)))
;        (pointer->procedure return-value function arguments)))))


(define (json-fetch-old url)
  "Return a alist representation of the JSON resource URL, or #f on failure."
  (receive (response body)
      (http-get url)
    (read-json (open-input-string body))))


(define (json-fetch url)
  "Return a alist representation of the JSON resource URL, or #f on failure."
  (catch #t
    (lambda ()
      (receive (response body)
          (http-get url)
        (read-json (open-input-string body))))
    (lambda (key . parameters)
      'nope)))


(define *REGISTRY*  "https://registry.npmjs.org/")

;(define registry (open-input-file "./min.registry.json"))

;(define table (json->scm registry))

;(define env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*)))

(define (downloaded-package package-name version)
  (let ((package (json-fetch (string-append *REGISTRY*
                                            (encode-and-join-uri-path
                                             (list package-name  version))))))
    package))


(define (npm-package package)
  (match package
    ((name . version)
     (downloaded-package name version))))


(define (extracted-deps key package)
  (let ((step1 (assoc-ref package key)))
    (if step1
        (cdr step1)
        '())))

(define (children package)
  (let ((soundness-check (sound? package)))
    (if (not (symbol? soundness-check))
        (let ((devDepsValue (extracted-deps "devDependencies" package))
              (depsValue (extracted-deps "dependencies" package)))
          (append depsValue devDepsValue))
        package)))

(define (extracted-version downloaded-package)
    (assoc-ref downloaded-package "version"))

(define (insert-new-package! request-vertex)
  (display "downloading ")
  (display request-vertex)
  (newline)
  (let* ((request-as-a-cons-cell (vertex-ref request-vertex 'request)) 
         (p (npm-package request-as-a-cons-cell))
         (name (car request-as-a-cons-cell)) 
         (downloaded-package-soundness-check-result (sound? p)))
    (if (symbol? downloaded-package-soundness-check-result)
        (create-vertex `((package . ,request-as-a-cons-cell)
                         (dependencies-already-processed? . #t)
                         (broken-package . ,downloaded-package-soundness-check-result)))
        (let ((actual-version (extracted-version p)))
          ;; now, it's possible that actual-version is #f
          ;; in fact, declared dependencies like ("pakage-name" . "") DO exist, in npm.
          ;; In that case the request to the registry resolves to the general package, not the single version,
          ;; so the extraction of the actual-version results in #f.
          ;; In that case we store the result of such a request in the db with the cell ('general-package? . #t)
          ;; And this accounts for the SECOND type of package vertex in our graph
          ;; see line no 371 in this file
          (receive (new package)
              (get-or-create-vertex 'package (cons name actual-version))
            (if new
                (if actual-version
                    (let ((deps (children p))) 
                      (save
                       (vertex-set
                        (vertex-set package 'dependencies-already-processed? #f)
                        'declared-deps deps)))
                    (save
                     (vertex-set
                      (vertex-set package 'dependencies-already-processed? #t)
                      'general-package? #t)))
                package))))))




(define (requested-package request)
  (display "found package ")
  (display request)
  (display "\n")
  (get (end (first (outgoings (vertex-uid request))))))
 

(define (seen? package-as-vertex)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((refreshed-vertex (get (vertex-uid package-as-vertex))))
      (vertex-ref refreshed-vertex 'dependencies-already-processed?)))
  )


(define (processed-dep! head dep-request-as-cons-cell)
  (receive (new request-vertex)
      (get-or-create-vertex 'request dep-request-as-cons-cell)
    (if new
        ;; TODO there are 10.455 request nodes with NO incoming edges and NO outgoing edges.
        ;; on a total on 15.528 nodes.
        ;; what the hell is going on here ?
        (let ((package-vertex (insert-new-package! request-vertex)))
          (create-edge head package-vertex '((label . depends-on) ))
          (create-edge head request-vertex '((label . requests)))
          (create-edge request-vertex package-vertex '((label . yelds)))
          package-vertex)
        (let ((package-vertex (requested-package request-vertex)))
          (create-edge head package-vertex '((label . depends-on) ))
          (create-edge head request-vertex '((label . requests)))
          package-vertex))))


(define (insert-deps! head deps)
  "HEAD is supposed to be a vertex and DEPS is supposed to be an alist with cells like (\"package-name\" . \"package-version\" ).
  Returns a list of vertices (turns the alist into a list of vertices, storing them in the db in the process)"
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((processed-deps (map (lambda (dep)
                                 (processed-dep! head dep))
                               deps)))  
      (save (vertex-set head 'dependencies-already-processed? #t))
      processed-deps)))

(define (populate-store! package max-level)
  (let loop ((current-level  package)
             (next-level     '())
             (level            1)
             )
   
      (if (<= level max-level)
          (match current-level
            (() 
             (match next-level
               (() ;; we have finished !
                ;; This is what this monstre function is supposed to return
                level  
                )
               ((head . tail)
                ;; we move to the next level
                (loop next-level '() (+ level 1)))))
        
            ((head . tail)
             (display (vertex-ref head 'package))
             (display "\n")
        
             (if (seen? head)
                 (loop tail next-level level)
                 (loop tail (append next-level
                                    (insert-deps! head (vertex-ref head 'declared-deps))) level)))))))

(define (sound? package)
  "If PACKAGE is sound, return #t
   if PACKAGE is not sond, return a symbol
   Probably it could be one symbol only, there's no point in 
   differentiating among different error conditions. Not sure about this. We'll see"
  (match package ((@)                           ;the package does not exists
                  'the-package-does-not-exist)
                  ((@ ("error" . error-message)) ;the version does not exists
                   'the-version-does-not-exist)
                  ((@ ("code" . error-message ) (a . b))
                   'GET-is-not-allowed)
                  ('nope                      ;http-get raised an exception. God knows why this happened :-/
                   'nope)
                  (_ #t)))




(define (root-for-populating package)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new req)
        (get-or-create-vertex 'request package)
      (insert-new-package! req)))
  )



(define (bridgehead list-of-packages levels)
  ;(with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (populate-store! (map root-for-populating list-of-packages) levels)
    ;)
  )

;(bridgehead (unexplored-vertices) 1)
;(bridgehead (list '("typescript" . "2.1.4")) 1)









(define (unexplored-vertices)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list  
     (traversi-map
      (lambda (id) (vertex-ref (get id) 'package)) 
      (traversi-filter 
       (lambda (vertex-id)
         (let ((vertex (get vertex-id)))
           (and
            (not (vertex-ref vertex 'dependencies-already-processed?))
            (not (vertex-ref vertex 'general-package?))
            (vertex-ref vertex 'package))))
       (vertices)))
     )))


(define (starting-point? vertex-id)
  (let ((vertex (get vertex-id)))
    (and
     (not (vertex-ref vertex 'dependencies-already-processed?))
     (not (vertex-ref vertex 'general-package))
     (vertex-ref vertex 'package))))

(define (general-package? vertex-id)
  (let ((vertex (get vertex-id)))
    (vertex-ref vertex 'general-package?)
    ))
























;; I discovered that lots of request vertices get stored as isolated, that is
;; with no incomiing and outgoing edges.


;; to reproduce the current problem
;; starting wit han empty datastore, run in order:

;; (bridgehead '(("jquery" . "3.1.1")) 1)
;; (select-vertices request? no-incomings?)
;; (select-vertices request? no-outgoings?)
;; (select-vertices package? no-incomings?)
;; (select-vertices package? no-outgoings?)
;; (select-vertices package? completely-isolated?)
;; (select-vertices package? any-outgoing?)
;; (select-vertices package? any-incoming?)
;; (select-edges no-start?)
;; (select-edges no-end?)

;; Ok: there are NO edges with no start or no end.
;; BUT there are NO packages with incoming or outgoing edges

;; This is not possible.

;; Are my queries wrong ?

;; I used the functions below to assess the problem.

;; I couldn't come up with a reason why this happens until now.
;; it seems like create-edge doesn't work :-/


(define (extracted-cons-cell vertex)
  (let ((package (vertex-ref vertex 'package))
        (request (vertex-ref vertex 'request)))
    (if package
        (match package
          ((name . version)
           (if (string? version)
               package
               (cons name ""))))   
        (match request
          ((name . version)
           (if (string? version)
               request
               (cons name "")))))))


(define (select-vertices is-what? proc)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-map (lambda (id) (extracted-cons-cell (get id)))
                   (traversi-filter
                    proc
                    (traversi-filter
                     is-what?
                     (vertices)))))))


(define (select-edges proc)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-filter
      proc
      (edges)))))

(define (no-start? edge-id)
  (let ((record (get edge-id)))
    (not (edge-start record))))

(define (no-end? edge-id)
  (let ((record (get edge-id)))
    (not (edge-end record))))


(define (request? vertex-id)
  (let* ((vertex (get vertex-id)))
    (vertex-ref vertex 'request)))

(define (package? vertex-id)
  (let* ((vertex (get vertex-id)))
    (vertex-ref vertex 'package)))


(define (no-outgoings? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((outs (outgoings vertex)))
      (= (length outs) 0))))

(define (any-outgoing? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((outs (outgoings vertex)))
      (> (length outs) 0))))

(define (any-incoming? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((ins (incomings vertex)))
      (> (length ins) 0))))


(define (no-incomings? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((ins (incomings vertex)))
      (= (length ins) 0))))

(define (completely-isolated? vertex-id)
  (and
   (no-incomings? vertex-id)
   (no-outgoings? vertex-id)))

(define (depends-on? vertex-id)
  "I wrote this funtion to see which package depended on (\"rc\". \"1.1.6\"), 
   whose corresponding request vertex is isolated (no incoming edges and no outgoing edges),
   when it should have had one incoming edge and one outgoing edge.
   I know that the #f and #t in the ifs look stupid."
  (let* ((vertex (get vertex-id))
         (deps (vertex-ref vertex 'declared-deps)))
    (if deps
        (if (find (lambda (x) (equal? x '("rc" . "1.1.6"))) deps)
            #t
            #f)
        #f)
    )
  )














;; I discovered that I had stored about 16 packages with no version, like ("package-name" . #f)
;; I had to explore the graph in order to assess the problem,
;; then I had to fetch them and destructively update them before continuing.
;; I used (dependants3 (dependants2)) in the REPL to update them
;; I used the other procedures below to assess the problem
;; I marked those as general packages (like (general-package? . #t))
;; as opposed to a versioned package
(define (dependants2);jison #f
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
                 (traversi-map get
                               (traversi-filter 
                                (lambda (vertex-id)
                                  (let ((vertex (get vertex-id)))
                                    (and
                                     (vertex-ref vertex 'package)
                                     (not (cdr (vertex-ref vertex 'package))))))
                                (vertices)))))
  )
  
  
  
(define (dependants3 l)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (for-each (lambda (vertex-package)
                (let* ((step1 (vertex-assoc vertex-package))
                       (step2 (assoc-set! step1 'dependencies-already-processed? #t))
                       (updated-vertex (set-field vertex-package (vertex-assoc) step2)))
                (save
                 (vertex-set updated-vertex 'general-package? #t))))
              l)
    )
  )



(define (get-package package-as-cons-cell)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new package)
        (get-or-create-vertex 'package package-as-cons-cell)
      package
      )))

(define (get-request request-as-cons-cell)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new request)
        (get-or-create-vertex 'request request-as-cons-cell)
      request
      )))
