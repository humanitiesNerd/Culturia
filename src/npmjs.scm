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
  #:use-module (srfi srfi-9 gnu)  ;; set-record-type-printer! and set-field. I added these for una tantum operations

  ;#:use-module (guix records)  
  ;#:use-module (srfi srfi-9 gnu)
  #:export (npm-package jquery)
  )

;(define-module (test_module)
;    #: export (square
;               cube))


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
 
;(define (dependencies node)
;  (map get (map end (outgoings (vertex-uid node)))))


(define (seen? package-as-vertex)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((refreshed-vertex (get (vertex-uid package-as-vertex))))
      (vertex-ref refreshed-vertex 'dependencies-already-processed?)))
  )

(define (processed-dep! head dep-request-as-cons-cell)
  (receive (new request-vertex)
      (get-or-create-vertex 'request dep-request-as-cons-cell)
    (if new
        ;; TODO there are 10.455 request nodes wiith NO incoming edges and NO outgoing edges.
        ;; on a total on 15.528 nodes.
        ;; what te hell is going on here ?
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
    ;(with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
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

























;; I discovered that I had stored about 16 packages with no version, like ("package-name" . #f)
;; I had to explore the graph in order to assess the problem,
;; then I had to fetch them and destructively update them before continuing.
;; I used (dependants3 (dependants2)) in the REPL to update them
;; I used the other procedures below to assess the problem

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


(define (depends-on? vertex-id)
  (let* ((vertex (get vertex-id))
         (deps (vertex-ref vertex 'declared-deps)))
    (if deps
        (if (find (lambda (x) (equal? x '("rc" . "1.1.6"))) deps)
            #t
            #f)
        #f)
    )
  )

(define (select-packages proc)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list  
     (traversi-map
      (lambda (id) (assoc-remove! (vertex-assoc (get id)) 'declared-deps)) 
      (traversi-filter 
       (lambda (vertex-id)
         (proc vertex-id))
         (vertices)))
     )))

(define (select-requests request? proc)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-map (lambda (vertex-id) (vertex-ref (get vertex-id) 'request ))
     (traversi-filter
      proc
      (traversi-filter
       request?
       (vertices)))))))

;(select-requests request? no-outgoings?)


(define (request? vertex-id)
  (let* ((vertex (get vertex-id)))
    (vertex-ref vertex 'request)))

(define (no-outgoings? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((outs (outgoings vertex)))
      (= (length outs) 0))))

(define (no-incomings? vertex-id)
  (let ((vertex (get vertex-id)))
    (let ((ins (incomings vertex)))
      (= (length ins) 0))))


(define (get-package package-as-cons-cell)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new package)
        (get-or-create-vertex 'package package-as-cons-cell)
      ;(assoc-remove! (vertex-assoc package) 'declared-deps)
      package
      )))

(define (get-request request-as-cons-cell)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new request)
        (get-or-create-vertex 'request request-as-cons-cell)
      request
      )))


(define (prov)
  (filter (lambda (x)
           (equal? x '("jison" . #f)))
          (unexplored-vertices)))



(define (dependants node);jison #f
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
          (map get (map start (car (map incomings (traversi->list
                               (traversi-filter 
                                (lambda (vertex-id)
                                  (let ((vertex (get vertex-id)))
                                    (and
                                     (not (vertex-ref vertex 'dependencies-already-processed?))
                                     (equal? '("jison" . #f) (vertex-ref vertex 'package)))))
                                (vertices)))))))))





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

