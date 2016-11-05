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
  ;#:use-module (guix records)  
  ;#:use-module (srfi srfi-9 gnu)
  #:export (npm-package jquery)
  )

;(define-module (test_module)
;    #: export (square
;               cube))


(define (json-fetch url)
  "Return a alist representation of the JSON resource URL, or #f on failure."
  (receive (response body)
      (http-get url)
    (read-json (open-input-string body))))


(define *REGISTRY*  "https://registry.npmjs.org/")

;(define registry (open-input-file "./min.registry.json"))

;(define table (json->scm registry))

;(define env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*)))

(define (downloaded-package package-name version)
  (let ((package (json-fetch (string-append *REGISTRY*
                                            package-name "/"
                                            (encode-and-join-uri-path
                                             (list version))))))
    package))


(define (npm-package package)
  (match (assoc-ref (vertex-assoc package) 'package)
    ((name . version)
     (downloaded-package name version))))

(define (extracted-deps key package)
  (let ((step1 (assoc-ref package key)))
    (if step1
        (cdr step1)
        '())))

(define (children package)
  (if (sound? package)
      (let ((devDepsValue (extracted-deps "devDependencies" package))
            (depsValue (extracted-deps "dependencies" package)))
        (append depsValue devDepsValue)
        )
      package))

(define (processed-package! package-as-cons-cell)
  (receive (new node)
      (get-or-create-vertex 'package package-as-cons-cell)
    (if new
        (let ((current-vertex (save (vertex-set node 'dependencies-already-processed? #f))))
          current-vertex)
        node)))



(define (seen? package-as-vertex)
  (vertex-ref package-as-vertex 'dependencies-already-processed?))

(define (processed-dep! head dep)
  (let ((node (processed-package! dep)))
    (create-edge head node '((label . depends-on)))
    node))

(define (insert-deps! head deps)
  "HEAD is supposed to be a vertex and deps is supposed to be an alist.
  Returns a list of vertices (turns the alist into a list of vertices, storing them in the db in the process)"
  (let ((processed-deps (map (lambda (dep)
                               (processed-dep! head dep))
                             deps)))  
    (save (vertex-set head 'dependencies-already-processed? #t))
    processed-deps))

(define (populate-store! package)
  (let loop ((current-level  (list package))
             (next-level     '())
             )
;    (with-env env
      (match current-level
        (() 
         (match next-level
           (() ;; we have finished !
            ;; This is what this monstre function is supposed to return
            'done  
            )
           ((head . tail)
            ;; we move to the next level
            (loop next-level '()))))
        
        ((head . tail)
         (display (vertex-ref head 'package))
         (if (seen? head)
             (loop tail next-level)
             (let ((p (npm-package head)))
               (if (sound? p)
                   (let* ((deps (children p))
                          (deps-as-vertices (insert-deps! head deps)))
                     (loop tail (append next-level deps-as-vertices)))))))
        );closes the match
 ;     ) ;closes with-env
    
    );closes the named let
  )


(define (sound? package)
  (match package ((@)                           ;the package does not exists
                  #f)
                  ((@ ("error" . error-message)) ;the version does not exists
                  #f)
                  (_ #t)))

(define (tdp)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (processed-package! '("shared-karma-files" . "git://github.com/karma-runner/shared-karma-files.git#82ae8d02"))))

(define (testa-di-ponte)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((jquery (processed-package! '("jquery" . "3.1.0"))))
      (populate-store! jquery))))


(define (write-graph!)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((jquery (processed-package! '("jquery" . "3.1.0"))))
      (export-graph! jquery))))

(define (dependencies?)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((jquery (processed-package! '("jquery" . "3.1.1"))))
      (dependencies jquery))))

(define (edges1)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-map get
                   (traversi-filter (where? 'package '("babel-preset-es2015" . "6.6.0"))
                                    (traversi-map end (edges)))))))
(define (edges2)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-map get 
                   (traversi-backtrack
                    (traversi-filter (where? 'package '("babel-preset-es2015" . "6.6.0"))
                                     (traversi-map end (edges))))))))
(define (edges3)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (traversi->list
     (traversi-map get
                   (traversi-map start 
                                 (traversi-backtrack
                                  (traversi-filter (where? 'package '("babel-preset-es2015" . "6.6.0"))
                                                   (traversi-map end (edges)))))))))
(define (jq1)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((jquery (processed-package! '("jquery" . "3.1.0"))))
      (outgoings jquery))))

(define (jq2)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (let ((jquery (processed-package! '("jquery" . "3.1.1"))))
      (outgoings jquery))))


(define* (export-graph! starting-from)
  (let ((port (open-output-file "../grafo.dot")))
    (emit-prologue "mane" port)
    (let loop ((nodes   (list starting-from))
               )
      (match nodes
        (()
         (emit-epilogue port))
        ((head . tail)
         (emit-node (node-name head) port)
         (let ((deps (dependencies head))
               (id (node-name head)))
           (for-each (lambda (dependency)
                       (emit-edge id (node-name dependency) port))
                     deps)
           (loop (append deps tail))))))
    (close-port port)))


(define (dependencies node)
  (map end (outgoings node)))

(define (node-name node-as-a-vertex)
  (let ((node-as-a-cons-cell (vertex-ref node-as-a-vertex 'package)))
    (match node-as-a-cons-cell
      ((name . version)
       (string-append name "~~~" version))))
  )

(define (emit-prologue name port)
  (format port "digraph \"Guix ~a\" {\n"
          name))
(define (emit-epilogue port)
  (display "\n}\n" port))
(define (emit-node id port)
  (format port "  \"~a\" [label = \"~a\", shape = box, fontname = Helvetica];~%"
          id id))
(define (emit-edge id1 id2 port)
  (format port "  \"~a\" -> \"~a\" [color = ~a];~%"
          id1 id2 (pop-color id1)))
