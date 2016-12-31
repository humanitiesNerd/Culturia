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
  (let* ((request-as-a-cons-cell (vertex-ref request-vertex 'request)) 
         (p (npm-package request-as-a-cons-cell))
         (name (car request-as-a-cons-cell)) 
         (downloaded-package-soundness-check-result (sound? p)))
    (if (symbol? downloaded-package-soundness-check-result)
        (create-vertex `((package . package-request)
                         (dependencies-already-processed? . #t)
                         (broken-package . package-handle)))
        (let ((actual-version (extracted-version p))
              (deps (children p)))
          (create-vertex `((package . ,(cons name  actual-version))
                           (dependencies-already-processed? . #f)
                           (declared-deps . ,deps)))))))



(define (processed-dep! head dep-request-as-cons-cell)
  ;(with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new request-vertex)
        (get-or-create-vertex 'request dep-request-as-cons-cell)
      (if new
          (let ((package-vertex (insert-new-package! request-vertex)))
            (create-edge head package-vertex '((label . depends-on) ))
            (create-edge head request-vertex '((label . requests)))
            (create-edge request-vertex package-vertex '((label . yelds)))
            package-vertex)
          (let ((package-vertex (requested-package request-vertex)))
            (create-edge head package-vertex '((label . depends-on) ))
            (create-edge head request-vertex '((label . requests)))
            (create-edge request-vertex package-vertex '((label . yelds)))
            package-vertex)))
    ;)
  )


(define (requested-package request)
  (display "found package ")
  (display request)
  (display "\n")
  (get (end (first (outgoings (vertex-uid request))))))
 
(define (dependencies node)
  (map get (map end (outgoings (vertex-uid node)))))


(define (seen? package-as-vertex)
  (vertex-ref package-as-vertex 'dependencies-already-processed?))


(define (insert-deps! head deps)
  "HEAD is supposed to be a vertex and deps is supposed to be an alist.
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
  (match package ((@)                           ;the package does not exists
                  'the-package-does-not-exist)
                  ((@ ("error" . error-message)) ;the version does not exists
                   'the-version-does-not-exist)
                  ((@ ("code" . error-message ) (a . b))
                   'GET-is-not-allowed)
                  (_ #t)))




(define (root-for-populating package)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new req)
        (get-or-create-vertex 'request package)
      (insert-new-package! req)))
  )

;     '("shared-karma-files" . "git://github.com/karma-runner/shared-karma-files.git#82ae8d02"))))


(define (root-for-graphing package)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new req)
        (get-or-create-vertex 'package package)
      req))
  )

(define (bridgehead list-of-packages levels)
  ;(with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (populate-store! (map root-for-populating list-of-packages) levels)
    ;)
  )




(define (write-graph! package level) 
  (let ((root-package (root-for-graphing package)))
    (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
      (export-graph! root-package level))
    
    )
  )


(define (export-graph! package max-level)

  (define (seen? store thing)
    (vhash-assq thing store))
  
  (define (seen store thing)
    (vhash-consq thing #t store))

  (let ((port (open-output-file "../grafo.dot")))
    (emit-prologue "name" port)
    
  (let loop ((current-level  (list package))
             (next-level     '())
             (store          vlist-null)
             (level          1))
    
      (match current-level
        (() 
         (match next-level
           (() ;; we have finished !
            ;; This is what this monstre function is supposed to return
            (emit-epilogue port)
              
            )
           ((head . tail)
            ;; we move to the next level
            (if (< level max-level)
                (loop next-level '() store (+ level 1))
                (emit-epilogue port)
                ))))
        
        ((head . tail)
         
         (if (seen? store head)
             (loop tail next-level store level)
             (let ((deps (dependencies head))
                   (id (node-name head)))
               
               (emit-node id port)
               (for-each (lambda (dependency)
                           (emit-edge id (node-name dependency) port))
                         deps)           
               (loop tail (append next-level deps) (seen store head) level)))
         
         )))
    (close-port port)))


(define (dependencies node)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (map get (map end (filter (where? 'label 'depends-on) (outgoings (vertex-uid node)))))))


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

(define %colors
  ;; See colortbl.h in Graphviz.
  #("red" "magenta" "blue" "cyan3" "darkseagreen"
    "peachpuff4" "darkviolet" "dimgrey" "darkgoldenrod"))

(define (pop-color hint)
  "Return a Graphviz color based on HINT, an arbitrary object."
  (let ((index (hash hint (vector-length %colors))))
    (vector-ref %colors index)))
