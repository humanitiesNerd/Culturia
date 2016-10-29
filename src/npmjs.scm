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


(define (downloaded-package package-name version)
  (let ((package (json-fetch (string-append *REGISTRY*  package-name "/" (encode-and-join-uri-path (list version))))))
    package))


(define (npm-package package)
  (match (assoc-ref (vertex-assoc package) 'package)
    ((name . version)
     (downloaded-package name version)))) 

(define (children package)
  (if (sound? package)
      (let ((devDeps (cdr (assoc-ref package "devDependencies")))
            (deps (cdr (assoc-ref package "dependencies"))))
        
        (append deps devDeps))       
      package))

;(define jquery (make-npm-package-version "jquery" "3.1.1"))

(define jquery
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new jquery)
        (get-or-create-vertex 'package  (cons "jquery"  "3.1.1") )
      jquery)
    ))


;(define jquery '("query" . "3.1.1"))

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
       (if (seen? head)
           (loop tail next-level)
           (let ((p (npm-package head)))
             (if (sound? p)
                 (let* ((deps (children p))
                       (deps-as-vertices (insert-deps! head deps)))
                 (loop tail (append next-level deps-as-vertices)))))
       ))
      );closes the match
    );closes the named let
)

(define (sound? package)
   (not (equal? (caadr package) "error")))



		 ;(if (sound? deps)
		 ;    (hash-fold (lambda (dep dep-version edges-store)
		 ;		  (vhash-consq
		 ;		   (string-append package-name "---" version)
		 ;		   (string-append dep "---" dep-version)
		 ;		   edges-store)
		 ;		  edges-store deps)))
		 
		 
		 
		 ;(if (sound? dev-deps);; esiste elif ?
		 ;    (hash-fold (lambda (dev-dep dev-dep-version edges-store)
		 ;		  (vhash-consq
		 ;		   ...
		 ;		   ...
		 ;		   edges-store))
		 ;		edges-store dev-deps))






;$29 = 25320
;$30 = 311894
