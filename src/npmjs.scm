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
  )



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


(define (package package-as-a-vertex) 
  (let ((package-cell (vertex-ref package-as-a-vertex 'package)))
    (match package-cell
      ((name . version)
       (downloaded-package name version)))))

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
  
(define (populate-store! packages-alist)

;  (define (starting-package package-cell)
;     (receive (new package-vertex)
         ;get-or-create
 ;      ) ;;TODO finire questa riga
 ;   package-vertex)

  (define (update-store! head deps) 
    (define (stuff-dep-in-store! head dep)
      (let ((dep-package (create-vertex '((package dep)))))
        (create-edge head dep-package '((label . depends-on)))))  
    (for-each stuff-dep-in-store! deps))

  (let loop ((current-level  (map starting-package packages))
             (next-level     '()))
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
       (let ((p (package head)))
         (if (sound? p)
             (let* ((deps (children p)))

               
               (update-store! head deps)
               (loop tail (append deps next-level) ))         
             (loop tail next-level ))
         )
       ))))

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
