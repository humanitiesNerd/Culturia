(define-module (npmjs2)
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
  #:use-module (sxml simple)
  ;#:use-module (guix records)  
  ;#:use-module (srfi srfi-9 gnu)
  #:export (npm-package jquery)
  )

;(define-module (test_module)
;    #: export (square
;               cube))


(define (root-for-graphing package)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (receive (new req)
        (get-or-create-vertex 'package package)
      req))
  )


(define (write-graph! package level) 
  (let ((root-package (root-for-graphing package)))
      (export-graph! root-package level))
    
    )



(define (export-graph! package max-level)

  (define (seen? store thing)
    (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
      (vhash-assoc (vertex-ref thing 'package) store)))
  
  (define (seen store thing)
    (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
      (vhash-cons (vertex-ref thing 'package) #t store)))

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
       (string-append name "~~~" version))
      (single-thing
       single-thing)))
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
