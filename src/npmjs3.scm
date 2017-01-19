(define-module (npmjs3)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-8) ;; receive
  #:use-module (srfi srfi-1) ;; lists
  #:use-module (srfi srfi-26);; cut  
  #:use-module (grf3)
  #:use-module (wiredtigerz)
  #:use-module (ukv)  
  ;#:export (npm-package jquery)
  )



(define (write-graph! ) 
  (let ((port (open-output-file "../grafo.dot")))
    (emit-prologue "grafo" port)
    (write-nodes! port)
    (write-edges! port)
    (emit-epilogue port)
    (close-port port)))



(define (extracted-cons-cell vertex)
  (let ((package (vertex-ref vertex 'package))
        (request (vertex-ref vertex 'request)))
    (if package
        (match package
          ((name . version)
           (if (string? version)
               (cons (string-append "p" name)  version)
               (cons (string-append "p" name) ""))))   
        (match request
          ((name . version)
           (if (string? version)
               (cons (string-append "r" name)  version)
               (cons (string-append "r" name) "")))))))



(define (write-nodes! port)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (for-each (cut write-node <> port)  (traversi->list (vertices)))
    ))

(define (write-edges! port)
  (with-env (env-open* "/home/catonano/Taranto/guix/Culturia/npmjsdata" (list *ukv*))
    (for-each (cut write-edge <> port)  (traversi->list (edges)))
    ))


(define (write-edge edge-id port)
  (let* ((record (get edge-id))
         (start (get (edge-start record)))
         (end (get (edge-end record))))
    (emit-edge
     (node-name (extracted-cons-cell start))
     (node-name (extracted-cons-cell end))
     port)))


(define (write-node node-id port)
  (let ((vertex (get node-id)))
      (emit-node (node-name (extracted-cons-cell vertex)) port)))

(define (node-name node-as-a-cons-cell)
  (match node-as-a-cons-cell
    ((name . version)
     (string-append name "~~~" version))))
  

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
