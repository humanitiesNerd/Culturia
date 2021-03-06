;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; msgpack.scm - MessagePack library for R6RS Scheme
;;;
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; 6 Sep 2013 - adjust MessagePack update proposal v5
;; 11 Aug 2015 - make it work with guile

#!r6rs
(library (msgpack)
    (export pack! pack pack-size
	    unpack get-unpack
	    ;; extension
	    define-ext-pack
	    define-ext-unpack)
    (import (for (rnrs) run expand)
	    (for (rnrs eval) expand)
	    (rnrs mutable-pairs))

  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))
  ;; constant values
  (define-syntax define-constant
    (lambda (x)
      (define (eval-expr k expr)
	(if (pair? (syntax->datum expr))
	    (let ((r (eval (syntax->datum expr) (environment '(rnrs)))))
	      (datum->syntax k r))
	    expr))
      (syntax-case x ()
	((k name expr)
	 (with-syntax ((value (eval-expr #'k #'expr)))
	   #'(define-syntax name
	       (lambda (y)
		 (syntax-case y ()
		   (var (identifier? #'var) value)))))))))

  (define-constant $2^8  (- (expt 2  8) 1))
  (define-constant $2^16 (- (expt 2 16) 1))
  (define-constant $2^32 (- (expt 2 32) 1))

  ;; Tags from spec (ordered by category)
  ;; I wish it has more organised tag but this might be a
  ;; backward compatibility so don't blame... *sigh*
  ;;
  ;;                  | first byte  | first byte 
  ;;     format name  | (in binary) | (in hex)
  ;; -----------------+-------------+--------------
  ;; positive fixint  |  0xxxxxxx   | 0x00 - 0x7f
  ;; fixmap           |  1000xxxx   | 0x80 - 0x8f
  ;; fixarray         |  1001xxxx   | 0x90 - 0x9f
  ;; fixstr           |  101xxxxx   | 0xa0 - 0xbf
  ;; negative fixint  |  111xxxxx   | 0xe0 - 0xff
  ;; 
  ;; nil              |  11000000   | 0xc0
  ;; (never used)     |  11000001   | 0xc1
  ;; false 	      |	 11000010   | 0xc2
  ;; true 	      |	 11000011   | 0xc3
  ;; 
  ;; bin 8 	      |	 11000100   | 0xc4
  ;; bin 16 	      |	 11000101   | 0xc5
  ;; bin 32 	      |	 11000110   | 0xc6
  ;; ext 8 	      |	 11000111   | 0xc7
  ;; ext 16 	      |	 11001000   | 0xc8
  ;; ext 32 	      |	 11001001   | 0xc9
  ;;
  ;; float 32 	      |	 11001010   | 0xca
  ;; float 64 	      |	 11001011   | 0xcb
  ;;
  ;; uint 8 	      |	 11001100   | 0xcc
  ;; uint 16 	      |	 11001101   | 0xcd
  ;; uint 32 	      |	 11001110   | 0xce
  ;; uint 64 	      |	 11001111   | 0xcf
  ;; int 8 	      |	 11010000   | 0xd0
  ;; int 16 	      |	 11010001   | 0xd1
  ;; int 32 	      |	 11010010   | 0xd2
  ;; int 64 	      |	 11010011   | 0xd3
  ;;
  ;; fixext 1 	      |	 11010100   | 0xd4
  ;; fixext 2 	      |	 11010101   | 0xd5
  ;; fixext 4 	      |	 11010110   | 0xd6
  ;; fixext 8 	      |	 11010111   | 0xd7
  ;; fixext 16 	      |	 11011000   | 0xd8
  ;;
  ;; str 8 	      |	 11011001   | 0xd9
  ;; str 16 	      |	 11011010   | 0xda
  ;; str 32 	      |	 11011011   | 0xdb
  ;;
  ;; array 16 	      |	 11011100   | 0xdc
  ;; array 32 	      |	 11011101   | 0xdd
  ;; map 16 	      |	 11011110   | 0xde
  ;; map 32 	      |	 11011111   | 0xdf

  (define *pack-table* '())
  ;; extended thing alist of pred type proc
  (define *ext-pack-table* '())
  (define *unpack-table* (make-eqv-hashtable))
  ;; type and proc (no tag)
  (define *ext-unpack-table* (make-eqv-hashtable))
  (define (add-pack-table! pred proc)
    ;; the last in will be used if there is the same pred
    (set! *pack-table* (cons (cons pred proc) *pack-table*)))
  (define (add-ext-pack-table! pred type proc)
    (set! *ext-pack-table* (cons (cons* pred type proc) *ext-pack-table*)))

  (define-syntax define-packer
    (syntax-rules ()
      ((_ (pred . args) body ...)
       (define-packer pred (lambda args body ...)))
      ((_ pred proc)
       (add-pack-table! pred proc))))

  (define-syntax define-unpacker
    (syntax-rules ()
      ((_ (tag . args) body ...)
       (define-unpacker tag (lambda args body ...)))
      ((_ tag proc)
       (hashtable-set! *unpack-table* tag proc))))

  (define-syntax define-ext-pack
    (syntax-rules ()
      ((_ (pred type . args) body ...)
       (define-ext-pack pred type (lambda args body ...)))
      ((_ pred type proc)
       (add-ext-pack-table! pred type proc))))

  (define-syntax define-ext-unpack
    (syntax-rules ()
      ((_ (type . args) body ...)
       (define-ext-unpack type (lambda args body ...)))
      ((_ type proc)
       (hashtable-set! *ext-unpack-table* type proc))))

  ;; map and array header writer
  (define-syntax define-header-writer
    (syntax-rules ()
      ((_ name bv offset small-tag mid-tag large-tag)
       (define-header-writer name bv offset #f small-tag mid-tag large-tag))
      ((_ name bv offset fx-tag small-tag mid-tag large-tag)
       (define-header-writer name bv offset 
	 fx-tag small-tag mid-tag large-tag 15))
      ((_ name bv offset fx-tag small-tag mid-tag large-tag fx-len)
       (define (name n)
	 (cond ((and fx-tag (<= n fx-len))
		(let ((tag (bitwise-ior fx-tag n)))
		  (when bv
		    (bytevector-u8-set! bv offset tag))
		  (+ offset 1)))
	       ((and small-tag (<= n $2^8))
		(when bv
		  (bytevector-u8-set! bv offset small-tag)
		  (bytevector-u8-set! bv (+ offset 1) n))
		(+ offset 2))
	       ((<= n $2^16)
		(when bv
		  (bytevector-u8-set! bv offset mid-tag)
		  (bytevector-u16-set! bv (+ offset 1) n (endianness big)))
		(+ offset 3))
	       ((<= n $2^32)
		(when bv
		  (bytevector-u8-set! bv offset large-tag)
		  (bytevector-u32-set! bv (+ offset 1) n (endianness big)))
		(+ offset 5)))))))

  ;; helpers
  ;; integer packer helpers
  (define (pack-uint! bv message offset)
    (cond ((<= message #xFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCC)
	     (bytevector-u8-set! bv (+ offset 1) message))
	   (+ offset 2))
	  ((<= message #xFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCD)
	     (bytevector-u16-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 3))
	  ((<= message #xFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCE)
	     (bytevector-u32-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 5))
	  ((<= message #xFFFFFFFFFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xCF)
	     (bytevector-u64-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 9))
	  (else
	   (error 'pack-uint! "given value is too big" message))))

  (define (pack-sint! bv message offset)
    (cond ((<= #x-80 message #x7F)
	   (when bv
	     (bytevector-u8-set! bv offset #xD0)
	     (bytevector-s8-set! bv (+ offset 1) message))
	   (+ offset 2))
	  ((<= #x-8000 message #x7FFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD1)
	     (bytevector-s16-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 3))
	  ((<= #x-80000000 message #x7FFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD2)
	     (bytevector-s32-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 5))
	  ((<= #x-8000000000000000 message #x7FFFFFFFFFFFFFFF)
	   (when bv
	     (bytevector-u8-set! bv offset #xD3)
	     (bytevector-s64-set! bv (+ offset 1) message (endianness big)))
	   (+ offset 9))
	  (else
	   (error 'pack-sint! "given value is out of range" message))))

  (define *fixexts* '((1 . #xD4) (2 . #xD5) (4 . #xD6) (8 . #xD7) (16 . #xD8)))
  (define (pack!* bv message offset)
    (define (fixedext type m size offset)
      (when bv
	(bytevector-u8-set! bv offset type)
	(bytevector-copy! m 0 bv (+ offset 1) size))
      (+ offset 1 size))
    (define (emitext type m size offset)
      (let ((offset (cond ((<= size $2^8)
			   (when bv (bytevector-u8-set! bv offset size))
			   (+ offset 1))
			  ((<= size $2^16)
			   (when bv
			     (bytevector-u16-set! bv offset size
						  (endianness big)))
			   (+ offset 2))
			  (else
			   (when bv
			     (bytevector-u32-set! bv offset size
						  (endianness big)))
			   (+ offset 4)))))
	(fixedext type m size offset)))
    (define (check-extension)
      (let* ((slot (do ((slot *ext-pack-table* (cdr slot)))
		       ((or (null? slot) ((caar slot) message))
			(if (null? slot)
			    (error 'pack "the message is not supported" message)
			    (car slot)))))
	     (type (cadr slot))
	     (bv2 ((cddr slot) message))
	     (size (bytevector-length bv2)))
	(let-values (((tag fixed?)
		      ;; choose the smallest one
		      (cond ((assv size *fixexts*)
			     => (lambda (s) (values (cdr s) #t)))
			    ((<= size $2^8 ) (values #xC7 #f))
			    ((<= size $2^16) (values #xC8 #f))
			    ((<= size $2^32) (values #xC9 #f))
			    (else
			     (error 'pack "ext size is too big" message)))))
	  (when bv (bytevector-u8-set! bv offset tag))
	  (if fixed?
	      (fixedext type bv2 size (+ offset 1))
	      (emitext type bv2 size (+ offset 1))))))
    (do ((slot *pack-table* (cdr slot)))
	((or (null? slot) ((caar slot) message))
	 (if (null? slot)
	     (check-extension)
	     ((cdar slot) bv message offset)))))

  (define pack! 
    (case-lambda
     ((bv message) (pack! bv message 0))
     ((bv message offset) (pack!* bv message offset))))

  (define (pack-size message) (pack!* #f message 0))

  (define (pack message)
    (let ((bv (make-bytevector (pack-size message))))
      (pack! bv message 0)
      bv))

  ;; unpack
  ;; len   = fix length or #f
  ;; bytes = leading length bytes (1, 2, 4 or 8) or #f
  (define (bytevector-u16b-ref bv index)
    (bytevector-u16-ref bv index (endianness big)))
  (define (bytevector-u32b-ref bv index)
    (bytevector-u32-ref bv index (endianness big)))
  (define (bytevector-u64b-ref bv index)
    (bytevector-u64-ref bv index (endianness big)))
  (define (bytevector-s16b-ref bv index)
    (bytevector-s16-ref bv index (endianness big)))
  (define (bytevector-s32b-ref bv index)
    (bytevector-s32-ref bv index (endianness big)))
  (define (bytevector-s64b-ref bv index)
    (bytevector-s64-ref bv index (endianness big)))

  (define (get-length in bytes)
    (if (= bytes 1)
	(get-u8 in)
	(let ((bv (get-bytevector-n in bytes)))
	  (case bytes
	    ((2) (bytevector-u16b-ref bv 0))
	    ((4) (bytevector-u32b-ref bv 0))
	    ((8) (bytevector-u64b-ref bv 0))
	    (else (error 'get-length "[internal] must be a bug!" bytes))))))

  (define (get-data in len bytes)
    (if len
	(get-bytevector-n in len)
	(let ((n (get-length in bytes)))
	  (get-bytevector-n in n))))

  (define (unpack-map in len bytes) 
    (let ((count (or len (get-length in bytes))))
      (let loop ((i 0) (r '()))
	(if (= i count)
	    (reverse r)
	    (let* ((k (unpack* in))
		   (v (unpack* in)))
	      (loop (+ i 1) (cons (cons k v) r)))))))

  (define (unpack-array in len bytes)
    (let* ((count (or len (get-length in bytes)))
	   (v (make-vector count)))
      (let loop ((i 0))
	(if (= i count)
	    v
	    (let ((o (unpack* in)))
	      (vector-set! v i o)
	      (loop (+ i 1)))))))

  (define (unpack* in)
    (define (fixed-collection in type)
      (cond ((= (bitwise-and type #xA0) #xA0) ;; fixstr
	     (let ((bv (get-data in (bitwise-and type #x1F) #f)))
	       (utf8->string bv)))
	    ((= (bitwise-and type #x90) #x90) ;; fixarray
	     (unpack-array in (bitwise-and type #x0F) #f))
	    ((= (bitwise-and type #x80) #x80) ;; fixmap
	     (unpack-map in (bitwise-and type #x0F) #f))
	    (else ;; something we don't know
	     (error 'unpack "unknown tag appeared" type))))
    (let ((type (get-u8 in)))
      ;; handle special case first
      (cond ((eof-object? type) type)
	    ((zero? (bitwise-and type #x80)) type)
	    ((= (bitwise-and type #xE0) #xE0) ;; negative fixint
	     (- (bitwise-and type #x1F) 32))
	    ((= (bitwise-and type #xC0) #x80) ;; fixed collection
	     (fixed-collection in type))
	    (else
	     ;; dispatch
	     (let ((proc (hashtable-ref *unpack-table* type #f)))
	       (if proc
		   (proc in)
		   ;; TODO ext
		   (error 'unpack "not supported" type)))))))

  (define (handle-ext-unpack bv)
    (let ((type (bytevector-u8-ref bv 0)))
      (cond ((hashtable-ref *ext-unpack-table* type #f)
	     => (lambda (proc)
		  (let* ((size (- (bytevector-length bv) 1))
			 (data (make-bytevector size)))
		    (bytevector-copy! bv 1 data 0 size)
		    (proc data))))
	    (else (error 'ext-unpack "handler is not registered")))))

  (define get-unpack unpack*)

  (define unpack
    (case-lambda
     ((bv) (unpack bv 0))
     ((bv offset)
      (let ((in (open-bytevector-input-port bv)))
	(set-port-position! in offset)
	(unpack* in)))))

  ;; packers R6RS doesn't allow to put this in between so the bottom
  (define-packer (null? bv message offset)
    (when bv (bytevector-u8-set! bv offset #xC0))
    (+ offset 1))

  (define-packer (boolean? bv message offset)
    (when bv (bytevector-u8-set! bv offset (if message #xC3 #xC2)))
    (+ offset 1))

  ;; bin 8 16 32
  ;; these doesn't have fx*** format
  (define-packer (bytevector? bv message offset)
    (define-header-writer write-header! bv offset #xC4 #xC5 #xC6)
    (let* ((len (bytevector-length message))
	   (new-offset (write-header! len)))
      (when bv
	(bytevector-copy! message 0 bv new-offset len))
      (+ new-offset len)))
  ;; ext must be done by user (user extension right?)
  
  ;; float 32/ float 64 (we don't support float 32 though)
  (define-packer (flonum? bv message offset)
    ;; we use only double, so floating points are always big...
    ;; FIXME Is there a way to determine if the message is float or double?
    (when bv
      (bytevector-u8-set! bv offset #xCB)
      (bytevector-ieee-double-set! bv (+ offset 1) message (endianness big)))
    (+ offset 9))

  (define-packer (integer? bv message offset)
    (cond ((<= 0 message 127)
	   (when bv
	     (bytevector-u8-set! bv offset message))
	   (+ offset 1))
	  ((<= -32 message -1)
	   (when bv
	     (bytevector-u8-set! bv offset 
				 (bitwise-ior #b11100000 (abs message))))
	   (+ offset 1))
	  ((positive? message) (pack-uint! bv message offset))
	  ((negative? message) (pack-sint! bv message offset))
	  (else (error 'pack-integer! "should not be here!" message))))

  (define-packer (string? bv message offset)
    (define-header-writer write-header! bv offset #b10100000 #xD9 #xDA #xDB 31)
    (let* ((message (string->utf8 message))
	   (msg-len (bytevector-length message))
	   (new-offset (write-header! msg-len)))
      (when bv
	(bytevector-copy! message 0 bv new-offset msg-len))
      (+ new-offset msg-len)))


  (define-packer (pair? bv message offset)
    (define-header-writer write-header! bv offset #b10000000 #f #xDE #xDF)
    (let ((new-offset (write-header! (length message))))
      (let loop ((message message)
		 (offset new-offset))
	(cond ((null? message) offset)
	      ((pair? (car message))
	       ;; should we reject if the key is pair/vector?
	       (let* ((key-off (pack!* bv (caar message) offset))
		      (value-off (pack!* bv (cdar message) key-off)))
		 (loop (cdr message) value-off)))
	      (else
	       (error 'pack-map! "alist is required" message))))))

  (define-packer (vector? bv message offset)
    (define-header-writer write-header! bv offset #b10010000 #f #xDC #xDD)
    (let* ((msg-len (vector-length message))
	   (new-offset (write-header! msg-len)))
      (let loop ((i 0) (offset new-offset))
	(if (= i msg-len)
	    offset
	    (loop (+ i 1) (pack!* bv (vector-ref message i) offset))))))

  ;; unpackers
  ;; special case fixed values are treated in unpack*

  ;; one byte values
  (define-unpacker (#xC0 in) '())
  (define-unpacker (#xC2 in) #f)
  (define-unpacker (#xC3 in) #t)

  ;; bin 8 - 32
  (define-unpacker (#xC4 in) (get-data in #f 1))
  (define-unpacker (#xC5 in) (get-data in #f 2))
  (define-unpacker (#xC6 in) (get-data in #f 4))

  ;; ext 8 - 32
  (define-unpacker (#xC7 in) 
    (let* ((n (get-length in 1))
	   (bv (get-data in (+ n 1) #f)))
      (handle-ext-unpack bv)))
  (define-unpacker (#xC8 in)
    (let* ((n (get-length in 2))
	   (bv (get-data in (+ n 1) #f)))
      (handle-ext-unpack bv)))
  (define-unpacker (#xC9 in)
    (let* ((n (get-length in 2))
	   (bv (get-data in (+ n 1) #f)))
      (handle-ext-unpack bv)))

  ;; float 32 and 64
  (define-unpacker (#xCA in) 
    (bytevector-ieee-single-ref (get-data in 4 #f) 0 (endianness big)))
  (define-unpacker (#xCB in) 
    (bytevector-ieee-double-ref (get-data in 8 #f) 0 (endianness big)))

  ;; uint8 - 64
  (define-unpacker (#xCC in) (bytevector-u8-ref (get-data in 1 #f) 0))
  (define-unpacker (#xCD in) (bytevector-u16b-ref (get-data in 2 #f) 0))
  (define-unpacker (#xCE in) (bytevector-u32b-ref (get-data in 4 #f) 0))
  (define-unpacker (#xCF in) (bytevector-u64b-ref (get-data in 8 #f) 0))

  ;; int8 - 64
  (define-unpacker (#xD0 in) (bytevector-s8-ref (get-data in 1 #f) 0))
  (define-unpacker (#xD1 in) (bytevector-s16b-ref (get-data in 2 #f) 0))
  (define-unpacker (#xD2 in) (bytevector-s32b-ref (get-data in 4 #f) 0))
  (define-unpacker (#xD3 in) (bytevector-s64b-ref (get-data in 8 #f) 0))

  ;; TODO fixext 1 - 16
  (define-unpacker (#xD4 in) (handle-ext-unpack (get-data in 2 	#f)))
  (define-unpacker (#xD5 in) (handle-ext-unpack (get-data in 3 	#f)))
  (define-unpacker (#xD6 in) (handle-ext-unpack (get-data in 5 	#f)))
  (define-unpacker (#xD7 in) (handle-ext-unpack (get-data in 9 	#f)))
  (define-unpacker (#xD8 in) (handle-ext-unpack (get-data in 17 #f)))

  ;; str 8 - 32
  (define-unpacker (#xD9 in) (utf8->string (get-data in #f 1)))
  (define-unpacker (#xDA in) (utf8->string (get-data in #f 2)))
  (define-unpacker (#xDB in) (utf8->string (get-data in #f 4)))

  ;; array 16 32
  (define-unpacker (#xDC in) (unpack-array in #f 2))
  (define-unpacker (#xDD in) (unpack-array in #f 4))

  ;; map 16 32
  (define-unpacker (#xDE in) (unpack-map in #f 2))
  (define-unpacker (#xDF in) (unpack-map in #f 4))

)
