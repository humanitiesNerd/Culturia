(use-modules (srfi srfi-26))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 receive))
(use-modules (ice-9 match))

(use-modules (csv))

(use-modules (ukv))
(use-modules (grf3))
(use-modules (wiredtiger))
(use-modules (wiredtigerz))


(define csv (make-csv-reader #\,))


(define (csv->rows input)
  (traversi-map vector->list (list->traversi (call-with-input-file input csv))))

(define (parse-movie csv)
  (match csv
    ((id title genres)
     (list (string->number id) title (string-split (string-trim-right genres) #\|)))))

(define movies (traversi-map parse-movie (traversi-cdr (csv->rows "data/movielens/ml-latest-small/movies.csv"))))

(define store-movie
  (match-lambda 
    ((id title genres) (let ((movie (create-vertex `((movie/id . ,id) (movie/title . ,title)))))
                       (let next ((genres genres))
                         (unless (null? genres)
                           (receive (new genre) (get-or-create-vertex 'genre (car genres))
                             (create-edge genre movie '((label . genre))))
                           (display ".")
                           (next (cdr genres))))))))

;; create movies with genres
(with-env (env-open* "/tmp/wt" (list *ukv*))
  (traversi-for-each store-movie movies))


(define (parse-rating rating)
  (match rating
    ((user/id movie/id rating timestamp) (list (string->number user/id)
                                         (string->number movie/id)
                                         (string->number rating)))))

(define ratings (traversi-map parse-rating (traversi-cdr (csv->rows "data/movielens/ml-latest-small/ratings.csv"))))

(define store-rating
  (match-lambda ((user/id movie/id rating)
                 (receive (_ user) (get-or-create-vertex 'user/id user/id)
                   (receive (_ movie) (get-or-create-vertex 'movie/id movie/id)
                     (create-edge user movie `((label . rating) (rating/value . ,rating)))
                     (display "."))))))

;; create user s with rating
(with-env (env-open* "/tmp/wt" (list *ukv*))
  (traversi-for-each store-rating ratings))
