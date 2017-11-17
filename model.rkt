#lang racket

;; Se necesita una tabla con todas las frases fijas  necesarias para el blog y su traducción a cada idioma. e.g. título, menú, fecha

(require db)
(require macro-debugger/expand)

(struct blog (db languages-list ))
;; db should be a sqlite file where the database is going to be stored.
;; languages-list is a list of available languages for the blog (The static parts of the blog)
;; default-language is the default language of the blog ( language to use when none of the languages specified by the user's browser is supported by the blog).
(struct post (id publication-date title body) #:transparent )

;; The folowing defines a static-page

;;The language and order are taken care of in the function
;;get-static-pages, so it is not necessary to include them in the
;;structure.
(struct static-page (title content) #:transparent)


(define (create-blog-databases db)
  
  ;; Create a table with id, publication_date, title and body.
  (query-exec db (string-append
		  "CREATE TABLE posts"
	       "(id INTEGER PRIMARY KEY,publication_date TEXT,title TEXT,body TEXT)"))
  
  ;; Create table with info about the blog. Title, menu names, etc.
  (query-exec db
	      (string-append
	       "CREATE TABLE static_info"
	       "(rubric TEXT,value TEXT,language TEXT)"
	       ))

  (query-exec db (string-append
		  "CREATE TABLE available_languages"
		  "(language TEXT)"
		  ))
  
  (query-exec db
	      (string-append
	       "CREATE TABLE static_pages"
	       "(position INTEGER, title TEXT,content TEXT,language TEXT)"
	       ))
  
  )

(define (row->post row-from-query)
  (post (vector-ref row-from-query 0)
	(vector-ref row-from-query 1)
	(vector-ref row-from-query 2)
	(vector-ref row-from-query 3)))


(define (add-static-page a-blog static-page-list)
  ;; The static-page-list should be a list of the form: (language title content)
  ;; The value of order will be the next integer that has not been used by any other static page

  ((languages-check-function a-blog (map
				     (lambda (x)
				       (cons (car x) (cadr x)))
				     static-page-list)
			     "static_page"))

  (unless (andmap (lambda (x) (string? (caddr x)) )
		  static-page-list)
    (raise-argument-error 'bad-contents "At least one of the contents is not a string")
    )
  
  (define max-pos  (query-maybe-value (blog-db a-blog)
				      "SELECT MAX(position) FROM static_pages")   )
  
  (define next-pos (if (sql-null? max-pos )
		       1 (+ max-pos 1)))
  
  (for-each
   (lambda (x)
     (match x
       [(list language title content )
	(query-exec (blog-db a-blog)
		    "INSERT INTO static_pages VALUES(?,?,?,?)"
		    next-pos
		    title
		    content
		    language
		    )]))
   static-page-list))






(define (get-static-pages a-blog language )
  ;; Gives back a list of the static pages for the chosen language.
  
  (define (row->static-page row )
    (static-page (vector-ref row 0)
		 (vector-ref row 1)))
  
  (unless (member language (blog-languages-list a-blog))
    (raise-argument-error 'unknown-language "Only languages defined in the blog should be used." language ))
  
  (define rows-list
    (query-rows (blog-db a-blog)
		(string-append
		 "SELECT  title,content FROM static_pages "	       
		 "WHERE language = ? "
		 "ORDER BY position")		
		language))
  (map row->static-page rows-list))

(define (get-static-page a-blog language index )

  
  (define (row->static-page row )
    (static-page (vector-ref row 0)
		 (vector-ref row 1)))
  
  (unless (member language (blog-languages-list a-blog))
    (raise-argument-error 'unknown-language "Only languages defined in the blog should be used." language ))
  
  (define rows-list
    (query-rows (blog-db a-blog)
		(string-append
		 "SELECT  title,content FROM static_pages "	       
		 "WHERE language = ? "
		 "AND position = ?")		
		language index))
  (row->static-page (car  rows-list)))



(define (blog-insert-post! a-blog title body)
  ;;Insert a new post in the blog.
  (query-exec (blog-db a-blog)	      
	      "INSERT INTO posts (publication_date,title,body) VALUES ( datetime('now','localtime'),?,?) "
	      title body))

(define (blog-insert-old-post!  a-blog post-date title body)
  ;;Insert a new post in the blog.
  (query-exec (blog-db a-blog)	      
	      "INSERT INTO posts (publication_date,title,body) VALUES (?,?,?) "
	      post-date title body))

(define (blog-erase-post! a-blog post-id)
  (query-exec (blog-db a-blog)
	      "DELETE FROM posts WHERE id=?"
	      post-id)
  )



(define (get-blog-title! a-blog language)
  (query-maybe-value (blog-db a-blog)
		     (string-append
		      (format "SELECT value FROM static_info WHERE rubric = 'title' AND language = \"~a\"  " language)))
  )

(define (get-blog-go-back-string! a-blog language)
  (query-maybe-value (blog-db a-blog)
		     (string-append
		      (format "SELECT value FROM static_info WHERE rubric = 'go-back-message' AND language = \"~a\"  " language)))
  )


;; Everything that is inserted in the statics_info needs to check that
;; ther languages passed are correct. The following function helps
;; with it.

(define (languages-check-function a-blog values-list rubric-name )
  ;; Returns a useful function for checking attributes with a list of languages
  (lambda ()
    (let ([blog-languages (blog-languages-list a-blog ) ])
      (cond
       [ (not (andmap pair? values-list ))
	 (raise-argument-error 'bad-argument "pair?" values-list  )  ]
       [ (not (andmap (lambda (x)
			(string? (cdr x) ))
		      values-list  ) )
	 (raise-argument-error 'bad-pair (format "Each element of ~as-list should have the form ('language' . '~a')" rubric-name rubric-name ) values-list )]
       [ (not (andmap (lambda (x) (member (car x) blog-languages  )) values-list  ))
	 (raise-argument-error 'unknown-language "Only languages defined in the blog should be used." values-list )]
       [ (not (= (length values-list) (length blog-languages ) ))
	 (raise-argument-error 'wrong-number-of-languages (format "There should be one ~a for each language defined in the blog" rubric-name) values-list)]
       [else #t ])))
  
  )

(define-syntax (define-set-function-with-languages stx)
  ;;; This macro creates a function that inserts information in the
  ;;; static_info table.  The macro was created since there is a lot
  ;;; of common code in the contents of the functions
  (syntax-case stx ()
    [(_ function-name rubric-string)
     #'(define (function-name a-blog things-list)
	 (define check-aux-function (languages-check-function a-blog things-list rubric-string))
	 (check-aux-function)
	 (query-exec (blog-db a-blog)
		     "DELETE FROM static_info WHERE rubric = ?" rubric-string)

	 (for-each
	  (lambda (x)
	    (let ([lang (car x)]
		  [thing (cdr x)])
	      (query-exec (blog-db a-blog) "INSERT INTO static_info VALUES(?,?,?)" rubric-string thing lang)))
	  
	  things-list))] ))

;; To check the expansion: (syntax->datum (expand-only #'(define-set-function-with-languages set-blog-title!  "title") (list #'define-set-function-with-languages)))

(define-set-function-with-languages set-blog-title!  "title")
(define-set-function-with-languages set-go-back-messages! "go-back-message" )



(define (get-last-posts! a-blog [n 5])  
  (query-rows (blog-db a-blog)	      
	      "SELECT * FROM posts ORDER BY id DESC LIMIT ?"
	      (number->string n) )

  )


;; returns a line with the
(define (get-post! a-blog id)
  (define id-row
    (query-maybe-row (blog-db a-blog)
		     "SELECT * FROM posts WHERE id = ?"
		     id))
  (if id-row (row->post id-row) #f))

(define (get!-number-of-posts blog)
  (query-value "SELECT COUNT(*) FROM"  )
  )





(define (initialize-blog! home [languages-list empty] )  
  (define db
    (virtual-connection
     (connection-pool
      (lambda ()
	(sqlite3-connect #:database home #:mode 'create)))) )
    
  (if (and (table-exists? db "available_languages")
	   (not  (null? (query-list db "SELECT * FROM available_languages" ))))
      (set! languages-list (query-list db "SELECT language FROM available_languages"))
      (begin
	(if (null? languages-list)
	    (raise-argument-error 'empty-lang-list "languages-list should not be empty when starting the blog database" languages-list )
	    (begin
	      (create-blog-databases db)
	      (for-each (lambda (lang)
			  (query-exec db "INSERT INTO available_languages  VALUES (?)" lang ))
			languages-list)))))
    
  (blog db languages-list) )





(provide (all-defined-out))
