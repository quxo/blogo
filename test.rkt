#lang racket

(require "../model.rkt")
(require "../blogpage.rkt")
(require rackunit)
(require db)
(require net/url)
(require web-server/http)



(define tests
  
  (test-suite
   "Blog model tests"
   

   (check-equal?

    (begin
      (when (file-exists? "dbtests.db") (delete-file "dbtests.db" )  )
      (with-handlers ([exn:fail:contract? (lambda (e) 'language-list-empty ) ]) 
      	(initialize-blog! "dbtests.db" empty ))

      )
    'language-list-empty
    "initialize-blog!: Error if file does not exist and language list is empty ")

   (check-equal?

    (begin
      (when (file-exists? "dbtests.db") (delete-file "dbtests.db" )  )
      (initialize-blog! "dbtests.db" '("en" "es" "fr") )
      (let ([ the-blog  (initialize-blog! "dbtests.db"  )  ])
	(delete-file "dbtests.db" )
      	(blog-languages-list the-blog)
      	)
      )
    '("en" "es" "fr")
    "initialize-blog!: Error if file does not exist and language list is empty ")

   
   

   (check-equal? 
    (let ;; This should return '(#("Primer Título" "Primer cuerpo") #("Segundo Título" "Segundo cuerpo") )
	([the-blog (initialize-blog! "dbtests.db"   '("en" "es" "fr" "it" "eo") )])
      (query-exec (blog-db the-blog ) "DELETE FROM posts"   )
      (blog-insert-post! the-blog "Primer Título" "Primer cuerpo")
      (blog-insert-post! the-blog "Segundo Título"  "Segundo cuerpo")
      
      (query-rows (blog-db the-blog) "SELECT title,body FROM posts"))
    
    (list #("Primer Título" "Primer cuerpo") #("Segundo Título" "Segundo cuerpo") )
    "blog-insert-post!")

   (check-equal?   
    (let
	([the-blog (initialize-blog! "dbtests.db"   '("en" "es" "fr" "it" "eo"))]
	 [title '(("en" . "title") ("es" . "Título") ("fr" . "Titre") ("ru" . "titolo") ("eo" . "titulo") ) ])
      (with-handlers ([exn:fail:contract? (lambda (e) 'problem ) ])
	  (set-blog-title! the-blog	title)))
    
    'problem
    "Language in title does not coincide with some language od the blog.")

   (check-equal?   
    (let
	([the-blog (initialize-blog! "dbtests.db"  '("en" "es" "fr" "it" "eo"))]
	 [title '(("en"  "title") ("es" . "Título") ("fr" . "Titre") ("ru" . "titolo") ("eo" . "titulo") ) ])
      (with-handlers ([exn:fail:contract? (lambda (e) 'problem ) ])
	  (set-blog-title! the-blog	title)))
    
    'problem
    "Wrong title format: (language . title) ")

   (check-equal?   
    (let
	([the-blog (initialize-blog! "dbtests.db"   '("en" "es" "fr" "it" "eo"))]
	 [title '(("en"  "title") ("es" . "Título") ("fr" . "Titre") ("eo" . "titulo") ) ])
      (with-handlers ([exn:fail:contract? (lambda (e) 'problem ) ])
	  (set-blog-title! the-blog	title)))
    
    'problem
    "Missing language title list")

   (check-equal?

    (let
	([the-blog (initialize-blog! "dbtests.db"  '("en" "es" "fr" "it" "eo"))]
	 [title '(("en" . "title") ("es" . "Título") ("fr" . "Titre") ("it" . "titolo") ("eo" . "titulo") ) ])
      (set-blog-title!  the-blog title )
      (query-rows (blog-db the-blog) "SELECT language,value FROM static_info WHERE rubric='title' ORDER BY language"  )
      )
    '(#("en"  "title") #("eo"  "titulo") #("es"  "Título") #("fr"  "Titre") #("it"  "titolo") )
    "Checking that set-blog-title! works properly when passed the right argument")
   
   (check-equal?

    (let ([the-blog (initialize-blog! "dbtests.db" "it" )])
      ;; From the previous test, the-blog has the languages ("en" "es" "fr" "it" "eo")
      (map (lambda (x)
	     (guess-language '("en" "es" "fr" "it" "eo") "it" x ) )
	   '(("ru" "ch") ("es" "ru") ("ja" "es") ("eo"))
	   )
      )
    (list "it" "es" "es" "eo" )
    "Checking most-likely-language"
    )
         

   (check-equal?

    (let ;; This should return '(#("Primer Título" "Primer cuerpo") #("Segundo Título" "Segundo cuerpo") )
   	([the-blog (initialize-blog! "dbtests.db" "")])
      (query-exec (blog-db the-blog ) "DELETE FROM posts"   )
      (blog-insert-post! the-blog "Primer Título" "Primer cuerpo")
      (blog-insert-post! the-blog "Segundo Título"  "Segundo cuerpo")
      (define results (get-last-posts! the-blog ))
      (list (vector-take-right (car results) 2 ) (vector-take-right (cadr results) 2 )  ))
    
    (list  #("Segundo Título" "Segundo cuerpo") #("Primer Título" "Primer cuerpo") )
    
    "get-last-posts!") 

   (check-equal?
    (let ([the-blog (initialize-blog! "dbtests.db" "eo") ])
      (query-exec (blog-db the-blog ) "DELETE FROM posts"   )
      (blog-insert-post! the-blog "Primer Título" "Primer cuerpo")
      (blog-insert-post! the-blog "Segundo Título"  "Segundo cuerpo")
      (define a-post (get-post! the-blog 1  ))
      (list (post-id a-post) (post-title a-post) (post-body a-post )  ))
   
    (list 1 "Primer Título" "Primer cuerpo" )
    "row-to-post check 1" )

   ;;; The following is to test the function that changes the language of the current page

   (check-equal?
    (change-language-url
     (make-request #"GET"
		   (string->url "http://blog.com/en/id/20")
		   empty
		   (delay empty)
		   #f
		   "1.2.3.4" 80 "4.3.2.1")          
     "eo")
    "eo/id/20"
    "Language relative path")


   (check-equal?
        
    (begin
      (when (file-exists? "dbtests.db") (delete-file "dbtests.db" )  )            
      (let ([ the-blog  (initialize-blog! "dbtests.db"  '("en" "es" "fr") )  ])	
      	(add-static-page  the-blog
			 '[("en" "title" "English content")
			   ("es" "título" "Contenido en español")
			   ("fr" "titre" "Contenus en français")])
	(query-rows (blog-db the-blog)
		    "SELECT position,title,content,language FROM static_pages ORDER BY language")))
    
    '(#(1 "title" "English content" "en" )
      #(1 "título" "Contenido en español" "es" )
      #(1 "titre" "Contenus en français" "fr" ))
    
    
    )

   (check-equal?

    (begin
      (when (file-exists? "dbtests.db") (delete-file "dbtests.db" )  )            
      (let ([ the-blog  (initialize-blog! "dbtests.db"  '("en" "es" "fr") )  ])	
      	(add-static-page  the-blog
			  '[("en" "title" "English content")
			    ("es" "título" "Contenido en español")
			    ("fr" "titre" "Contenus en français")])

	(add-static-page  the-blog
			  '[("en" "About" "English content")
			    ("es" "Acerca de" "Contenido en español")
			    ("fr" "À propos" "Contenus en français")])
	(list
	 (get-static-pages the-blog "fr")
	 (get-static-pages the-blog "es")
	 (get-static-pages the-blog "en"))
	)
      
      )
    (list
     (list (static-page "titre" "Contenus en français" )
	   (static-page "À propos" "Contenus en français"))
     (list (static-page "título" "Contenido en español")
	   (static-page "Acerca de" "Contenido en español" ))
     (list (static-page "title" "English content" )
	   (static-page "About" "English content")
	   )))
   
   
   
   )    
  
  )



(require rackunit/text-ui)
;; runs the test
(run-tests tests)
