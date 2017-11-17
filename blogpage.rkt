 #lang racket

(require "model.rkt")
(require web-server/servlet)
(require web-server/formlets)
(require web-server/dispatch)
(require web-server/configuration/responders)
(require web-server/templates)

;;The following is so we can convert a string to a Sexp
(require xml)
(permissive-xexprs #t)

;; a blog page consists of a blog-page

(struct blog-page-details  (blog current-language url-path) #:mutable )
;; url-path should finish with /
;; current-language should only be the name of the language

(define (get-main-page-title! a-blog-page-details)
  (get-blog-title! (blog-page-details-blog a-blog-page-details)
		   (blog-page-details-current-language a-blog-page-details)))


(define (gen-path a-blog-page-details rest-path)
  ;;;; returns a path for the current language
  (string-append "/"
   (blog-page-details-url-path a-blog-page-details)
		 (blog-page-details-current-language a-blog-page-details)
		 "/"
		 rest-path))

(define-syntax-rule (template-response body ...)  
  (response/full
     200 #"Okay"
     (current-seconds) TEXT/HTML-MIME-TYPE
     empty
     (list (string->bytes/utf-8
	    body ...	    
	    )))  
  )



(define (get-browser-languages req)
  ;;; Esta función devuelve la lista de idiomas soportados por el navegador en orden de importancia.
  (define language-header (headers-assq* #"Accept-Language" (request-headers/raw  req)))
  (if language-header
      (let ([locales-list  (regexp-match* #px"[a-z]{2,}(-[A-Z]+)?" (bytes->string/utf-8 (header-value language-header)))  ])
	(remove-duplicates (map (lambda (x) (car (regexp-match* #px"^[a-z]{2,}" x)))
				locales-list)))      
      empty
      ))


(define (guess-language blog-languages default-language browser-languages)
  ;; All the functions will be like this since is is the easiest way to not rewrite everything
  (define (auxf browser-languages)
    (if  (null? browser-languages)
	 default-language
	 (let ([cur-lan (car browser-languages) ])
	   (if (member cur-lan blog-languages)
	       cur-lan
	       (auxf (cdr browser-languages))))))
  (auxf browser-languages) )


(define (change-language-url req lang)
  ;;; Changes the language by just changing the language part of a url
  
  ;; This function receives a request struct and returns the string of
  ;; the same url with the language changed to the parameter lang
  
  (let* ([current-url (request-uri req)  ]
	 [current-url-path-list (url-path current-url)]
	 [current-url-new-path-list (cons (path/param lang '()) (cdr current-url-path-list) )] )

    (url->string (struct-copy url current-url
			      [path current-url-new-path-list]
			      [path-absolute? #f]
			      [host #f]
			      [scheme #f]
			      ))))


(define (languages-links-section a-blog-page-details req css-class-name)
  (define a-blog (blog-page-details-blog a-blog-page-details))
  (let [(lang-list (blog-languages-list a-blog))]
    `(div ((class ,css-class-name ))
	  ,@(map (lambda (x)	   
		   `(a ((href ,(string-append "/"
					      (blog-page-details-url-path a-blog-page-details)
					      (change-language-url req x) )  )) ,(string-upcase x) ) )
	       lang-list)))  
  )


(define-syntax html-page
  ;;; Macro to create a page by just giving the body
  (syntax-rules ()
    [(html-page a-blog-page-info page-title page-body ... )
     (response/xexpr
      `(html
        (head
         (meta ((http-equiv "content-type") (content "text/html; charset=utf-8")))
	 (meta ((name "viewport" ) (content "width=device-width" ) ))
         (link ((rel "stylesheet")
                (href ,(string-append "/" (blog-page-details-url-path a-blog-page-info)  "style.css") )
                (type "text/css")))
         (title  page-title ))
        (body
	 (div ((class "top-border")))
	 (div ((class "bottom-border")))
	 (div ((class "left-border")))
	 (div ((class "right-border")))	
	 page-body ... )))]))



(define (create-post-detail-function a-blog language url-path)

  (define a-blog-page-details (blog-page-details a-blog language url-path))  
  (define go-back-string (get-blog-go-back-string! a-blog language))
						       
  (define (post-detail-page req a-post)

    (define (change-language-path lang)      
      (string-append "/"
		     (blog-page-details-url-path a-blog-page-details)
		     (change-language-url req lang) ))
  
    (template-response
     
     (let ([languages-list (blog-languages-list a-blog)]
	   [this-post-title (post-title a-post)]
	   [this-post-body (post-body a-post )]
	   [go-back-path (gen-path   a-blog-page-details "")]
    	   )
       (include-template "post_template.html")
       )))
  
  
  
  (define (show-post req id)
    (define a-post (get-post! a-blog id ))
    
    (if a-post (post-detail-page req a-post)
	(response/xexpr (string->xexpr (file->string "not-found.html"))) )
    )
  
  show-post)

(define (create-static-page-function a-blog language url-path)
  
  (define a-blog-page-details (blog-page-details a-blog language url-path))  
  (define go-back-string (get-blog-go-back-string! a-blog language))      

  (define (static-page req a-static-page index)

    (define (language-url lang)
    ;;; Returns the url of a static page given a language and the
      (string-append "/" (blog-page-details-url-path a-blog-page-details) lang "/" (static-page-url (static-page-title (get-static-page a-blog lang index)))))
    
    (template-response
     (let ([languages-list (blog-languages-list a-blog)  ]
	   [title (static-page-title a-static-page)]
	   [body (static-page-content a-static-page)]
	   [go-back-string-url (gen-path a-blog-page-details "")]
	   )
       (include-template "static_pages_template.html" ))))
  
  (define (show-static-page req static-page-url-title)
    (define static-pages-list (get-static-pages a-blog language ))

    (define (auxf sp-list index)
      ;sp=static pages
      (if (null? sp-list)
    	  (values empty 0)
    	  (let ([cur-sp (car sp-list)])
    	    (if (string=? (static-page-url (static-page-title cur-sp)) static-page-url-title)
    		(values cur-sp index)
    		(auxf (cdr  sp-list) (+ index 1))))))
    
    (define-values (cur-static-page index)  (auxf static-pages-list 1))

    (if (null? cur-static-page)
	(response/xexpr (string->xexpr (file->string "not-found.html")))
	(static-page req cur-static-page index)
	))
      
  show-static-page  
)

(define (static-page-url x)
  (string-replace 
   (string-replace
    (string-downcase x) " " "-")
   "à"  "a" ))


(define (create-main-page-function a-blog n-posts language url-path)
  ;; At this point the database for the blog must have been initialized
  (define a-blog-page-info (blog-page-details a-blog language url-path))
    
  ;; db is the path string to the database  
  ;; n-posts is the number of posts to show on the main page

  (define (post-path a-post)
    (string-append
     (gen-path a-blog-page-info "id/") 
     (number->string (post-id a-post)) ))

  (define (static-page-path a-static-page)    
    (gen-path a-blog-page-info (static-page-url (static-page-title a-static-page))))

  

  
  (define (start request)

    (define (change-language-path lang )
      (string-append "/"
		     (blog-page-details-url-path a-blog-page-info)
		     (change-language-url request lang) )
      )

    (template-response (let ([languages-list (blog-languages-list a-blog ) ]
			     [url-path (blog-page-details-url-path a-blog-page-info)]
			     [page-http-request request]
			     [blog-title (get-main-page-title! a-blog-page-info)]
			     [posts-list (map row->post (get-last-posts! a-blog n-posts)) ]
			     [static-pages-list (get-static-pages a-blog language) ])	      
			 (include-template "main_page_template.html")))
    
    
    )
  start)

;; create-main-page-function should be assigned to a function called main-page  


(require web-server/servlet-env)

(define-syntax blog-start
  ;; The database should have been initialized before calling this macro
  (syntax-rules ()
    [ (blog-start database-file n-posts default-language url-path root-path  )
      ;; url-path should have all the necessary /
      (begin
	
	(define the-blog (initialize-blog! database-file))
	;(define the-blog-page-details (blog-page-details the-blog default-language))
	
	;(set-blog-current-language! the-blog (most-likely-language ))

	(define (main-page-creator req lang)
	  (if (member lang (blog-languages-list the-blog) )
	      ((create-main-page-function the-blog n-posts lang url-path) req )
	      (redirect-to (string-append
			    "/"
			    url-path			    
			    (guess-language (blog-languages-list the-blog)
					    default-language
					    (get-browser-languages req))
			    ))))

	(define (show-post-creator req lang id)
	  (if (member lang (blog-languages-list the-blog) )
	      ((create-post-detail-function the-blog lang url-path) req id )
	      (redirect-to (string-append
			    "/"
			    url-path
			    (guess-language (blog-languages-list the-blog)
					    default-language
					    (get-browser-languages req))
			    "/id/"
			    (number->string id)
			    ))
	      ) )

	(define (static-page-creator req lang static-page-title)
	  
	  (with-output-to-file "/tmp/title.txt" #:exists 'replace
	    (lambda ()
	      (display static-page-title)
	      )
	    )
	  
	  (if (member lang (blog-languages-list the-blog) )
	      ((create-static-page-function the-blog lang url-path) req static-page-title)
	      (redirect-to (string-append
			    "/"
			    url-path
			    (guess-language (blog-languages-list the-blog)
					    default-language
					    (get-browser-languages req))			    
			    static-page-title
			    ))
	      )
	 
	  )

	
	
	
	
	;	(define show-post (create-post-detail-function the-blog))
	
       ;       (define start (create-dispatch-function main-page ))

       (define-values ( blog-dispatch blog-url )
	 (dispatch-rules
					;[("") main-page]

	  
	  [((string-arg)) main-page-creator]
	  [((string-arg) "" ) main-page-creator]
	  [((string-arg) "id" (number-arg))  show-post-creator]	  
	  [((string-arg) (string-arg)) static-page-creator ]
	  [((string-arg) (string-arg) "") static-page-creator ]
	  
	  
;	  [("") main-page ]
	  ))

       (define (start req)
	 (blog-dispatch req)
	 )
       
       
       (serve/servlet start
	       #:launch-browser? #f
	       #:quit? #f
	       #:listen-ip #f
	       #:servlet-regexp
	       (regexp
		(string-append "^/*$|^/[a-z]+/*$|^/[a-z]+/[a-z-]+/*$|^/[a-z]+/id/.*" ))	       
	       #:port 8000
	       #:server-root-path (build-path root-path)
	       #:file-not-found-responder
	       (gen-file-not-found-responder
		(build-path root-path "htdocs"
		 "not-found.html"))
	       #:extra-files-paths
	       (list (build-path root-path "htdocs")
		     (build-path root-path "htdocs" "MathJax-master"))
	       #:servlet-path "/"))]))


(provide (all-defined-out))
