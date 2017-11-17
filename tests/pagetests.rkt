#lang racket

;; Es necesario tener una variable que corresponda al número de posts
;; a mostrar en la página principal.
(require "../model.rkt")
(require "../blogpage.rkt")

(let ((a-blog (initialize-blog! "layout_test.db" '("en" "es" "fr" "it" "eo") )) )

  ;; (set-blog-title! a-blog '( ("en" . "Title")
  ;; 			     ("es" . "Título")
  ;; 			     ("it" . "Titolo")
  ;; 			     ("fr" . "Titre" )
  ;; 			     ("eo" . "Titolo")))

  ;; (set-go-back-messages! a-blog '( ("en" . "Go back to main page")
  ;; 			     ("es" . "Regresar a la página principal")
  ;; 			     ("it" . "Tornare alla pagina principale")
  ;; 			     ("fr" . "Retourner à la page principal" )
  ;; 			     ("eo" . "Reveni al la ĉefa paĝo")))

  ;; (add-static-page a-blog '(
  ;; 			    ("en" "About" "About this blog" )
  ;; 			    ("es" "Acerca de" "Acerca de este blog" )
  ;; 			    ("fr" "À propos" "A propos de ce blog" )
  ;; 			    ("it" "Su questo blog" "Su questo blog")
  ;; 			    ("eo" "Pri" "Pri tiu ĉi blogo")			    			    
  ;; 			    ) )

  
  

  
  (blog-insert-post! a-blog
		     "Título 1"
		     "<p> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus molestie erat in dui vulputate, ut feugiat odio tristique. Cras lacinia nulla sit amet felis auctor, ut aliquam ligula blandit. Morbi id augue a enim vestibulum molestie sit amet non neque. Pellentesque vel egestas lorem. Nullam non aliquet sapien, sed placerat risus. Nunc molestie, justo quis maximus efficitur, libero tellus sagittis velit, eu ornare ex leo sit amet massa. In nec lectus a nisi tincidunt vehicula vitae nec velit. Donec sed consequat est. Proin quis finibus turpis, vitae venenatis orci. Sed facilisis nec lacus sed dignissim. Donec mollis quam quam, ut aliquam lorem posuere sed. Etiam hendrerit purus lorem, nec dignissim erat suscipit dignissim. </p>

<p> Vestibulum dignissim velit sed nibh dictum accumsan. Phasellus bibendum vel metus auctor dignissim. Donec ultrices, augue et tincidunt convallis, nulla mi iaculis lectus, sed tincidunt odio ex vitae risus. Maecenas eget libero tellus. Interdum et malesuada fames ac ante ipsum primis in faucibus. Pellentesque accumsan turpis et diam congue vehicula. Vivamus at sollicitudin ex, sit amet porta tellus. Nullam et tellus ac ligula auctor blandit eget in nulla. Ut arcu odio, tempus at elementum fermentum, gravida a arcu. Donec pulvinar, odio at fermentum pulvinar, sem sem posuere neque, sit amet lobortis ante erat placerat ex. Nullam dui nibh, elementum id laoreet eget, mollis vel odio. Duis lacinia risus quis erat sodales sagittis. Nunc eleifend lacus eget lacinia gravida. Nunc non tellus magna. In hac habitasse platea dictumst. Phasellus magna nulla, tincidunt quis pellentesque eget, mollis id nibh. </p>"
		     ) 

  (blog-insert-post! a-blog
		     "Título 2"
		     "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus molestie erat in dui vulputate, ut feugiat odio tristique. Cras lacinia nulla sit amet felis auctor, ut aliquam ligula blandit. Morbi id augue a enim vestibulum molestie sit amet non neque. Pellentesque vel egestas lorem. Nullam non aliquet sapien, sed placerat risus. Nunc molestie, justo quis maximus efficitur, libero tellus sagittis velit, eu ornare ex leo sit amet massa. In nec lectus a nisi tincidunt vehicula vitae nec velit. Donec sed consequat est. Proin quis finibus turpis, vitae venenatis orci. Sed facilisis nec lacus sed dignissim. Donec mollis quam quam, ut aliquam lorem posuere sed. Etiam hendrerit purus lorem, nec dignissim erat suscipit dignissim .</p>

<p>Vestibulum dignissim velit sed nibh dictum accumsan. Phasellus bibendum vel metus auctor dignissim. Donec ultrices, augue et tincidunt convallis, nulla mi iaculis lectus, sed tincidunt odio ex vitae risus. Maecenas eget libero tellus. Interdum et malesuada fames ac ante ipsum primis in faucibus. Pellentesque accumsan turpis et diam congue vehicula. Vivamus at sollicitudin ex, sit amet porta tellus. Nullam et tellus ac ligula auctor blandit eget in nulla. Ut arcu odio, tempus at elementum fermentum, gravida a arcu. Donec pulvinar, odio at fermentum pulvinar, sem sem posuere neque, sit amet lobortis ante erat placerat ex. Nullam dui nibh, elementum id laoreet eget, mollis vel odio. Duis lacinia risus quis erat sodales sagittis. Nunc eleifend lacus eget lacinia gravida. Nunc non tellus magna. In hac habitasse platea dictumst. Phasellus magna nulla, tincidunt quis pellentesque eget, mollis id nibh. </p>"
		     )

  )

;; (define (page-not-found-lang lang)
;;   (cond 
;;    ((string=? lang en)
;;     (html-page "Page Not Found"
;; 	       ,(string->xexpr
;; 		 (string-append
;; 		  "<h1>Page Not Found</h1>"
;; 		  "<a href=\"blog2\">Return to main page </a> "
;; 		  ))))
   
;;    ))

;; (define (page-not-found req)
  
;;   )


;(blog-start "layout_test.db" 10 "it" "blog/" "/home/oscar/blog/branch/blogo" )



;; url-path should be like lala/lala/ (so, no / at the beggining)
;; 
