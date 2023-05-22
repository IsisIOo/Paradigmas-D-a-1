#lang racket
;tda folder
;CAPA CONSTRUCTORA

#|recursion-system
DOMINIO: system
RECORRIDO: string
RECURSION: de cola
DESCRIPCION: Se encarga de formar un string de las carpetas del sistema
tiene el problema de que no puedo eliminar folder1, pues ya no se llama asi|#
(define (recursion-system system) 
  (define (mostrar-directory folders direc)
    (if(null? folders)
       direc
       (mostrar-directory (cdr folders) (string-append direc "-" (car folders) "\n" ))))
  (mostrar-directory (remove-duplicates(remove-nulos system)) "")) ;tdafolder

#|recursion-system-archivos2
DOMINIO: system
RECORRIDO: string
RECURSION: de cola
DESCRIPCION: Se encarga de crear un string con las carpetas y los contenidos de cada una
el contenido se diferencia del titulo de la carpeta con un /|#
(define (recursion-system-archivos2 system)
  (define (mostrar-directory2 lista folders direc)
    (if (null? folders)
        direc ;tdafolder
        (mostrar-directory2 lista (cdr folders) (string-append direc "-" (car folders) " / " (string-join (map car (rec-archivos (car folders) system)) " " )"\n")))) 
  (mostrar-directory2 (get-system-ruta system) (remove-duplicates (remove-nulos system)) ""))





;CAPA MODIFICADORA

#|capacidad
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: funcion que crea el drive con nombre cambiado y utilizando la funcion no-remove para poder
 obtener la capacidad del drive previo, crea el nuevo sistema con el cambio en los drives|#

(define (set-capacidad letter system nombre) ;tda drive
  (if (and(is-string nombre)(is-char letter))
      (if (member letter (map car(get-system-drive system)))
          (make-system(get-system-name system);verdadero
                      (cons(make-drive letter nombre (car (cdr (cdr (car(no-remove letter system))))))(remove2 letter system))
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (remove1 letter system))
          #f)
      #f))
              
#|encriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que encripta todo el contenido de una carpeta|#
(define encriptar
  (lambda(system path) ;tda folder
    (define encriptar2
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(member path (map cadr (get-system-ruta system)))
              (encriptar2 (cdr lista) (append vacio (list(cons (string-append(plus-one(car(string-split(car(car lista))".")))"." (cadr(car(rec-archivos path system))))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (plus-one(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar2 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar2 (rec-archivos path system) '())))



;Hola, estos los deje aqui tambien para que funcionara el codigo, no me di cuenta que se formaba una dependencia circular
(define minus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (- x 1)) (map char->integer (string->list string)))))
        #f)))

(define plus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (+ x 1)) (map char->integer (string->list string)))))
        #f)))

#|desencriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que desencripta los archivos de una carpeta (varios archivos simultaneos)|#
(define desencriptar
  (lambda(system path) ;tdafolder
    (define encriptar5
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(member (plus-one path) (map cadr (get-system-ruta system)))
              (encriptar5 (cdr lista) (append vacio (list(cons (string-append(minus-one(plus-one(car(string-split(car(car lista))"."))))"." (cadr(car(rec-archivos path system))))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (minus-one(plus-one(caddr(car lista))))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar5 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar5 (rec-archivos path system) '())))




;CAPA SELECTORA
#|get-posicion
DOMINIO: system
RECORRIDO: ruta actual
RECURSION: no
DESCRIPCION: obtiene la ruta mas actual del sistema que se entrega|#
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system)))))))))) ;tda carpeta


#|get-carpetas
DOMINIO: system
RECORRIDO: carpeta abierta o creada
RECURSION: no
DESCRIPCION: obtiene la carpeta que ha sido creada o el dueño de los archivos
(depende de la funcion que se le haya aplicado) en md es carpeta creada, en move, copy, add-file es el dueño del archivo|#
(define get-carpetas (lambda (system) (car(cdr(car(get-system-ruta system)))))) ;tdafolder


#|get-password 
DOMINIO: system
RECORRIDO: password (string)
RECURSION: no
DESCRIPCION: obtiene la contraseña de la carpeta en caso de que hay sido encriptado|#
(define get-password (lambda (system) (car(cdr(cdr(cdr(cdr(car(get-system-ruta system))))))))) ;tdafolder

#|recuperar-ruta 
DOMINIO: system
RECORRIDO: system
RECURSION: no
DESCRIPCION: recupero la ultima ruta de mi drive actual luego de los cambios hechos por move o copy
lo necesito para ren pq no se deben cambiar los nombres en cualquier drive sino en el actual.|#

(define (recuperar-ruta system)
  (if (null? (filter(lambda (x) (if(equal? (string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system)))
      null
      (car(filter(lambda (x) (if(equal?(string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system))))) ;tdafolder



#|remove1 
DOMINIO: letter (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene y remueve elementos de la lista que tienen la primera letra igual a la primera letra de la ruta actual
tipo entra c y la ruta es c/folder1/, cumple|#

(define (remove1 letter system) ;tdacarpeta
      (filter (lambda(x)(if (not(equal?(string-append(string letter)":") (car(string-split (car x) "/"))))
                            #t
                            #f))
              (get-system-ruta system)))

#|remove2 
DOMINIO: letter (string) X system
RECORRIDO: get-system-drive
RECURSION: no
DESCRIPCION: obtiene y remueve drives que tengan la misma letra que letter entrante
lo ejecuta en un lugar distinto a remove1|#

(define (remove2 letter system)  ;tdacarpeta
      (filter (lambda(x)(if (not(equal? letter  (car x)))
                            #t
                            #f))
              (get-system-drive system)))



#|remove-posicion 
DOMINIO: file (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: obtiene y remueve una carpeta de nombre file de todas las posiciones del sistema como ruta o
carpeta actual.|#

(define (remove-posicion file system) ;tdacarpeta
  (filter (lambda(x) (if (not(or(equal? file (car(reverse(string-split (car x) "/")))) (equal? file (cadr x))))
                         #t
                         #f))
          (get-system-ruta system))) ;FUNCIONA

#|rec-archivos
DOMINIO: path (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: recupera los elementos que habian dentro de uan carpeta, esto es util en caso de que la
carpeta dueña de los archivos ya no esté en el estado mas actual, asi se recupera todo cuando se quiera volver a entrar
a ella |#
(define (rec-archivos path system) ;tda carpeta
  (if(null? (filter(lambda(x) (if(equal? path (cadr x))
                                 #t
                                 #f))
                   (get-system-ruta system)))
     null
     (car(cdr(cdr(car(filter (lambda(x) (if(equal? path (cadr x))                     
                                           #t
                                           #f))
                             (get-system-ruta system))))))))


#|remove-carpeta-drive
DOMINIO: source (string) X target (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: remueve uan carpeta de su drive previo, esto en caso de move carpeta a otro drive|#
(define (remove-carpeta-drive source target system)
      (filter (lambda(x) (if (and(not(equal? source (cadr x)))) ;borra los archivos igual al que entra                     
                         #t
                         #f))
          (get-system-ruta system))) ;tdacarpeta


#|no-remove
DOMINIO: filename (string) X system
RECORRIDO: get-system-drive
RECURSION: no
DESCRIPCION: obtiene la lista de drive que tiene a letter (las propiedades del drive letter) para asi poder obtener
su capacidad. Sirve para la funcion de renombrar drive|#
(define (no-remove letter system) ;tda drive
      (filter (lambda(x)(if (equal? letter (car x))
                            #t
                            #f))
              (get-system-drive system)))

#|remove-nulos
DOMINIO: system
RECORRIDO: lista de carpetas de ruta actual del sistema sin nulos
RECURSION: no
DESCRIPCION: obtiene las carpetas del sistema y elimina los nulos que habian entre medio|#
(define (remove-nulos system) ;tdafolder
  (filter (lambda (x) (not(equal? '() x))) (map cadr (recuperar-listas system))))

#|no-recuperar-listas
DOMINIO: system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: Obtiene las (get-system-ruta) rutas que se han armado en otros drive que no son el actual|#
(define (no-recuperar-listas system) ;tdafolder
  (if (null? (filter(lambda (x) (if(not(equal? (string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/")))))
                                   #t
                                   #f))
                    (get-system-ruta system)))
      null
      (filter(lambda (x) (if(not(equal?(string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/")))))
                                   #t
                                   #f))
                    (get-system-ruta system))))

#|no-recuperar-listas
DOMINIO: system
RECORRIDO: get-system-ruta
RECURSION: no ;tda carpeta
DESCRIPCION: Obtiene las (get-system-ruta) rutas/cambios de stack que se han armado en el drive actual|#
(define (recuperar-listas system) ;tdafolder
  (if (null? (filter(lambda (x) (if(equal? (string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system)))
      null
      (filter(lambda (x) (if(equal?(string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system))))


(provide (all-defined-out))