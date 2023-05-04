#lang racket
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")


;CAPA CONSTRUCTORA

#|make-file
DOMINIO: system
RECORRIDO: archivos actuales en la carpeta abierta, ultima actualizacion 
RECURSION: no
DESCRIPCION: crea la lista que contendrá los elementos del file (funcion add-file), donde rest equivale
a los atributos que pueda tener el archivo (oculto, solo lectura)|#

(define (make-file nombre ext cont rest) ;rest = atr1 atr2
  (list nombre ext cont rest))



#|file
DOMINIO: file (lista de strings)
RECORRIDO: make-file file
RECURSION: no
DESCRIPCION: crea una varianle con el mismo nombre file para que lo que la lista que se crea aqui sea recibido por la
funcion add-file y trabaje con ella|#
(define file ;como makedrive
  (lambda (nombre ext cont . rest)
    (make-file nombre ext cont rest)))




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
  (mostrar-directory (remove-duplicates(remove-nulos system)) ""))

#|recursion-system-archivos2
DOMINIO: system
RECORRIDO: string
RECURSION: de cola
DESCRIPCION: Se encarga de crear un string con las carpetas y los contenidos de cada una
el contenido se diferencia del titulo de la carpeta con un /|#
(define (recursion-system-archivos2 system)
  (define (mostrar-directory2 lista folders direc)
    (if (null? folders)
        direc
        (mostrar-directory2 lista (cdr folders) (string-append direc "-" (car folders) " / " (string-join (map car (rec-archivos (car folders) system)) " " )"\n")))) 
  (mostrar-directory2 (get-system-ruta system) (remove-duplicates (remove-nulos system)) ""))





;CAPA MODIFICADORA

#|cambia-nombre-archivo
DOMINIO: system X nombre(string) X nuevo nombre (string)
RECORRIDO: archivo llamado nombre renombrado como nuevo nombre
RECURSION: no
DESCRIPCION: remueve el nombre previo del archivo y luego de hacerlo le agrega nuevo nombre a la lista del archivo|#
(define (cambia-nombre-archivo system name new-name)
  (append(remove-titulo2 name system) (list(cons new-name (remove name (car(no-remove-archivo2 name system)))))))

#|capacidad
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: funcion que crea el drive con nombre cambiado y utilizando la funcion no-remove para poder
 obtener la capacidad del drive previo, crea el nuevo sistema con el cambio en los drives|#

(define (set-capacidad letter system nombre)
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

#|dominio: source x system
recorrido:system
recursion: de cola
descripcion: elimina el archivo original que ha sido movido a otro drive o carpeta|#
(define eliminar_archivo
  (lambda(system source)
    (define DelFile
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if (null? (caddr(car lista)))
               (DelFile (cdr lista) (append vacio (list (car lista))))
               (if(and(member source (map car(caddr (car lista))))(equal? (car(string-split(get-posicion system)"/")) (car(string-split (car(car lista)) "/"))))
                  (DelFile (cdr lista) (append vacio (list(cons (car (car lista))
                                                           (cons (cadr (car lista))
                                                                 (cons (filter(lambda (x) (if (not(equal? source (car x)))
                                                                                              #t
                                                                                              #f))(caddr(car lista)))
                                                                       (cons (cadddr(car lista))
                                                                             (cons (cadddr(cdr (car lista))) '()))))))))
                  (DelFile (cdr lista) (append vacio (list (car lista)))))))))
      (DelFile (get-system-ruta system) '())))
              
#|encriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que encripta todo el contenido de una carpeta|#
(define encriptar
  (lambda(system path)
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

#|encriptar-t
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que encripta el archivo solicitado de una carpeta|#
(define encriptar-t 
  (lambda(system path)
    (define encriptar3
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(equal? path  (car(car lista)))
              (encriptar3 (cdr lista) (append vacio (list(cons (string-append(plus-one(car(string-split(car(car lista))".")))"." (cadr(car lista)))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (plus-one(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar3 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar3 (rec-archivos (get-carpetas system) system) '())))

#|desencriptar-t
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION: Funcion que encuentra el archivo encriptado y lo desencripta|#
(define desencriptar-t 
  (lambda(system path)
    (define encriptar4
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(equal? (string-append(plus-one(car(string-split path ".")))"."(car(cdr(string-split path "."))))  (car(car lista)))
              (encriptar4 (cdr lista) (append vacio (list(cons (string-append(minus-one(car(string-split(car(car lista))".")))"." (cadr(car lista)))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (minus-one(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar4 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar4 (rec-archivos (get-carpetas system) system) '())))   

#|desencriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que desencripta los archivos de una carpeta (varios archivos simultaneos)|#
(define desencriptar
  (lambda(system path)
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
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system))))))))))

#|get-files
DOMINIO: system
RECORRIDO: archivos actuales en la carpeta abierta, ultima actualizacion 
RECURSION: no
DESCRIPCION: obtiene los archivos mas actuales de acuerdo al sistema|#
(define get-files (lambda (system) (car(cdr(cdr(car(get-system-ruta system)))))))

#|get-carpetas
DOMINIO: system
RECORRIDO: carpeta abierta o creada
RECURSION: no
DESCRIPCION: obtiene la carpeta que ha sido creada o el dueño de los archivos
(depende de la funcion que se le haya aplicado) en md es carpeta creada, en move, copy, add-file es el dueño del archivo|#
(define get-carpetas (lambda (system) (car(cdr(car(get-system-ruta system))))))



(define get-password (lambda (system) (car(cdr(cdr(cdr(cdr(car(get-system-ruta system)))))))))



#|remove-titulo2 
DOMINIO: nombre del archivo (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene la lista de archivos sin el elemento del cual se desea cambiar el nombre
al combinarla con cambia-nombre-archivo logra insertar el archivo con nombre cambiado al sistema|#
(define (remove-titulo2 filename system) 
      (filter (lambda(x) (if (not(equal? filename (car x)))
                            #t
                            #f))
              (caddr(recuperar-ruta system))))

#|no-remove-archivo2
DOMINIO: nombre del archivo (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene la lista que contiene el nombre del archivo, al obtenerla se le puede remover el nombre
la diferencia con no-remove-archivo1 es que trabajan en distintas partes, este necesita recuperar la ultima actualizacion
de la ruta de acuerdo al drive en que estamos.|#

(define (no-remove-archivo2 file system)
      (filter (lambda(x)(if (equal? file (car x))
                            #t
                            #f))
              (caddr(recuperar-ruta system))))

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
                    (get-system-ruta system)))))



#|remove1 
DOMINIO: letter (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene y remueve elementos de la lista que tienen la primera letra igual a la primera letra de la ruta actual
tipo entra c y la ruta es c/folder1/, cumple|#

(define (remove1 letter system)
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

(define (remove2 letter system) 
      (filter (lambda(x)(if (not(equal? letter  (car x)))
                            #t
                            #f))
              (get-system-drive system)))



#|remove-extension 
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene y remueve los archivos que tengan la misma extension que la entrante
se ejecuta en los archivos actuales|#

(define (remove-extension filename system) 
      (filter (lambda(x) (if (not(equal? filename (cadr x)))
                            #t
                            #f))
              (get-files system)))


#|remove-titulo 
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene y remueve los archivos que tengan el mismo nombre que el filename entrante
se ejecuta en los archivos actuales|#

(define (remove-titulo filename system) 
      (filter (lambda(x) (if (not(equal? filename (car x)))
                            #t
                            #f))
              (get-files system)))


#|remove-extension 
DOMINIO: file (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: obtiene y remueve una carpeta de nombre file de todas las posiciones del sistema como ruta o
carpeta actual.|#

(define (remove-posicion file system)
  (filter (lambda(x) (if (not(or(equal? file (car(reverse(string-split (car x) "/")))) (equal? file (cadr x))))
                         #t
                         #f))
          (get-system-ruta system))) ;FUNCIONA


#|letter-titles
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: remueve los elementos que tengan la misma letra inicial de nombre del} archivo y
tengan una extension de archivo igual a la entrante |#

(define (letter-titles filename system) 
   (filter (lambda(x) (if(not(and(equal? (car(string-split filename "*")) (string(car(string->list(car x)))))
                                 (equal? (car(string-split (cadr(string-split filename "*")) ".")) (car(cdr x)))))
                    #t
                   #f))
       (get-files system)))


#|rec-archivos
DOMINIO: path (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: recupera los elementos que habian dentro de uan carpeta, esto es util en caso de que la
carpeta dueña de los archivos ya no esté en el estado mas actual, asi se recupera todo cuando se quiera volver a entrar
a ella |#
(define (rec-archivos path system)
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
          (get-system-ruta system)))


#|no-remove
DOMINIO: filename (string) X system
RECORRIDO: get-system-drive
RECURSION: no
DESCRIPCION: obtiene la lista de drive que tiene a letter (las propiedades del drive letter) para asi poder obtener
su capacidad. Sirve para la funcion de renombrar drive|#
(define (no-remove letter system)
      (filter (lambda(x)(if (equal? letter (car x))
                            #t
                            #f))
              (get-system-drive system)))

#|no-remove-archivo
DOMINIO: file (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene las propiedades del archivo file en su estado mas actual para asi poder moverlo a otra ubicacion
Sirve para la funcion move y copy.|#
(define (no-remove-archivo file system)
      (filter (lambda(x)(if (equal? file (car x))
                            #t
                            #f))
              (get-files system)))

#|remove-nulos
DOMINIO: system
RECORRIDO: lista de carpetas de ruta actual del sistema sin nulos
RECURSION: no
DESCRIPCION: obtiene las carpetas del sistema y elimina los nulos que habian entre medio|#
(define (remove-nulos system)
  (filter (lambda (x) (not(equal? '() x))) (map cadr (recuperar-listas system))))

#|no-recuperar-listas
DOMINIO: system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: Obtiene las (get-system-ruta) rutas que se han armado en otros drive que no son el actual|#
(define (no-recuperar-listas system)
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
RECURSION: no
DESCRIPCION: Obtiene las (get-system-ruta) rutas/cambios de stack que se han armado en el drive actual|#
(define (recuperar-listas system)
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