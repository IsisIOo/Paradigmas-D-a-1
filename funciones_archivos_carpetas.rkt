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


;CAPA MODIFICADORA

#|cambia-nombre-archivo
DOMINIO: system X nombre(string) X nuevo nombre (string)
RECORRIDO: archivo llamado nombre renombrado como nuevo nombre
RECURSION: no
DESCRIPCION: remueve el nombre previo del archivo y luego de hacerlo le agrega nuevo nombre a la lista del archivo|#

    (define (cambia-nombre-archivo system name new-name)
      (cons(car(remove-titulo2 name system)) (list(cons new-name (remove name (car(no-remove-archivo2 name system)))))))

#|capacidad
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: funcion que crea el drive con nombre cambiado y utilizando la funcion no-remove para poder
 obtener la capacidad del drive previo, crea el nuevo sistema con el cambio en los drives|#

(define (capacidad letter system nombre)
  (if (and(is-string nombre)(is-char letter))
      (if (member letter (map car(get-system-drive system)))
          ;(map caddr (get-system-drive S30)) capacidades
          (make-system(get-system-name system);verdadero
                      (cons(make-drive letter nombre (car (cdr (cdr (car(no-remove letter system))))))(remove2 letter system))
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (remove1 letter system))
          #f)
      #f))
             

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






(provide (all-defined-out))