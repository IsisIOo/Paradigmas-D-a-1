#lang racket
(require "tda_system_21168603_OyanedelAlvarez.rkt")
(require "tda_drive_21168603_OyanedelAlvarez.rkt")

;OTRAS FUNCIONES

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
  (mostrar-directory (remove-duplicates(get-remove-nulos system)) ""))

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
        (mostrar-directory2 lista (cdr folders) (string-append direc "-" (car folders) " / " (string-join (map car (get-rec-archivos (car folders) system)) " " )"\n")))) 
  (mostrar-directory2 (get-system-ruta system) (remove-duplicates (get-remove-nulos system)) ""))





;CAPA MODIFICADORA
              
#|encriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que encripta todo el contenido de una carpeta|#
(define set-encriptar
  (lambda(system path fn1)
    (define encriptar2
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(member path (map cadr (get-system-ruta system)))
              (encriptar2 (cdr lista) (append vacio (list(cons (string-append(fn1(car(string-split(car(car lista))".")))"." (cadr(car(get-rec-archivos path system))))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (fn1(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar2 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar2 (get-rec-archivos path system) '())))

#|desencriptar
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que desencripta los archivos de una carpeta (varios archivos simultaneos)|#
(define set-desencriptar
  (lambda(system path fn1 fn2)
    (define encriptar5
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(member (fn1 path) (map cadr (get-system-ruta system)))
              (encriptar5 (cdr lista) (append vacio (list(cons (string-append(fn2(fn1(car(string-split(car(car lista))"."))))"." (cadr(car(get-rec-archivos path system))))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (fn2(fn1(caddr(car lista))))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar5 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar5 (get-rec-archivos path system) '())))




;CAPA SELECTORA

#|get-carpetas
DOMINIO: system
RECORRIDO: carpeta abierta o creada
RECURSION: no
DESCRIPCION: obtiene la carpeta que ha sido creada o el dueño de los archivos
(depende de la funcion que se le haya aplicado) en md es carpeta creada, en move, copy, add-file es el dueño del archivo|#
(define get-carpetas (lambda (system) (car(cdr(car(get-system-ruta system))))))

#|get-password 
DOMINIO: system
RECORRIDO: password (string)
RECURSION: no
DESCRIPCION: obtiene la contraseña de la carpeta en caso de que hay sido encriptado|#
(define get-password (lambda (system) (car(cdr(cdr(cdr(cdr(car(get-system-ruta system)))))))))

#|get-recuperar-ruta 
DOMINIO: system
RECORRIDO: system
RECURSION: no
DESCRIPCION: recupero la ultima ruta de mi drive actual luego de los cambios hechos por move o copy
lo necesito para ren pq no se deben cambiar los nombres en cualquier drive sino en el actual.|#

(define (get-recuperar-ruta system)
  (if (null? (filter(lambda (x) (if(equal? (string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system)))
      null
      (car(filter(lambda (x) (if(equal?(string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/"))))
                                   #t
                                   #f))
                    (get-system-ruta system)))))


#|get-remove-posicion 
DOMINIO: file (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: obtiene y remueve una carpeta de nombre file de todas las posiciones del sistema como ruta o
carpeta actual.|#

(define (get-remove-posicion file system)
  (filter (lambda(x) (if (not(or(equal? file (car(reverse(string-split (car x) "/")))) (equal? file (cadr x))))
                         #t
                         #f))
          (get-system-ruta system))) ;FUNCIONA

#|get-rec-archivos
DOMINIO: path (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: recupera los elementos que habian dentro de uan carpeta, esto es util en caso de que la
carpeta dueña de los archivos ya no esté en el estado mas actual, asi se recupera todo cuando se quiera volver a entrar
a ella |#
(define (get-rec-archivos path system)
  (if(null? (filter(lambda(x) (if(equal? path (cadr x))
                                 #t
                                 #f))
                   (get-system-ruta system)))
     null
     (car(cdr(cdr(car(filter (lambda(x) (if(equal? path (cadr x))                     
                                           #t
                                           #f))
                             (get-system-ruta system))))))))


#|get-remove-carpeta-drive
DOMINIO: source (string) X target (string) X system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: remueve uan carpeta de su drive previo, esto en caso de move carpeta a otro drive|#
(define (get-remove-carpeta-drive source target system)
      (filter (lambda(x) (if (and(not(equal? source (cadr x))))                      
                         #t
                         #f))
          (get-system-ruta system)))


#|get-remove-nulos
DOMINIO: system
RECORRIDO: lista de carpetas de ruta actual del sistema sin nulos
RECURSION: no
DESCRIPCION: obtiene las carpetas del sistema y elimina los nulos que habian entre medio|#
(define (get-remove-nulos system)
  (filter (lambda (x) (not(equal? '() x))) (map cadr (get-recuperar-listas system))))




#|get-remove-nulos-archivos
DOMINIO: system
RECORRIDO: lista de archivos de ruta actual del sistema sin nulos
RECURSION: no
DESCRIPCION: obtiene los archivos del sistema y elimina los nulos que habian entre medio|#
(define (get-remove-nulos-archivos system)
  (filter (lambda (x) (not(equal? '() x))) (map caddr (get-recuperar-listas system))))


#|get-no-recuperar-listas
DOMINIO: system
RECORRIDO: get-system-ruta
RECURSION: no
DESCRIPCION: Obtiene las (get-system-ruta) rutas que se han armado en otros drive que no son el actual|#
(define (get-no-recuperar-listas system)
  (if (null? (filter(lambda (x) (if(not(equal? (string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/")))))
                                   #t
                                   #f))
                    (get-system-ruta system)))
      null
      (filter(lambda (x) (if(not(equal?(string-append(string(car(get-system-drive-seleccionado system)))":") (string-upcase(car(string-split (car x) "/")))))
                                   #t
                                   #f))
                    (get-system-ruta system))))

#|recuperar-listas
DOMINIO: system
RECORRIDO: get-system-ruta
RECURSION: no ;tda carpeta
DESCRIPCION: Obtiene las (get-system-ruta) rutas/cambios de stack que se han armado en el drive actual|#
(define (get-recuperar-listas system) 
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