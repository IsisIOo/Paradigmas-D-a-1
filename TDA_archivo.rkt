#lang racket
;TDA ARCHIVO/CARPETA
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
(require "tda_usuario.rkt")

;capa constructora, paso 1 para crear el camino
(define (carpeta string)
    (if(is-string string)
       (make-carpeta null null null null null null)
       #f))

(define (files string)
  (list string null null))

;IMPORTANTE
;(string #\D)




;capa selectora
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system))))))))))
(define get-my-string-posicion (lambda (system) (car(car(get-system-ruta system))))) ;obtiene el string de la ultima ruta utilizada
(define get-files (lambda (system) (car(cdr(cdr(car(get-system-ruta system)))))))



#|FUNCION 9 md
DOMINIO: system X name (String) 
RECORRIDO: SYSTEM
RECURSION: no sé
DESCRIPCION:  función que permite crear directorio dentro de una unidad a partir del nombre especificado.
Internamente la función añade datos relativos a usuario creador, fecha de creación, fecha de última
modificación y atributos de seguridad como los señalados en el enunciado general|#

(define md
  (lambda(system)
    (lambda(nombre)
      (if(and(is-string nombre)(not(member nombre (map cadr (get-system-ruta system)))))
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (cons(make-carpeta (get-posicion system) nombre (get-system-usuario-conectado system)
                                        '()) (get-system-ruta system)))
         
         (make-system(get-system-name system);falso
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))));que devuelva lo mismo en caso de que no cumpla

#|FUNCION 10 CD
DOMINIO: system X path or folderName (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION: función que permite cambiar la ruta (path) donde se realizarán operaciones.
cd permite cambiarse a un directorio especificado a partir de la ruta señalada en un String.
Además, contará con con los comodines especiales “/” que permitirán
regresar a la carpeta del nivel anterior siguiendo la ruta actual del usuario y volver a la raíz de
la unidad respectivamente.|#

(define cd
  (lambda(system)
    (lambda (path)
      (if (is-string path)
          (if(member path (map cadr(get-system-ruta system)))
             (make-system(get-system-name system);verdadero
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-append (get-posicion system) path "/")
                                            null
                                            (get-system-usuario-conectado system)
                                            '())
                              (get-system-ruta system)))
             (if (equal? path "..")
                 (make-system(get-system-name system);verdadero
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (string-append(string-join(reverse(cdr(reverse(string-split (get-posicion system) "/"))))"/")"/")
                                                null
                                                (get-system-usuario-conectado system)
                                                '())
                                  (get-system-ruta system)))
                    
                 (if(equal? path "/")
                    (make-system(get-system-name system);verdadero
                                (get-system-drive system)
                                (get-system-usuarios system)
                                (get-system-usuario-conectado system)
                                (get-system-drive-seleccionado system)
                                (cons(make-carpeta (substring (car(car(get-system-ruta system))) 0 3)
                                                   null
                                                   (get-system-usuario-conectado system)
                                                   '())
                                     (get-system-ruta system)))

                    (if(member (car(reverse(string-split path "/")))(map cadr(get-system-ruta system)))
                       (make-system(get-system-name system)
                                   (get-system-drive system)
                                   (get-system-usuarios system)
                                   (get-system-usuario-conectado system)
                                   (car(string-split path "/"))
                                   (cons(make-carpeta path
                                                      null
                                                      (get-system-usuario-conectado system)
                                                      '())
                              (get-system-ruta system)))

                    (make-system(get-system-name system);verdadero
                                (get-system-drive system)
                                (get-system-usuarios system)
                                (get-system-usuario-conectado system)
                                (get-system-drive-seleccionado system)
                                (get-system-ruta system))))))
                       
          (make-system(get-system-name system);verdadero
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))


                        
  

#|FUNCION 11  TDA system - add-file
DOMINIO:system X file
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función que permite añadir un archivo en la ruta actual.
|#
(define add-file
  (lambda(system)
    (lambda(file)
      (if (map is-string (list file))
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (cons(make-carpeta (get-posicion system)
                                         null
                                         (get-system-usuario-conectado system)
                                         (cons file (get-files system)))           
                           (get-system-ruta system)))
          
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
      
      
      
;capa constructora crea la list que contendrá los elementos del file
(define (make-file nombre ext cont rest) ;rest = atr1 atr2
  (list nombre ext cont rest))

;capa constructora, crea una variable del mismo nombre que lo que entra en la funcion para que lo que entregue se transforme en lista y sea trabajado como lista
(define file ;como makedrive
  (lambda (nombre ext cont . rest)
    (make-file nombre ext cont rest)))
    
#|TDA FUNCION 12 DEL
DOMINIO:system X fileName or fileNamePattern (string)
RECORRIDO: system
RECURSION: NO C
DESCRIPCION: función para eliminar un archivo o varios archivos en base a un patrón determinado.
Esta versión también puede eliminar una carpeta completa con todos sus subdirectorios.
El contenido eliminado se va a la papelera.
|#

;muchos filtros
;if null remove
;















#|FUNCION 18 FORMAT
DOMINIO:system X letter (char) X name (String)
RECORRIDO: system 
RECURSION: NO
DESCRIPCION:función para formatear una unidad dada su letra, lo que borra todo su contenido,
además de indicar nuevo nombre, pero conservando capacidad.
|#

(define format
  (lambda (system)
    (lambda(letter nombre)
      (if(and(is-char letter)(is-string nombre))
         (capacidad letter system nombre)
         
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (string-downcase (car(car(get-system-ruta system)))))))))



;capa selectora, la pude hacer en la funcion pero me costo el filter. saca los elementos de la lista que tienen la misma primera letra en el primer elemento
(define (remove letter system)
      (filter (lambda(x)(if (not(equal?(string-append(string letter)":") (car(string-split (car x) "/"))))
                            #t
                            #f))
              (get-system-ruta system)))


;capa selectora elimina los drives que tengan la misma letra de primer elemento que la letra que entra, lo hace en un lugar diferente que el de arriba
(define (remove2 letter system) 
      (filter (lambda(x)(if (not(equal? letter  (car x)))
                            #t
                            #f))
              (get-system-drive system)))



;capa selectora, encuentra el drive con la misma letra para sacar propiedades/capacidad
(define (no-remove letter system)
      (filter (lambda(x)(if (equal? letter (car x))
                            #t
                            #f))
              (get-system-drive system)))


;capa modificadora obtener capacidad
(define (capacidad letter system nombre)
  (if (and(is-string nombre)(is-char letter))
      (if (member letter (map car(get-system-drive system)))
          ;(map caddr (get-system-drive S30)) capacidades
          (make-system(get-system-name system);verdadero
                      (cons(make-drive letter nombre (car (cdr (cdr (car(no-remove letter system))))))(remove2 letter system))
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (remove letter system))
          #f)
      #f))
              



;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2")) ;funciona
(define S16 ((run S15 md) "folder3"))

;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2")) ;funciona

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21")) ;funciona
(define S19 ((run S18 md) "folder21")) ;cumple

;;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S21
(define S20 ((run S19 cd) "folder21")) ;funciona
(define S21 ((run S20 cd) "folder22")) ;funciona 16-04-23

;;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))  ;funciona 16-04-23

;;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21")) ;segun yo si deberia poder a entrar, pues la carpeta no se ha eliminado. Agregar "/"

;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211")) ;funciona 16-04-23
(define S25 ((run S24 cd) "folder211")) ;funciona 16-04-23

;vuelve a la raíz de la unidad c:/
(define S26 ((run S25 cd) "/")) ;funciona 16-04-23
      
#|S13
S14
S15
S16
S17
S18
S19
S20
S21
S22
S23
S24
S25
S26 ;todo bem|#

    
;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D)) ;FUNCIONA

;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5")) ;FUNCIONA
(define S29 ((run S28 cd) "folder5")) ;FUNCIONA

;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/")) ;no funciona, supongo que la dejaré sin funcionar 75%

;formateando drive D:
(define S31 ((run S30 format) #\D "newD")) ;FUNCIONAAAA 17-04-2023
      
;añadiendo archivos
(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1"))) ;funciona 17-04 
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2"))) ;funciona 17-04 
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3"))) ;funciona 17-04 
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;funciona 17-04 , no me agrada la lista
;con atributos de seguridad oculto (h) y de solo lectura (r)

;eliminando archivos
(define S36 ((run S35 del) "*.txt"))
(define S37 ((run S35 del) "f*.docx"))
(define S38 ((run S35 del) "goo4.docx"))
(define S39 ((run S35 cd) ".."))
(define S40 ((run S35 del) "folder1"))





      
S27
S28
S29
S30
S31
S32
S33
S34
S35

;(cons(make-carpeta (string-append (string(car(get-system-drive-seleccionado system)))":/" nombre "/")


;MD NO REPITE CARPETAS, EN ESO FUNCIONA
;CD NO DEJA METER CARPETAS DENTRO DE CARPETAS

;(make-carpeta (string-append (string(car(get-system-drive-seleccionado system)))":/") (append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system))) (get-system-usuario-conectado system))
;                         (get-system-ruta system))) ;si está en el resto de lugares no se agrega
                         ;(append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system)))))

(provide (all-defined-out))