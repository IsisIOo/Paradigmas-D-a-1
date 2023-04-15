#lang racket
;TDA ARCHIVO/CARPETA
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
(require "tda_usuario.rkt")

;capa constructora, paso 1 para crear el camino
(define (carpeta string)
    (if(is-string string)
       (make-carpeta null null null null null)
       (display "no se puede crear la ruta")))

(define (crear-ruta cd1)
      (list cd1))




;capa selectora
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system))))))))))
(define get-my-string-posicion (lambda (system) (car(car(get-system-ruta system))))) ;obtiene el string de la ultima ruta utilizada


;CAPA MODIFICADORA


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
                     (cons(make-carpeta (get-posicion system) nombre (get-system-usuario-conectado system)) (get-system-ruta system)))
         
         (make-system(get-system-name system);falso
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))));que devuelva lo mismo en caso de que no cumpla

#|FUNCION 10 CD
DOMINIO: system X path or folderName (String)
RECORRIDO: system
RECURSION: NO C
DESCRIPCION: función que permite cambiar la ruta (path) donde se realizarán operaciones.
cd permite cambiarse a un directorio especificado a partir de la ruta señalada en un String.
Además, contará con con los comodines especiales “/” que permitirán
regresar a la carpeta del nivel anterior siguiendo la ruta actual del usuario y volver a la raíz de
la unidad respectivamente.|#

(define cd
  (lambda(system)
    (lambda (path)
      (if(or(is-string path)(member path (map cadr(get-system-ruta system))))      ;(member path (map cadr (get-system-ruta system)
         (if (equal? path "..")
             (make-system(get-system-name system);verdadero
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-join(reverse(cdr(reverse(string-split (get-posicion system) "/"))))"/")
                                            null
                                            (get-system-usuario-conectado system))
                                            (get-system-ruta system)))
             (if(equal? path "/")
                (make-system(get-system-name system);verdadero
                            (get-system-drive system)
                            (get-system-usuarios system)
                            (get-system-usuario-conectado system)
                            (get-system-drive-seleccionado system)
                            (cons(make-carpeta (substring (car(car(get-system-ruta system))) 0 3)
                                                null
                                               (get-system-usuario-conectado system))
                                               (get-system-ruta system)))
             
                (make-system(get-system-name system);verdadero
                            (get-system-drive system)
                            (get-system-usuarios system)
                            (get-system-usuario-conectado system)
                            (get-system-drive-seleccionado system)
                            (cons(make-carpeta (string-append (get-posicion system) path "/")
                                               null
                                               (get-system-usuario-conectado system))
                                               (get-system-ruta system)))))
                     
                (make-system(get-system-name system);verdadero
                            (get-system-drive system)
                            (get-system-usuarios system)
                            (get-system-usuario-conectado system)
                            (get-system-drive-seleccionado system)
                            (make-carpeta (get-system-ruta system)
                                          null
                                          (get-system-usuario-conectado system)))))))     


;(filter (lambda (x) (eq? (car x) 5)) mi-lista))
                     
  

#|FUNCION 11  TDA system - add-file
DOMINIO:system X file
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función que permite añadir un archivo en la ruta actual.
|#

      










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
(define S21 ((run S20 cd) "folder22")) ;no agrega el folder22 a la ruta actual

;;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))

;;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21"))

;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))

;vuelve a la raíz de la unidad c:/
(define S26 ((run S25 cd) "/"))


      
S13
S14
S15
S16
S17
S18
S19
S20
S21
#|S22|#

;S26
;(cons(make-carpeta (string-append (string(car(get-system-drive-seleccionado system)))":/" nombre "/")


;MD NO REPITE CARPETAS, EN ESO FUNCIONA
;CD NO DEJA METER CARPETAS DENTRO DE CARPETAS

;(make-carpeta (string-append (string(car(get-system-drive-seleccionado system)))":/") (append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system))) (get-system-usuario-conectado system))
;                         (get-system-ruta system))) ;si está en el resto de lugares no se agrega
                         ;(append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system)))))

(provide (all-defined-out))