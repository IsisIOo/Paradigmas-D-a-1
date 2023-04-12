#lang racket
;TDA ARCHIVO/CARPETA
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
(require "tda_usuario.rkt")

;capa constructora, paso 1 para crear el camino
(define (carpeta string)
    (if(is-string string)
       (make-carpeta string null null null)
       (display "no se puede crear la ruta")))

(define (make-carpeta string user)
  (list string user null null (current-seconds))) ;este current seconds es la fecha de modificacion, aunque no se como mantenerlo fijo

;capa selectora
(define get-name-carpeta car)


#|FUNCION 9 MD
DOMINIO: system X name (String) 
RECORRIDO: SYSTEM
RECURSION: no sé
DESCRIPCION:  función que permite crear directorio dentro de una unidad a partir del nombre especificado.
Internamente la función añade datos relativos a usuario creador, fecha de creación, fecha de última
modificación y atributos de seguridad como los señalados en el enunciado general|#

(define md
  (lambda(system)
    (lambda(string)
      (if(and(is-string string)(not(member string (map car (get-system-ruta system)))))
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system);asigna lista vacia a los usuarios logeados
                     (get-system-drive-seleccionado system)
                     (cons(make-carpeta string (get-system-usuario-conectado system))(get-system-ruta system)))
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system);asigna lista vacia a los usuarios logeados
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))));que devuelva lo mismo en caso de que no cumpla
             
             

(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))

S13
S14
S15
S16







(provide (all-defined-out))