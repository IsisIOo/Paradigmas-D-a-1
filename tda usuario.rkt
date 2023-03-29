#lang racket
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
#| TDA USUARIO
primero se declarará la variable user donde tendrá su nombre, el cual será irrepetible,
se tratará de hacer funcion donde se determine si un usuario es igual a otro
-----TDA system - register-----
constructor:crea usuario
DESCRIPCION DE LA FUNCION: Función que permite registrar un nuevo usuario al
sistema. El nombre de usuario es único y no puede ser duplicado.
Dominio: system x username (string)
Recorrido: system
RECURSION: NO
ideas: se necesitan las funciones creadas en tda system(nombre temporal) para poder registrar un usuario en lista usuarios
|#

(define (make-user string) ;se supone que crea el usuario
  (list string))   

(define get-usuarios car);obtener usuarios


(define add-user
  (lambda(system)
    (lambda (string)
      (if(member string (map car(get-system-usuarios system))) ;buen
         ((display "ya existe el usuario")(display string)system) ;if
         (make-system(get-system-name system);else
                     (get-system-drive system)
                     (cons(make-user string)(get-system-usuarios system)))))))
           

#|FUNCION PARA VERIFICAR SI EL USUARIO SE REGISTRÓ
Pertenencia: verifica si el usuario ya existia o no
DESCRIPCION DE LA FUNCION: funcion ligada a la no duplicidad de nombres de usuario
dominio: lista
recorrido: boolean
recursion: no
|#

(define (is-user? user)
  (if(list? user) ;pregunta si user es lista, en este caso s4 lo es
     (if(string?(car(car(car(reverse user)))))
        #t   ;if
        #f) ;else
     #f))






(define S4 ((run S3 add-user) "user1"))
S4
(define S5 ((run S3 add-user) "user1"))
;(is-user? S4)
S5
(define S6 ((run S3 add-user) "user2"))
S6

;(map string? S4);para saber si hay un string en la lista
;(map string? (car(car(reverse S4)))); para saber si hay string en la lista usuarios
;(member "user1" (map car(car(reverse S4)))) ;verifica si en la lista está cierto usuario


