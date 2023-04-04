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

(define (make-user string);se supone que crea el usuario
  (if(is-string string)
     (list string)
     (display "no se creará el usuario\n"))) ;revisa si es string



;capa selectora
(define get-usuarios car);obtener usuarios


(define add-user
  (lambda(system)
    (lambda (string)
      (if(member string (map get-usuarios(get-system-usuarios system))) ;buen
         (display "ya existe el usuario \n") ;if
         (make-system(get-system-name system);else
                     (get-system-drive system)
                     (cons(make-user string)(get-system-usuarios system))
                     (get-system-usuario-conectado system)))))) ;no me guarda los otros usuarios
           
;capa de pertenencia verifica si está el usuario en lista de usuarios
(define (comprobar string system)
  (member string (map get-usuarios(get-system-usuarios system))))
     
#|FUNCION PARA VERIFICAR SI ES USUARIO
Pertenencia: verifica si el usuario ya existia o no
DESCRIPCION DE LA FUNCION: funcion ligada a la no duplicidad de nombres de usuario
dominio: lista
recorrido: boolean
recursion: no
la verdad no me sirve mucho, pero la dejo por si acaso AHORA SI SIRVE SLDKSÑDFLK

(define (is-user? user)
  (if(list? user) ;pregunta si user es lista, en este caso s4 lo es
     (if(string?(car(car(car(reverse user)))))
        (cons #t (car(car(car(reverse user)))))  ;if
        (cons #f (car(car(car(reverse user))))));else
     (display "el usuario no existe o no se pudo crear\n"))) ;referente a que se copió el nombre de otro usuario
|#

  
#|FUNCION LOGIN
DOMINIO:system X userName (String)
RECORRIDO: system
RECURSION: 
DESCRIPCION: Función que permite iniciar sesión con un usuario del sistema, solo si éste existe.|#


;capa constructura
(define (status-user string)
  (if(is-string string)
     (list string)
     (display "no se puede iniciar sesion\n")))

(define login
  (lambda(system)
    (lambda (string)
      (if (null? (get-system-usuario-conectado system))
          (if (comprobar string system);verdadero
              (make-system(get-system-name system);verdadero
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (cons(status-user string)(get-system-usuario-conectado system)))
              (display "no se puede iniciar sesion, el usuario no existe\n")) ;falso
          (display "ya hay un usuario conectado\n"))))) ;falso

#|FUNCION LOGOUT
DOMINIO:system 
RECORRIDO: system
RECURSION: no
DESCRIPCION: Función que permite cerrar la sesión de un usuario en el sistema.
|#        

(define (logout system)
  (if (not(null?(get-system-usuario-conectado system)))
      (make-system(get-system-name system);verdadero
                  (get-system-drive system)
                  (get-system-usuarios system)
                  '()) ;asigna lista vacia a los usuarios logeados
      (display "no hay usuario conectado para deslogear\n")))




      

(define S4 ((run S3 add-user) "user1")) ;guarda 1
S4

(define S6 ((run S4 add-user) "user2")) ;regresa mensaje
S6

;(define S6 ((run S5 add-user) "user1"))
;S6

(define S7 ((run S6 login) "user1"))
S7
;(define S8 ((run S7 login) "user2"))
;S8

(define S9 (run S7 logout))
S9

#|(define S4 ((run S3 add-user) "user1")) ;guarda 1
S4

(define S5 ((run S4 add-user) "user2")) ;regresa mensaje
S5

(define S6 ((run S5 add-user) "user1"))
S6
SCRIPTS ORIGINALES|#

;(is-user? S4)
;(is-user? S5)

#|(define S4 ((run S3 add-user) "user1")) ;guarda 1
S4
(define S5 ((run S4 add-user) "user1")) ;regresa mensaje
(is-user? S4)
S5
(define S6 ((run S4 add-user) "user2")) ;guarda 2
S6
(define S7 ((run S6 add-user) "user3")) ;guarda 3, con S5 no funciona, S6 si, S3 tambien pero solo guarda user3
S7
(define S8 ((run S7 add-user) "user3"))
S8
;S5 NO FUNCIONA POR ALGUNA RAZON|#




;(map string? S4);para saber si hay un string en la lista
;(map string? (car(car(reverse S4)))); para saber si hay string en la lista usuarios
;(member "user1" (map car(car(reverse S4)))) ;verifica si en la lista está cierto usuario

;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
;(define S7 ((run S6 login) "user1"))
;(define S8 ((run S7 login) "user2"))
;(define S8 ((run S7 login) "user1"))
