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


(define register
  (lambda(system)
    (lambda (string)
      (if(member string (map car(get-system-usuarios system))) ;buen
         (make-system(get-system-name system)
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system)) ;if
         
         (make-system(get-system-name system);else
                     (get-system-drive system)
                     (cons(make-user string)(get-system-usuarios system))
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system)))))) ;no me guarda los otros usuarios
           

  
#|FUNCION LOGIN
DOMINIO:system X userName (String)
RECORRIDO: system
RECURSION: 
DESCRIPCION: Función que permite iniciar sesión con un usuario del sistema, solo si éste existe.|#

(define login
  (lambda(system)
    (lambda (string)
      (if (null? (get-system-usuario-conectado system))
          (if (comprobar string system);verdadero
              (make-system(get-system-name system);verdadero
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (cons(status-user string)(get-system-usuario-conectado system))
                          (get-system-drive-seleccionado system)
                          (get-system-ruta system))
              
              (make-system(get-system-name system);falso
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (get-system-usuario-conectado system)
                          (get-system-drive-seleccionado system)
                          (get-system-ruta system)))
          
          (make-system(get-system-name system);falso
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system)))))) 

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
                  '();asigna lista vacia a los usuarios logeados
                  (get-system-drive-seleccionado system)
                  (get-system-ruta system))
      
      (make-system(get-system-name system);falso
                  (get-system-drive system)
                  (get-system-usuarios system)
                  (get-system-usuario-conectado system)
                  (get-system-drive-seleccionado system)
                  (get-system-ruta system))))

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))


;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K)) ;no existe este drive 
(define S12 ((run S11 switch-drive) #\C))

#|
S4
S5
S6
S7
S8
S9
S10
S11
S12
|#

;(map string? S4);para saber si hay un string en la lista
;(map string? (car(car(reverse S4)))); para saber si hay string en la lista usuarios
;(member "user1" (map car(car(reverse S4)))) ;verifica si en la lista está cierto usuario

(provide (all-defined-out))