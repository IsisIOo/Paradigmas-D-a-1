#lang racket
;Isidora Oyanedel
;profesor: Gonzalo Martinez
;
#|TDA System - CONSTRUCTOR
-----------descripcion del TDA--------------
Función constructora de un sistema. Deja registro de la fecha de creación.

          --- CONSTRUCTOR ---
DOMINIO: string
RECORRIDO: system (conformado por nombre sistema, drive y usuario/s)
DESCRIPCION: se ingresa un string en la funcion devolviendo un system
             conformado por una lista de elementos que complementan el system
USO DE RECURSION: no se usa.

debe tener el nombre system y funcionar con las cosas del doc

probablemente se usa el TDA FECHA
|#

;(define (system string) 
 ; (list string 16042023))

(define (system string)
  (if(is-string string)
     (make-system string null null null null null) ;el primer elemento es el nombre, siguiente drive, sig usuario
     (display "no se creará el sistema")))    ;por mientras así, no creará el sistema si es numeros



;-capa constructora-
(define (make-system nombre drive usuario usuario-conectado drive-solicitado rutaa)
  (list nombre drive usuario usuario-conectado drive-solicitado rutaa (current-seconds)))

;-capa pertenencia-
(define (is-string string)
  (if(string? string)
     #t ;"se ha creado el sistema\n"
     #f))

;capa pertenencia
(define (is-char letter)
  (if(char? letter)
     #t ;"se ha creado el sistema\n"
     #f))

#|FUNCION RUN

DOMINIO: System X Command (funcion =command)
RECORRIDO: system
DESCRIPCION: Función que permite ejecutar un comando (función)
sobre un sistema. Toda acción realizada con run relativa a creación de archivos,
carpetas, renombrar, copiar, mover, eliminar, debe dejar un registro de la fecha de
modificación, además de verificar los permisos del recurso que será alterado|#

(define (run system command) ;se aplica una funcion en la lista system por ejemplo add-rive
  (command system))

#|Otras funciones necesarias, capa selectora|#
(define get-system-name car)
(define get-system-drive cadr)
(define get-system-usuarios caddr)
(define get-system-usuario-conectado cadddr)

;modificadora
(define(get-system-drive-seleccionado system)
  (caddr (reverse system))) ;se corre una posicion porque ahora el ultimo no es el tiempo

(define(get-system-ruta system)
  (cadr (reverse system)))

#|FUNCION ADD-DRIVE
DOMINIO: system x/dominio del primer lambda
         letter(char) x name(string) x capacity(int) /este se refiere a el dominio de la currificacion
RECORRIDO: system
DESCRIPCION: Función que permite añadir una unidad a un
sistema. La letra de la unidad es única.
RECURSION: no

|#

;capa constructora crea la estructura drive
(define (make-drive letter name capacity)
  (list letter name capacity))

(define add-drive
  (lambda(system)
    (lambda (letter name capacity);info del drive
      (if(member letter (map car(get-system-drive system)))
         (display "ya existe una drive con esa letra \n")
         (make-system (get-system-name system)
                      (cons(make-drive letter name capacity) ;;make-drive= lista que recibe 3 cosas, y le agrega algo adelante de 3 cosas
                      (get-system-drive system)) ;cadr de la lista system, system =lista
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))



#|FUNCION SWITCH DRIVE
DOMINIO:system X letter (char)
RECORRIDO: system
DESCRIPCION: Permite fijar la unidad en la que el usuario
realizará acciones. La función solo debe funcionar cuando hay un usuario con sesión
iniciada en el sistema
RECURSION: no sé|#

(define switch-drive
  (lambda(system)
    (lambda(letter)
      (if(and(is-char letter)(member letter (map car(get-system-drive system)))) ;no necesito que la letra sea un string
         (make-system (get-system-name system)
                      (get-system-drive system) ;cadr de la lista system, system =lista
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (if (null?(get-system-drive-seleccionado system))
                                (cons letter(get-system-drive-seleccionado system)) ;verdaderp
                                (if(equal? letter (car(get-system-drive-seleccionado system))) ;falso
                                   (get-system-drive-seleccionado system) ;v
                                   ((remove (car(get-system-drive-seleccionado system))) (cons letter(get-system-drive-seleccionado system))))) ;f
                       (get-system-ruta system)); el ultimo elemento de system, cierra make system
         (display "No se puede seleccionar ese drive porque no existe\n"))))) ;se supone que si no existe la letra en la lista no puede iniciar nada
;pero creo que falta que agregar que debe haber tambien un usuario iniciado, componer esa funciones.

;capa pertenencia verificar si letter es char
         

;ejemplos
(define S0 (system "newSystem"))
S0

(define S1 ((run S0 add-drive) #\C "SO" 1000))
;(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S2 ((run S1 add-drive) #\D "Util" 2000))
S1
;S2
S2

;(define S11 ((run S2 switch-drive) #\K)) si no hubiera dispplay funcionaria, o sea, funciona pero tira error y no deja avanzar a las que siguen
(define S12 ((run S2 switch-drive) #\C))
S12
(define S13 ((run S12 switch-drive) #\D))
S13

(provide (all-defined-out))