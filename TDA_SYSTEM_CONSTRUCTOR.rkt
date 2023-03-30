#lang racket
;Isidora Oyanedel
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
  (make-system string null null))  ;el primer elemento es el nombre, siguiente drive, sig usuario


(define (make-system nombre drive usuario)
  (list nombre drive usuario))

#|FUNCION RUN

DOMINIO: System X Command (funcion =command)
RECORRIDO: system
DESCRIPCION: Función que permite ejecutar un comando (función)
sobre un sistema. Toda acción realizada con run relativa a creación de archivos,
carpetas, renombrar, copiar, mover, eliminar, debe dejar un registro de la fecha de
modificación, además de verificar los permisos del recurso que será alterado|#

(define (run system command) ;se aplica una funcion en la lista system por ejemplo add-rive
  (command system))

#|Otras funciones necesarias|#
(define get-system-name car)
(define get-system-drive cadr)
(define get-system-usuarios caddr)

#|FUNCION ADD-DRIVE seguro hay que cambiarlo a otro archivo
DOMINIO: system x/dominio del primer lambda
         letter(char) x name(string) x capacity(int) /este se refiere a el dominio de la currificacion
RECORRIDO: system
DESCRIPCION: Función que permite añadir una unidad a un
sistema. La letra de la unidad es única.
RECURSION: no

|#

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
                      (get-system-usuarios system))))))








;ejemplos
(define S0 (system "newSystem"))
S0

(define S1 ((run S0 add-drive) #\C "SO1" 3000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S1 add-drive) #\D "Util" 2000))
;(define S4 ((run S3 add-drive) #\E "INutil" 2000))
S1
S2
;S3; chinga en S3, por alguna razon no funciona en los estados que ya me envío un mensaje

(provide (all-defined-out)) ;es para que las funciones de aqui se usen en otros