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


#|FUNCION ADD-DRIVE seguro hay que cambiarlo a otro archivo
DOMINIO: system x/dominio del primer lambda
         letter(char) x name(string) x capacity(int) /este se refiere a el dominio de la currificacion
RECORRIDO: system
DESCRIPCION: Función que permite añadir una unidad a un
sistema. La letra de la unidad es única.
|#








;ejemplos
(define S0 (system "newSystem"))
(define S1 (make-system "newSystem" "ewe" "ISIDORA"))
S1
S0


(define S2 ((run S0 add-drive) #\C "SO" 1000))
(define S3 ((run S1 add-drive) #\C "SO1" 3000))
(define S4 ((run S2 add-drive) #\D "Util" 2000))
