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




(define S0 (system "newSystem"))
(define S1 (make-system "newSystem" "ewe" "ISIDORA"))
S1
S0


(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))
