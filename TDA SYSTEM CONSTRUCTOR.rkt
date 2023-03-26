#lang racket
;Isidora Oyanedel
#|TDA System - CONSTRUCTOR
-----------descripcion del TDA--------------
Función constructora de un sistema. Deja registro de la fecha de creación.

          --- CONSTRUCTOR ---
DOMINIO: recibe un string, que supongo será el nombre del nuevo sistema
RECORRIDO: regresa un system 
DESCRIPCION: se ingresa un string en la funcion devolviendo un system
             que se identifica con el string ingresado
USO DE RECURSION: no se usa.

probablemente se usa el TDA FECHA
|#

;(lambda (fecha) (list fecha))
;(define (nuevo string)
; (lambda (fecha)
;    (lambda (fecha) (list fecha))))
;en este intenté usar calculo lambda pero no corresponde
;con el script pq hay que entregarle dos cosas

(define (system string) 
  (list string 16042023))
;en este recibe solo el nombre y coloca una fecha predeterminada en el codigo
; para que funcione: (system "hola")

;(define (system . string) string)


