#lang racket
(require "tda_system_21168603_OyanedelAlvarez.rkt")
(require "tda_user_21168603_OyanedelAlvarez.rkt")


;tda drive

;CAPA CONSTRUCTORA
#|make-drive
DOMINIO:letter (char) x name (string) x capacity (integer)
RECORRIDO: lista de los elementos que componene un drive o simplemente el drive
DESCRIPCION: crea el drive para agregarlo al sistema
RECURSION: no
|#
(define (make-drive letter name capacity)
  (list letter name capacity)) ;CAMBIAR

;CAPA DE PERTENENCIA
#|is-char
DOMINIO: letter (char)
RECORRIDO: bool
DESCRIPCION: verifica si lo que entra es un char, antes poseia display y daba sentido a su existencia
pero la verdad display no funciona CAMBIAR A DRIVE
RECURSION: no
|#
(define (is-char letter)
  (if(char? letter)
     #t ;"se ha creado el sistema\n"
     #f))

;CAPA MODIFICADORA

#|capacidad
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: funcion que crea el drive con nombre cambiado y utilizando la funcion no-remove para poder
 obtener la capacidad del drive previo, crea el nuevo sistema con el cambio en los drives|#

(define (set-capacidad letter system nombre) ;tda drive
  (if (and(is-string nombre)(is-char letter))
      (if (member letter (map car(get-system-drive system)))
          (make-system(get-system-name system);verdadero
                      (cons(make-drive letter nombre (car (cdr (cdr (car(get-no-remove letter system))))))(get-remove2 letter system))
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-remove1 letter system))
          #f)
      #f))

;CAPA SELECTORA

#|get-system-drive
DOMINIO: depende de la lista en que se aplique
RECORRIDO: drives del sistema
DESCRIPCION: obtiene y muestra una lista con los drives que posee el sistema, la letra,
el nombre y la capacidad de cada uno
RECURSION: no
|#
(define get-system-drive cadr)

#|get-system-drive-seleccionado
DOMINIO: system
RECORRIDO: drive seleccionado
DESCRIPCION: muestra la letra del drive que ha sido seleccionado para trabajar
RECURSION: no
|#
(define(get-system-drive-seleccionado system)
  (caddr (reverse system)))

#|get-remove1 
DOMINIO: letter (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene y remueve elementos de la lista que tienen la primera letra igual a la primera letra de la ruta actual
tipo entra c y la ruta es c/folder1/, cumple|#

(define (get-remove1 letter system)
      (filter (lambda(x)(if (not(equal?(string-append(string letter)":") (car(string-split (car x) "/"))))
                            #t
                            #f))
              (get-system-ruta system)))

#|get-no-remove
DOMINIO: filename (string) X system
RECORRIDO: get-system-drive
RECURSION: no
DESCRIPCION: obtiene la lista de drive que tiene a letter (las propiedades del drive letter) para asi poder obtener
su capacidad. Sirve para la funcion de renombrar drive|#
(define (get-no-remove letter system) 
      (filter (lambda(x)(if (equal? letter (car x))
                            #t
                            #f))
              (get-system-drive system)))

#|get-remove2 
DOMINIO: letter (string) X system
RECORRIDO: get-system-drive
RECURSION: no
DESCRIPCION: obtiene y remueve drives que tengan la misma letra que letter entrante
lo ejecuta en un lugar distinto a remove1|#

(define (get-remove2 letter system) 
      (filter (lambda(x)(if (not(equal? letter  (car x)))
                            #t
                            #f))
              (get-system-drive system)))

(provide (all-defined-out))