#lang racket
(require "tda_system_21168603_OyanedelAlvarez.rkt")
;CAPA CONSTRUCTORA

#|make-user
DOMINIO: string (string)
RECORRIDO: usuario
DESCRIPCION: crea un usuario (lista de un elemento) comprobando si cumple la condicion de ser un string
RECURSION: no
|#
(define (make-user string);se supone que crea el usuario
  (if(is-string string)
     (list string)
     #f)) ;revisa si es string


;CAPA DE PERTENENCIA

#|is-member
DOMINIO: string (string) x system (system)
RECORRIDO: bool
DESCRIPCION: verifica si está el usuario en lista de usuarios
RECURSION: no
|#
(define (is-member string system)
  (member string (map car(get-system-usuarios system))))



#|is-status-user
DOMINIO: string (string)
RECORRIDO: lista de un elemento
DESCRIPCION: recrea un usuario pero en este caso, deja una lista vacia si no se cumple
RECURSION: no
|#
(define (is-status-user string)
  (if(is-string string)
     string
     null))


;CAPA SELECTORA

#|get-system-usuarios
DOMINIO: depende de la lista en que se aplique
RECORRIDO: lista de usuarios registrados
DESCRIPCION: entrega los usuarios que han sido correctamente registrados en el sistema
RECURSION: no
|#
(define get-system-usuarios caddr)


#|get-system-usuario-conectado
DOMINIO: depende de la lista en que se aplique 
RECORRIDO: usuario conectado
DESCRIPCION: entrega el usuario que esté conectado//logueado actualmente en el sistema
RECURSION: no
|#
(define get-system-usuario-conectado cadddr)

(provide (all-defined-out))