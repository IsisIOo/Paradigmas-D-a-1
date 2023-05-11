#lang racket
;tda system

;CAPA CONSTRUCTORA

#|make-system
DOMINIO: string (nombre) x string(usuario) x drive (make drive char x string x int) x string (usuario conectado) x char (drive solicitado)
x ruta (ruta del sistema representado en make-carpeta, compuesto de otros elementos)
RECORRIDO: lista con los elementos escenciales del sistema, ademas del tiempo actual o simplemente el sistema
DESCRIPCION: Se crea la lista que contiene los elementos principales del sistema
RECURSION: no
|#
(define (make-system nombre drive usuario usuario-conectado drive-solicitado rutaa)
  (list nombre drive usuario usuario-conectado drive-solicitado rutaa (current-seconds)))

#|make-carpeta
DOMINIO: ruta (string) x string(carpeta agregada o dueño de archivos) x user (usuario conectado actualmente) x file (lista de archivos de la carpeta) x password (string cuando una carpeta o archivo ha sido encriptado)
RECORRIDO: lista con los elementos escenciales para crear una carpeta junto con el tiempo actual o simplemente un carpeta
DESCRIPCION: se crea la estructura de una carpeta
RECURSION: no
|#

(define (make-carpeta ruta string user file password) ;el primer elemento es la ruta con la letra, segundo elemento lo ultimo agregado, tercer user, 4to documento 5to, tipo de doc, usuario
  (list ruta string file user password (current-seconds))) ;este current seconds es la fecha de modificacion, aunque no se como mantenerlo fijo
;LA PUSE AQUI PORQUE PROVOCABA DEPENDENCIA CIRCULAR


;CAPA PERTENECIA

#|is-string
DOMINIO: string
RECORRIDO: bool
DESCRIPCION: verifica si lo que entra es un string, antes poseia display y daba sentido a su existencia
pero la verdad display no era necesario
RECURSION: no
|#
(define (is-string string)
  (if(string? string)
     #t ;"se ha creado el sistema\n"
     #f))

;CAPA SELECTORA

#|get-system-name
DOMINIO: depende de la lista en que se aplique
RECORRIDO: nombre del sistema
DESCRIPCION: entrega el nombre del sistema en que se trabaja, se suele utilizar unido a system
RECURSION: no
|#
(define get-system-name car)

#|get-system-ruta
DOMINIO: system
RECORRIDO: ruta del sistema
RECURSION: no
DESCRIPCION: recupera todos los cambios en el stack |#

(define(get-system-ruta system) ;aqui por dependencia circular
  (cadr (reverse system)))

#|get-posicion
DOMINIO: system
RECORRIDO: ruta actual
RECURSION: no
DESCRIPCION: obtiene la ruta mas actual del sistema que tiene de entrada|#
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system))))))))))

(provide (all-defined-out))