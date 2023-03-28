#lang racket
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

|#

(define (register user) ;se supone que registra el usuario
  (list user))       ;registra el usuario en una lista, pero deberia crear una funcion que tome la lista
                     ;formada aqui y haga una lista de listas

#|FUNCION PARA VERIFICAR SI EL USUARIO SE REGISTRÓ
Pertenencia: verifica si el usuario ya existia o no
DESCRIPCION DE LA FUNCION: funcion ligada a la no duplicidad de nombres de usuario
dominio: lista
recorrido: boolean
|#

(define (is-user? user)
  (if(list? user)
     (if(string?(car user))
        #t   ;if
        #f) ;else
     #f))



(define user0 (register "isi")) ;prueba del registro
(is-user? user0) ;prueba de si es user

(define user1 (register "thomi"))
(is-user? user1)



