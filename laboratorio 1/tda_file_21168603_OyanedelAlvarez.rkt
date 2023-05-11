#lang racket
(require "tda_system_21168603_OyanedelAlvarez.rkt")
(require "tda_folder_21168603_OyanedelAlvarez.rkt")

;CAPA CONSTRUCTORA

#|make-file
DOMINIO: system
RECORRIDO: archivos actuales en la carpeta abierta, ultima actualizacion 
RECURSION: no
DESCRIPCION: crea la lista que contendrá los elementos del file (funcion add-file), donde rest equivale
a los atributos que pueda tener el archivo (oculto, solo lectura)|#

(define (make-file nombre ext cont rest) ;rest = atr1 atr2
  (list nombre ext cont rest)) 

#|file
DOMINIO: file (lista de strings)
RECORRIDO: make-file file
RECURSION: no
DESCRIPCION: crea una variante con el mismo nombre file para que lo que la lista que se crea aqui sea recibido por la
funcion add-file y trabaje con ella|#
(define file ;como makedrive
  (lambda (nombre ext cont . rest)
    (make-file nombre ext cont rest)))


;CAPA MODIFICADORA

#|set-cambia-nombre-archivo
DOMINIO: system X nombre(string) X nuevo nombre (string)
RECORRIDO: archivo llamado nombre renombrado como nuevo nombre
RECURSION: no
DESCRIPCION: remueve el nombre previo del archivo y luego de hacerlo le agrega nuevo nombre a la lista del archivo|#
(define (set-cambia-nombre-archivo system name new-name) ;tda archivo
  (append(get-remove-titulo2 name system) (list(cons new-name (remove name (car(get-no-remove-archivo2 name system)))))))

#|set-eliminar_archivo
dominio: source x system
recorrido:system
recursion: de cola
descripcion: elimina el archivo original que ha sido movido a otro drive o carpeta|#
(define set-eliminar_archivo
  (lambda(system source)
    (define DelFile
      (lambda(lista vacio) ;tdaarchivo
        (if(null? lista)
           vacio
           (if (null? (caddr(car lista)))
               (DelFile (cdr lista) (append vacio (list (car lista))))
               (if(and(member source (map car(caddr (car lista))))(equal? (car(string-split(get-posicion system)"/")) (car(string-split (car(car lista)) "/"))))
                  (DelFile (cdr lista) (append vacio (list(cons (car (car lista))
                                                           (cons (cadr (car lista))
                                                                 (cons (filter(lambda (x) (if (not(equal? source (car x)))
                                                                                              #t
                                                                                              #f))(caddr(car lista)))
                                                                       (cons (cadddr(car lista))
                                                                             (cons (cadddr(cdr (car lista))) '()))))))))
                  (DelFile (cdr lista) (append vacio (list (car lista)))))))))
      (DelFile (get-system-ruta system) '())))


#|set-encriptar-t
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION:Funcion que encripta el archivo solicitado de una carpeta|#
(define set-encriptar-t  ;tda archivo
  (lambda(system path fn1) ;fn1=plus one fn2=minus-one
    (define encriptar3
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(equal? path  (car(car lista)))
              (encriptar3 (cdr lista) (append vacio (list(cons (string-append(fn1(car(string-split(car(car lista))".")))"." (cadr(car lista)))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (fn1(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar3 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar3 (get-rec-archivos (get-carpetas system) system) '())))


#|set-desencriptar-t
DOMINIO: path (string) X system
RECORRIDO: system archivos
RECURSION: de cola
DESCRIPCION: Funcion que encuentra el archivo encriptado y lo desencripta|#
(define set-desencriptar-t 
  (lambda(system path fn1 fn2) ;tda archivo ;fn1=plus one fn2=minus-one
    (define encriptar4
      (lambda(lista vacio)
        (if(null? lista)
           vacio
           (if(equal? (string-append(fn1(car(string-split path ".")))"."(car(cdr(string-split path "."))))  (car(car lista)))
              (encriptar4 (cdr lista) (append vacio (list(cons (string-append(fn2(car(string-split(car(car lista))".")))"." (cadr(car lista)))
                                                               (cons (cadr (car lista)) 
                                                                     (cons (fn2(caddr(car lista)))
                                                                           (cons (cadddr(car lista))'())))))))
                                                                                 
              (encriptar4 (cdr lista) (append vacio (list (car lista))))))))
    (encriptar4 (get-rec-archivos (get-carpetas system) system) '())))

;CAPA SELECTORA

#|get-files
DOMINIO: system
RECORRIDO: archivos actuales en la carpeta abierta, ultima actualizacion 
RECURSION: no
DESCRIPCION: obtiene los archivos mas actuales de acuerdo al sistema|#
(define get-files (lambda (system) (car(cdr(cdr(car(get-system-ruta system))))))) ;tda archivo

#|get-remove-titulo2 
DOMINIO: nombre del archivo (string) X system
RECORRIDO: system 
RECURSION: no
DESCRIPCION: obtiene la lista de archivos sin el elemento del cual se desea cambiar el nombre
al combinarla con cambia-nombre-archivo logra insertar el archivo con nombre cambiado al sistema|#
(define (get-remove-titulo2 filename system)  ;tda archivo
      (filter (lambda(x) (if (not(equal? filename (car x)))
                            #t
                            #f))
              (caddr(get-recuperar-ruta system))))

#|get-no-remove-archivo2
DOMINIO: nombre del archivo (string) X system
RECORRIDO: system
RECURSION: no
DESCRIPCION: obtiene la lista que contiene el nombre del archivo, al obtenerla se le puede remover el nombre
la diferencia con no-remove-archivo1 es que trabajan en distintas partes, este necesita recuperar la ultima actualizacion
de la ruta de acuerdo al drive en que estamos.|#

(define (get-no-remove-archivo2 file system) ;tdaarchivo
      (filter (lambda(x)(if (equal? file (car x))
                            #t
                            #f))
              (caddr(get-recuperar-ruta system))))


#|remove-extension 
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene y remueve los archivos que tengan la misma extension que la entrante
se ejecuta en los archivos actuales|#

(define (get-remove-extension filename system) ;tda archivo
      (filter (lambda(x) (if (not(equal? filename (cadr x)))
                            #t
                            #f))
              (get-files system)))

#|get-remove-titulo 
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene y remueve los archivos que tengan el mismo nombre que el filename entrante
se ejecuta en los archivos actuales|#

(define (remove-titulo filename system) ;tda archivo
      (filter (lambda(x) (if (not(equal? filename (car x)))
                            #t
                            #f))
              (get-files system)))

#|get-letter-titles
DOMINIO: filename (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: remueve los elementos que tengan la misma letra inicial de nombre del archivo y
tengan una extension de archivo igual a la entrante |#

(define (get-letter-titles filename system)   ;tda archivo
   (filter (lambda(x) (if(not(and(equal? (car(string-split filename "*")) (string(car(string->list(car x)))))
                                 (equal? (car(string-split (cadr(string-split filename "*")) ".")) (car(cdr x)))))
                    #t
                   #f))
       (get-files system)))

#|get-no-remove-archivo
DOMINIO: file (string) X system
RECORRIDO: get-files
RECURSION: no
DESCRIPCION: obtiene las propiedades del archivo file en su estado mas actual para asi poder moverlo a otra ubicacion
Sirve para la funcion move y copy.|#
(define (get-no-remove-archivo file system) ;tda archivo
      (filter (lambda(x)(if (equal? file (car x))
                            #t
                            #f))
              (get-files system)))

(provide (all-defined-out))