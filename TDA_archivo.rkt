#lang racket
;TDA ARCHIVO/CARPETA
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
(require "tda_usuario.rkt")

;capa constructora, paso 1 para crear el camino
(define (carpeta string)
    (if(is-string string)
       (make-carpeta null null null null null null)
       #f))

(define (files string)
  (list string null null))

;IMPORTANTE
;(string #\D)

;capa selectora
(define get-posicion (lambda(system) (car(car(car(cdr(cdr(cdr(cdr(cdr system))))))))))
(define get-my-string-posicion (lambda (system) (car(car(get-system-ruta system))))) ;obtiene el string de la ultima ruta utilizada
(define get-files (lambda (system) (car(cdr(cdr(car(get-system-ruta system)))))))
(define get-carpetas (lambda (system) (car(cdr(car(get-system-ruta system))))))
#|FUNCION 9 md
DOMINIO: system X name (String) 
RECORRIDO: SYSTEM
RECURSION: no sé
DESCRIPCION:  función que permite crear directorio dentro de una unidad a partir del nombre especificado.
Internamente la función añade datos relativos a usuario creador, fecha de creación, fecha de última
modificación y atributos de seguridad como los señalados en el enunciado general|#

(define md
  (lambda(system)
    (lambda(nombre)
      (if(and(is-string nombre)(not(member nombre (map cadr (get-system-ruta system))))) ;si el nombre es string y no existe aun en la lista de carpetas, lo deja crearse
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (cons(make-carpeta (get-posicion system) nombre (get-system-usuario-conectado system)
                                        '()) (get-system-ruta system)))
         
         (make-system(get-system-name system);falso
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))));que devuelva lo mismo en caso de que no cumpla

#|FUNCION 10 CD
DOMINIO: system X path or folderName (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION: función que permite cambiar la ruta (path) donde se realizarán operaciones.
cd permite cambiarse a un directorio especificado a partir de la ruta señalada en un String.
Además, contará con con los comodines especiales “/” que permitirán
regresar a la carpeta del nivel anterior siguiendo la ruta actual del usuario y volver a la raíz de
la unidad respectivamente.|#

(define cd
  (lambda(system)
    (lambda (path)
      (if (is-string path)
          (if(and(member path (map cadr(get-system-ruta system)))(not(member path (string-split(get-posicion system)"/"))))
             (make-system(get-system-name system) ;si existe la carpeta y no está abierto en la ruta, le deja entrar, recupera los archivos que tenia por ultima vez
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-append (get-posicion system) path "/")
                                            null
                                            (get-system-usuario-conectado system)
                                            (rec-archivos path system))
                              (get-system-ruta system)))
             (if (equal? path "..") ;vuelve a carpeta anterior
                 (make-system(get-system-name system)
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (string-append(string-join(reverse(cdr(reverse(string-split (get-posicion system) "/"))))"/")"/")
                                                null
                                                (get-system-usuario-conectado system)
                                                (rec-archivos path system))
                                  (get-system-ruta system)))
                    
                 (if(equal? path "/") ;vuelve a raiz
                    (make-system(get-system-name system)
                                (get-system-drive system)
                                (get-system-usuarios system)
                                (get-system-usuario-conectado system)
                                (get-system-drive-seleccionado system)
                                (cons(make-carpeta (substring (car(car(get-system-ruta system))) 0 3)
                                                   null
                                                   (get-system-usuario-conectado system)
                                                   (rec-archivos path system))
                                     (get-system-ruta system)))

                    (if(member (car(reverse(string-split path "/")))(map cadr(get-system-ruta system)))  ;cuando recibe ruta c;/apsdk
                       (make-system(get-system-name system)
                                   (get-system-drive system)
                                   (get-system-usuarios system)
                                   (get-system-usuario-conectado system)
                                   (car(string-split path "/"))
                                   (cons(make-carpeta path
                                                      null
                                                      (get-system-usuario-conectado system)
                                                      (rec-archivos path system)) ;AQUI ARCHIVOS
                              (get-system-ruta system)))

                    (make-system(get-system-name system)
                                (get-system-drive system)
                                (get-system-usuarios system)
                                (get-system-usuario-conectado system)
                                (get-system-drive-seleccionado system)
                                (get-system-ruta system))))))
                       
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
;le puse para que todos recuperen sus respectivos archivos            

#|FUNCION 11  TDA system - add-file
DOMINIO:system X file
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función que permite añadir un archivo en la ruta actual.
|#
(define add-file
  (lambda(system)
    (lambda(file)
      (if (map is-string (list file))
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (cons(make-carpeta (get-posicion system)
                                         (car(reverse(string-split(get-posicion system)"/"))) ;DUEÑO
                                         (get-system-usuario-conectado system)
                                         (cons file (get-files system)))           
                           (get-system-ruta system)))
          
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
      
      
      
;capa constructora crea la list que contendrá los elementos del file
(define (make-file nombre ext cont rest) ;rest = atr1 atr2
  (list nombre ext cont rest))

;capa constructora, crea una variable del mismo nombre que lo que entra en la funcion para que lo que entregue se transforme en lista y sea trabajado como lista
(define file ;como makedrive
  (lambda (nombre ext cont . rest)
    (make-file nombre ext cont rest)))
    
#|TDA FUNCION 12 DEL
DOMINIO:system X fileName or fileNamePattern (string)
RECORRIDO: system
RECURSION: NO C
DESCRIPCION: función para eliminar un archivo o varios archivos en base a un patrón determinado.
Esta versión también puede eliminar una carpeta completa con todos sus subdirectorios.
El contenido eliminado se va a la papelera.
|#
;(equal? (car(string->list "foo1.txt")) #\f)

(define del
  (lambda (system)
    (lambda (filename)
      (if(is-string filename)
         (if (or(equal? (car(string-split filename "*")) ".txt")(equal? (car(string-split filename "*")) ".docx"))

             (make-system(get-system-name system) ;si lo que entra corresponde a lo de arriba, borra los que tienen lo mismo
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (get-posicion system)
                                            (car(reverse(string-split(get-posicion system)"/")))
                                            (get-system-usuario-conectado system)
                                            (remove-extension (car (string-split (car(string-split filename "*")) ".")) system))           
                              (get-system-ruta system)))
             
             (if (member (car(string-split filename "*")) (map string(map car(map string->list (map car(get-files system)))))) ;si al separarlos por * tiene la misma letra inicial

                 (make-system(get-system-name system) 
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (get-posicion system)
                                                (car(reverse(string-split(get-posicion system)"/")))
                                                (get-system-usuario-conectado system)
                                                (letter-titles filename system))           
                                  (get-system-ruta system)))
                 
                 (if (member filename (map car(get-files system))) ;borra el ARCHIVO que tiene ese titulo
                     (make-system(get-system-name system)
                                 (get-system-drive system)
                                 (get-system-usuarios system)
                                 (get-system-usuario-conectado system)
                                 (get-system-drive-seleccionado system)
                                 (cons(make-carpeta (get-posicion system)
                                                    (car(reverse(string-split(get-posicion system)"/")))
                                                    (get-system-usuario-conectado system)
                                                    (remove-titulo filename system))           
                                      (get-system-ruta system)))
                     
                     (if (member filename (map cadr(get-system-ruta system))) ;borra LA CARPETA SI ES QUE EXISTE folder, se devuelve uno al borrar 
                         (make-system(get-system-name system)
                                     (get-system-drive system)
                                     (get-system-usuarios system)
                                     (get-system-usuario-conectado system)
                                     (get-system-drive-seleccionado system)
                                     (remove-posicion filename system))
                         
                         (if(equal? "*.*" filename) ;borra todos los archivos DOC TXT
                            (make-system(get-system-name system)
                                        (get-system-drive system)
                                        (get-system-usuarios system)
                                        (get-system-usuario-conectado system)
                                        (get-system-drive-seleccionado system)
                                        (cons(make-carpeta (get-posicion system)
                                                           (car(reverse(string-split(get-posicion system)"/")))
                                                           (get-system-usuario-conectado system)
                                                           '())           
                                             (get-system-ruta system)))
                         
                         (make-system(get-system-name system)
                                     (get-system-drive system)
                                     (get-system-usuarios system)
                                     (get-system-usuario-conectado system)
                                     (get-system-drive-seleccionado system)
                                     (get-system-ruta system)))))))  ;agregar el normal
         
         (make-system(get-system-name system)
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))))


#|FUNCION 13 rd
DOMINIO:system X folderName or folderPath (string)
RECORRIDO: system
RECURSION: no obigatoria asi que no
DESCRIPCION:  función para eliminar una carpeta, siempre y cuando ésta esté vacía.
Una carpeta se puede eliminar estando posicionado fuera de ésta.|#

(define rd
  (lambda (system)
    (lambda(foldername)
      (if (and(is-string foldername)(member foldername (map cadr (get-system-ruta system))))
          (if(not(null? (rec-archivos foldername system))) ;si es vacia la union del dueño y los archivos no se hace nada
             (make-system(get-system-name system)
                         (get-system-drive system) ;si no es vacia la ultima version, lo mantiene
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (get-system-ruta system)) 
             
                 (make-system(get-system-name system) ;si es vacia la ultima version, lo ELIMINA
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (remove-posicion foldername system)))
          
          (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                      (get-system-drive system) 
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
                 
                 #|(make-system(get-system-name system) ;si no son nulos los archivos, no se puede borrar la carpeta
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (get-system-ruta system))))
          
          (make-system(get-system-name system) ;si no son nulos los archivos, no se puede borrar la carpeta
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))|#
             
 #|            ;HAY QUE HACERLE MAP APRA QUE FUNCIONE (member foldername (string-split (get-posicion system)"/")) 
          ;si el nombre es string, existe la carpeta en el sistema, es miembro de la ruta actual y es nulo
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (remove-posicion foldername system))
          
          (make-system(get-system-name system);en parte funciona pq no borra folder1 cuando tiene archivos
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))|#
          

#|FUNCION 14
DOMINIO: system X source (file or folder) (String) x target path (String)
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función para copiar un archivo o carpeta desde una ruta origen a una ruta destino.|#













#|FUNCION 18 FORMAT
DOMINIO:system X letter (char) X name (String)
RECORRIDO: system 
RECURSION: NO
DESCRIPCION:función para formatear una unidad dada su letra, lo que borra todo su contenido,
además de indicar nuevo nombre, pero conservando capacidad.
|#

(define format
  (lambda (system)
    (lambda(letter nombre)
      (if(and(is-char letter)(is-string nombre))
         (capacidad letter system nombre)
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (string-downcase (car(car(get-system-ruta system)))))))))



;capa selectora, la pude hacer en la funcion pero me costo el filter. saca los elementos de la lista que tienen la misma primera letra en el primer elemento
(define (remove letter system)
      (filter (lambda(x)(if (not(equal?(string-append(string letter)":") (car(string-split (car x) "/"))))
                            #t
                            #f))
              (get-system-ruta system)))


;capa selectora elimina los drives que tengan la misma letra de primer elemento que la letra que entra, lo hace en un lugar diferente que el de arriba
(define (remove2 letter system) 
      (filter (lambda(x)(if (not(equal? letter  (car x)))
                            #t
                            #f))
              (get-system-drive system)))

;CAPA SELECTORA REMOVE 3, HARA LO MISMO QUE REMOVE 2 SOLO QUE CON OTRA PARTE DEL SYSTEM
(define (remove-extension filename system) 
      (filter (lambda(x) (if (not(equal? filename (cadr x)))
                            #t
                            #f))
              (get-files system)))
  
;capa selectora, REMUEVE LOS QUE TENGAN EL MISMO TITULO, ME PERDI CON TANTO FILTRO PERO ALGUNO DEBE SER PARECIDO
(define (remove-titulo filename system) 
      (filter (lambda(x) (if (not(equal? filename (car x)))
                            #t
                            #f))
              (get-files system)))


;Capa selectora elimina todas las existencias de la carpeta de la ruta (el string / /)
(define (remove-posicion file system)
  (filter (lambda(x) (if (not(or(equal? file (car(reverse(string-split (car x) "/")))) (equal? file (cadr x))))
                         #t
                         #f))
          (get-system-ruta system))) ;FUNCIONA


;CAPA SELECTORA, necesito que me haga lista de los titulos de cada file
(define (letter-titles filename system) 
   (filter (lambda(x) (if(not(and(equal? (car(string-split filename "*")) (string(car(string->list(car x)))))
                                 (equal? (car(string-split (cadr(string-split filename "*")) ".")) (car(cdr x)))))
                    #t
                   #f))
       (get-files system)))

;capa selectora, trata de recuperar los archivos que habia dentro de una carpeta
(define (rec-archivos path system)
  (if(null? (filter(lambda(x) (if(equal? path (cadr x))
                                 #t
                                 #f))
                   (get-system-ruta system)))
     null
     (car(cdr(cdr(car(filter (lambda(x) (if(equal? path (cadr x))                     
                                           #t
                                           #f))
                             (get-system-ruta system))))))))


    

;capa selectora, encuentra el drive con la misma letra para sacar propiedades/capacidad
(define (no-remove letter system)
      (filter (lambda(x)(if (equal? letter (car x))
                            #t
                            #f))
              (get-system-drive system)))


;capa modificadora obtener capacidad
(define (capacidad letter system nombre)
  (if (and(is-string nombre)(is-char letter))
      (if (member letter (map car(get-system-drive system)))
          ;(map caddr (get-system-drive S30)) capacidades
          (make-system(get-system-name system);verdadero
                      (cons(make-drive letter nombre (car (cdr (cdr (car(no-remove letter system))))))(remove2 letter system))
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (remove letter system))
          #f)
      #f))
              



;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2")) ;funciona
(define S16 ((run S15 md) "folder3"))

;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2")) ;funciona

;crea subcarpeta folder21 dentro de folder2 (incluye caso S19 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21")) ;funciona
(define S19 ((run S18 md) "folder21")) ;cumple

;;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S21
(define S20 ((run S19 cd) "folder21")) ;funciona
(define S21 ((run S20 cd) "folder22")) ;funciona 16-04-23

;;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))  ;funciona 16-04-23

;;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21")) ;segun yo si deberia poder a entrar, pues la carpeta no se ha eliminado. Agregar "/"

;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211")) ;funciona 16-04-23
(define S25 ((run S24 cd) "folder211")) ;funciona 16-04-23

;vuelve a la raíz de la unidad c:/
(define S26 ((run S25 cd) "/")) ;funciona 16-04-23
      
#|S13
S14
S15
S16
S17
S18
S19
S20
S21
S22
S23
S24
S25
S26 ;todo bem|#

    
;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D)) ;FUNCIONA

;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5")) ;FUNCIONA
(define S29 ((run S28 cd) "folder5")) ;FUNCIONA

;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/")) 

;formateando drive D:
(define S31 ((run S30 format) #\D "newD")) ;FUNCIONAAAA 17-04-2023
      
;añadiendo archivos
(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1"))) ;funciona 17-04 
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2"))) ;funciona 17-04 
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3"))) ;funciona 17-04 
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;funciona 17-04 , no me agrada la lista
;con atributos de seguridad oculto (h) y de solo lectura (r)

;eliminando archivos
(define S36 ((run S35 del) "*.txt")) ;borra todos los txt ;FUNCIONA 19-04
(define S37 ((run S35 del) "f*.docx")) ;borra los que comienzan con f y tengan extension .doc BORRA 1 ;FUNCIONA 19-04
(define S38 ((run S35 del) "goo4.docx")) ;borra el que tiene ese titulo ;FUNCIONA 19-04
(define S39 ((run S35 cd) ".."))
;(define S39a ((run S39 cd) "folder1"));FUNCIONA 19-04
(define S40 ((run S35 del) "folder1")) ;borra la carpeta con todo FUNCIONA 19-04 -20

;borrando una carpeta
(define S41 ((run S39 rd) "folder1"))  ;no debería borrarla, pues tiene archivos, funciona 20-04, devuelve c pq en s39 me sali de folder1
(define S42 ((run S41 cd) "folder1")) ;VUELVO A ENTRAR EN LA CARPETA Y RECUPERO LOS ARCHIVOS
(define S43 ((run S42 del) "*.*"));borra todos los archivos FUNCIONA LO DEJA PELADO LA PARTE DE LOS ARCHIVOS
(define S44 ((run S43 cd) ".."))
(define S45 ((run S44 rd) "folder1")) ;FUNCIONA 20-04




      
#|S27
S28
S29
S30
S31
S32
S33
S34
S35
S36
S37
S38
S39
S40
S41
S42
S43
S44
S45|#

;(make-carpeta (string-append (string(car(get-system-drive-seleccionado system)))":/") (append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system))) (get-system-usuario-conectado system))
;                         (get-system-ruta system))) ;si está en el resto de lugares no se agrega
                         ;(append(crear-ruta path)(filter (lambda (x) (eq? (cadr x) path))(get-system-ruta system)))))

(provide (all-defined-out))