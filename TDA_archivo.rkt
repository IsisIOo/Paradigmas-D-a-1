#lang racket
;TDA ARCHIVO/CARPETA
(require "TDA_SYSTEM_CONSTRUCTOR.rkt")
(require "tda_usuario.rkt")
(require "funciones_archivos_carpetas.rkt")

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
      (if(and(is-string nombre)(not(member nombre (map cadr (get-system-ruta system))))(not(null? (get-system-usuario-conectado system)))) ;si el nombre es string y no existe aun en la lista de carpetas, lo deja crearse
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (cons(make-carpeta (string-downcase(get-posicion system))
                                        nombre
                                        (car(get-system-usuario-conectado system))
                                        '()
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
      (if (and(is-string path)(not(null? (get-system-usuario-conectado system))))
          (if(and(member (string-downcase path) (map cadr(get-system-ruta system)))(not(member (string-downcase path) (string-split(string-downcase(get-posicion system))"/"))))
             (make-system(get-system-name system) ;si existe la carpeta y no está abierto en la ruta, le deja entrar, recupera los archivos que tenia por ultima vez
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase(string-append (car(recuperar-ruta system)) path "/"))
                                            null
                                            (car(get-system-usuario-conectado system))
                                            (rec-archivos path system)
                                            '())
                              (get-system-ruta system)))
             
             (if (equal? path "..") ;vuelve a carpeta anterior
                 (make-system(get-system-name system)
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (string-downcase(string-append(string-join(reverse(cdr(reverse(string-split (get-posicion system) "/"))))"/")"/"))
                                                null
                                                (car(get-system-usuario-conectado system))
                                                (rec-archivos path system)
                                                '())
                                  (get-system-ruta system)))
                    
                 (if(equal? path "/") ;vuelve a raiz
                    (make-system(get-system-name system)
                                (get-system-drive system)
                                (get-system-usuarios system)
                                (get-system-usuario-conectado system)
                                (get-system-drive-seleccionado system)
                                (cons(make-carpeta (substring (car(car(get-system-ruta system))) 0 3)
                                                   null
                                                   (car(get-system-usuario-conectado system))
                                                   (rec-archivos path system)
                                                   '())
                                     (get-system-ruta system)))

                    (if(member (string-downcase(car(reverse(string-split path "/"))))(map cadr(get-system-ruta system)))  ;cuando recibe ruta c;/apsdk
                       (make-system(get-system-name system)
                                   (get-system-drive system)
                                   (get-system-usuarios system)
                                   (get-system-usuario-conectado system)
                                   (string->list(string-upcase(string(car(string->list(car(string-split path "/")))))))
                                   (cons(make-carpeta (string-downcase path)
                                                      null
                                                      (car(get-system-usuario-conectado system))
                                                      (rec-archivos path system)
                                                      '()) ;AQUI ARCHIVOS
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

#|FUNCION 11  TDA system - add-file
DOMINIO:system X file
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función que permite añadir un archivo en la ruta actual.
|#
(define add-file
  (lambda(system)
    (lambda(file)
      (if (and(map is-string (list file))(not(null? (get-system-usuario-conectado system))))
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (cons(make-carpeta (string-downcase(get-posicion system))
                                         (car(reverse(string-split(get-posicion system)"/"))) ;DUEÑO
                                         (car(get-system-usuario-conectado system))
                                         (cons file (get-files system))
                                         '())           
                           (get-system-ruta system)))
          
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
      
      
      
#|TDA FUNCION 12 DEL
DOMINIO:system X fileName or fileNamePattern (string)
RECORRIDO: system
RECURSION: NO C
DESCRIPCION: función para eliminar un archivo o varios archivos en base a un patrón determinado.
Esta versión también puede eliminar una carpeta completa con todos sus subdirectorios.
El contenido eliminado se va a la papelera.
|#

(define del
  (lambda (system)
    (lambda (filename)
      (if(and(is-string filename)(not(null? (get-system-usuario-conectado system))))
         (if (or(equal? (car(string-split filename "*")) ".txt")(equal? (car(string-split filename "*")) ".docx"))

             (make-system(get-system-name system) ;si lo que entra corresponde a lo de arriba, borra los que tienen lo mismo
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase(get-posicion system))
                                            (string-downcase(car(reverse(string-split(get-posicion system)"/"))))
                                            (car(get-system-usuario-conectado system))
                                            (remove-extension (car (string-split (car(string-split filename "*")) ".")) system)
                                            '())           
                              (get-system-ruta system)))
             
             (if (member (car(string-split filename "*")) (map string(map car(map string->list (map car(get-files system)))))) ;si al separarlos por * tiene la misma letra inicial

                 (make-system(get-system-name system) 
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (string-downcase(get-posicion system))
                                                (string-downcase(car(reverse(string-split(get-posicion system)"/"))))
                                                (car(get-system-usuario-conectado system))
                                                (letter-titles filename system)
                                                '())           
                                  (get-system-ruta system)))
                 
                 (if (member filename (map car(get-files system))) ;borra el ARCHIVO que tiene ese titulo
                     (make-system(get-system-name system)
                                 (get-system-drive system)
                                 (get-system-usuarios system)
                                 (get-system-usuario-conectado system)
                                 (get-system-drive-seleccionado system)
                                 (cons(make-carpeta (string-downcase(get-posicion system))
                                                    (car(reverse(string-split(get-posicion system)"/")))
                                                    (car(get-system-usuario-conectado system))
                                                    (remove-titulo filename system)
                                                    '())           
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
                                                           (car(get-system-usuario-conectado system))
                                                           '()
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
      (if (and(is-string foldername)(member foldername (map cadr (get-system-ruta system))) (not(null? (get-system-usuario-conectado system))))
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

#|FUNCION 14 copy
DOMINIO: system X source (file or folder) (String) x target path (String)
RECORRIDO: system
RECURSION: NOC
DESCRIPCION:función para copiar un archivo o carpeta desde una ruta origen a una ruta destino.|#

(define copy ; no debe eliminar los archivos
  (lambda(system)
    (lambda(source target)
      ;AGREGAR QUE SE PUEDE HACER SOLO SI LA ULTIMA POSICION DE LOS ARCHIVOS NO SEA VACIA
      (if(and(is-string source)(is-string target)(not(null? (get-system-usuario-conectado system))))    ;si es miembro el ultimo elemento del path en las carpetas creadas y existe el archivo en los archivos
         (if (and(member(car(reverse(string-split target "/"))) (map cadr (get-system-ruta system))) (member source (map car(get-files system))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase target) 
                                            (car(reverse(string-split target "/")))
                                            (car(get-system-usuario-conectado system))
                                            (no-remove-archivo source system)
                                            '())     ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                              (get-system-ruta system)))
             
             (if (and(member source (map cadr (get-system-ruta system))) (member (car(string-split target ":/")) (map string(map car(get-system-drive system))))) ;MIEMBRO LAS CARPETAS Y EL DRIVE, PARA EL CASO DE LAS CARPETAS Y DRIVES
                 (make-system(get-system-name system)
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (string-downcase(string-append target source "/")) 
                                                source ;DUEÑO DE FOLDER1, O LO QUE ENTRE
                                                (car(get-system-usuario-conectado system))
                                                (rec-archivos source system)
                                                '())      ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                                                (get-system-ruta system)))
                 
                 (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                             (get-system-drive system) 
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (get-system-ruta system))))
         
         (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                     (get-system-drive system) 
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))))
      

#|FUNCION 15 MOVE
DOMINIO:system X source (file or folder) (String) x target path (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION:función para mover un archivo o carpeta desde una ruta origen a una ruta destino.
La operación de mover elimina el contenido desde la ruta origen.|#

(define move
  (lambda(system)
    (lambda(source target)
      (if(and(is-string source)(is-string target)(not(null? (get-system-usuario-conectado system))))
        ;caso de que source es una carpeta y target es una letra de ruta. si existe la carpeta y la letra a la que va existe en los drives existentes
         (if(and(member source (map cadr (get-system-ruta system)))(member (car(string-split target ":/")) (map string(map car(get-system-drive system)))))

            (make-system(get-system-name system)
                        (get-system-drive system)
                        (get-system-usuarios system)
                        (get-system-usuario-conectado system)
                        (get-system-drive-seleccionado system)
                        (cons(make-carpeta (string-downcase(string-append target source "/"))
                                           source ;DUEÑO DE FOLDER1, O LO QUE ENTRE
                                           (car(get-system-usuario-conectado system))
                                           (rec-archivos source system)
                                           '())      ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                             (remove-carpeta-drive source target system)))
            
         (if(and(member (car(reverse(string-split (string-downcase target) "/"))) (map cadr (get-system-ruta system))) (member (string-downcase target) (map car (get-system-ruta system))))

            (make-system(get-system-name system)
                        (get-system-drive system)
                        (get-system-usuarios system)
                        (get-system-usuario-conectado system)
                        (get-system-drive-seleccionado system)
                        (cons(make-carpeta (string-downcase target)
                                           (car(reverse(string-split target "/"))) ;DUEÑO DE FOLDER1, O LO QUE ENTRE
                                           (car(get-system-usuario-conectado system))
                                           (append(rec-archivos (car(reverse(string-split target "/"))) system) (no-remove-archivo source system))
                                           '()) ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                             (eliminar_archivo system source)))
             
             (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                         (get-system-drive system) 
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (get-system-ruta system))))
         ;si la carpeta en la que se quiera agregar es miembro del string posicion de la ultima actualizacion del sistema, se recrea la lista con ella adelante y solo la raiz
            
         (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                     (get-system-drive system) 
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))))
  

#|FUNCION 16 REN (RENAME)
DOMINIO:system XcurrentName (String) X newName (String)
RECORRIDO:system
RECURSION: no
DESCRIPCION:función para renombrar una carpeta o archivo,
siempre y cuando el nuevo nombre no viole la restricción de unicidad dentro del mismo nivel.|#
(define ren
  (lambda (system)
    (lambda(name new-name)
      (if(and(is-string name)(is-string new-name)(not(null? (get-system-usuario-conectado system))))
         ;el caso de cambiar el nmbre a un archivo de texto
         ;solo tomare en cuenta si existe en la ultima actualizacion de archivos
         ;si existe el nombre en los archivos y si no existe el nuevo nombre en los archivos(tratando de evitar las duplicas)
;Caso nombre archivos
         (if (and (member name (map car(caddr(recuperar-ruta system))))
                  (not(member new-name (map car(caddr(recuperar-ruta system))))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (car(get-system-usuario-conectado system))
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase(car(recuperar-ruta system)))
                                                    (car(reverse(string-split (car(recuperar-ruta system)) "/")))
                                                    (get-system-usuario-conectado system)
                                                    (cambia-nombre-archivo system name new-name)
                                                    '())           
                                            (get-system-ruta system)))
             
                 ;Caso de carpetas, tiene que estar fuera de la carpeta para poder cambiarle el nombre
             ;por lo tanto, el ultimo elemento de posicion debe cambiar
         (if (and(member name (map cadr (get-system-ruta system)))
                  (not(member new-name (map cadr (get-system-ruta system))))) ;no debe existir previamente en cualquier posicion

             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase(string-append(string-append (get-posicion system) new-name)"/")) 
                                            new-name
                                            (car(get-system-usuario-conectado system))
                                            (rec-archivos name system)
                                            '())           
                              (get-system-ruta system)))
             
             (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                         (get-system-drive system) 
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (get-system-ruta system))))
         
         (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                     (get-system-drive system) 
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))))


#|FUNCION 17 DIR
DOMINIO: system X params (String list)
RECORRIDO: string (formateado para poder visualizarlo con display)
RECURSION: SI 
DESCRIPCION:función para listar el contenido de un directorio específico o de toda una ruta,
lo que se determina mediante parámetros.
|#
;problema con entrada nula
(define dir
  (lambda(system)
    (lambda (entry)
      (if (not(null? (get-system-usuario-conectado system)))
       (if(equal? entry "") ;entrada nula-caso base, muestra nomas  ;poner en especifcaciones
         (recursion-system system) ;no funciona, o sea,funciona pero no es lo que piden
         (if(equal? entry "/s")
            (recursion-system system) ;muestra las carpetas y subcarpetas
            (if(equal? entry "/s /a"); a muestra lo oculto
               (recursion-system-archivos2 system) ;recursion mostrnado carpetas y archivos
               #f)))
      #f))))


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
      (if(and(is-char letter)(is-string nombre)(not(null? (get-system-usuario-conectado system))))
         (set-capacidad letter system nombre)
         (make-system(get-system-name system);verdadero
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))))

#|TDA FUNCION 19 ENCRYPT
DOMINIO:system X
        encryptFn (fn: String->String) X decryptFn (fn: String->String) password (String) X folderName or Path (String)
RECORRIDO: system
RECURSION: 
DESCRIPCION: función para encriptar un archivo o carpeta y todo su contenido
(nombres de carpetas, subcarpetas, archivos y el contenido de éstos).
La función de encriptación está dada por una función String->String.
La función de desencriptación (decryptFn) quedará registrada
en la metadata de la carpeta y su contenido, para una posterior desencriptación.|#
(define encrypt
  (lambda (system)
    (lambda (encryptFn decryptFn password path)
      (if (and(is-string password) (not(null?(get-system-usuario-conectado system))))
          (if (member path (map cadr (get-system-ruta system)))
              (make-system(get-system-name system)
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (get-system-usuario-conectado system)
                          (get-system-drive-seleccionado system)
                          (cons(make-carpeta (get-posicion system)
                                             (plus-one (get-carpetas system)) ;NOMBRE ENCRIPTADO
                                             (car(get-system-usuario-conectado system))
                                             (encriptar system path)
                                             password)           
                               (get-system-ruta system)))
              
              (if(member path (map car(get-files system))) ;cuando entregan titulo de archivo
                 (make-system(get-system-name system)
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (get-posicion system)
                                                (car(reverse(string-split (car(recuperar-ruta system)) "/")))
                                                (car(get-system-usuario-conectado system))
                                                (encriptar-t system path)
                                                password)           
                                  (get-system-ruta system)))
                 
                 (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))
          
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))
                        
      
#|TDA FUNCION 20 DECRYPT
DOMINIO: system X  password (String) X folderName or Path (String)
RECORRIDO: system
RECURSION:
DESCRIPCION: función para desencriptar un archivo o carpeta y todo su contenido
(nombres de carpetas, subcarpetas, archivos y el contenido de éstos)..
|#
(define decrypt
  (lambda (system)
    (lambda (password path)
      (if (and(is-string password) (not(null?(get-system-usuario-conectado system)))) ;CASO archivo
          (if (and(equal? password (get-password system)) (member (string-append(plus-one(car(string-split path ".")))"."(car(cdr(string-split path ".")))) (map car (get-files system)))) 
              (make-system(get-system-name system)
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (get-system-usuario-conectado system)
                          (get-system-drive-seleccionado system)
                          (cons(make-carpeta (get-posicion system)
                                             (get-carpetas system) 
                                             (car(get-system-usuario-conectado system))
                                             (desencriptar-t system path)
                                             '())           
                               (get-system-ruta system)))
              ;caso carpetas  
              (if (and(equal? password (caddr (cdr(cdr(cadr(recuperar-listas system))))))  (member(plus-one path) (map cadr (get-system-ruta system))))
                  (make-system(get-system-name system)
                              (get-system-drive system)
                              (get-system-usuarios system)
                              (get-system-usuario-conectado system)
                              (get-system-drive-seleccionado system)
                              (cons(make-carpeta (string-downcase(string-append(get-posicion system)(minus-one (plus-one path))"/"))
                                                 (string-downcase(minus-one (plus-one path))) ;NOMBRE ENCRIPTADO
                                                 (car(get-system-usuario-conectado system))
                                                 (desencriptar system path)
                                                 '())           
                                   (get-system-ruta system)))
                  
                  (make-system(get-system-name system)
                              (get-system-drive system)
                              (get-system-usuarios system)
                              (get-system-usuario-conectado system)
                              (get-system-drive-seleccionado system)
                              (get-system-ruta system))))
              
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))

#|TDA FUNCION 21 PLUS-ONE
DOMINIO: String
RECORRIDO: String
RECURSION: prob
DESCRIPCION: función que transforma un String sumando al código ASCII
de cada carácter un 1. Está función se puede usar en combinación con la función encrypt.|#
(define plus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (+ x 1)) (map char->integer (string->list string)))))
        #f)))
       


#|TDA FUNCION 22 MINUS-ONE
DOMINIO: String
RECORRIDO: String
RECURSION: prob
DESCRIPCION: función que transforma un String restando al código ASCII de
cada carácter un 1. Está función se puede usar en combinación con la función encrypt.|#
(define minus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (- x 1)) (map char->integer (string->list string)))))
        #f)))





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


;copiando carpetas y archivos
(define S46 ((run S35 copy) "foo1.txt" "C:/folder3/"))
(define S47 ((run S46 cd) ".."))
(define S48 ((run S47 copy) "folder1" "D:/"))


;moviendo carpetas y archivos
(define S49 ((run S48 move) "folder3" "D:/")) ;SE BORRA FOLDER3 DE C Y EL REGISTRO
(define S50 ((run S49 cd) "folder1")) ;DEBERIA MOVERSE EN C
(define S51 ((run S50 move) "foo3.docx" "D:/folder3/")) ;75% NO PUEDO RECUPERAR QUE ESTÉN PUESTOS DESPUES DEL QUE DEBO ELIMINAR

(define S52 ((run S51 ren) "foo1.txt" "newFoo1.txt"))
(define S53 ((run S52 ren) "foo2.txt" "newFoo1.txt")) ;no debería efectuar cambios pues ya existe archivo con este nombre
(define S54 ((run S53 cd) ".."))
(define S55 ((run S54 ren) "folder1" "newFolder1"))
      
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
S45
S46
S47
S48|#


(define S56 ((run S55 encrypt) plus-one minus-one "1234" "newFolder1")) ;cambiar lo que hay dentro de la carpeta
(define S57 ((run S56 switch-drive) #\D))
(define S58 ((run S57 cd) "folder3"))
(define S59 ((run S58 encrypt) plus-one minus-one "4321" "foo3.docx"))



;desencriptando archivos y carpetas
(define S60 ((run S59 decrypt) "1234" "foo3.docx")) ;no logra desencriptar por clave incorrecta
(define S61 ((run S60 decrypt) "4321" "foo3.docx"))
(define S62 ((run S61 switch-drive) #\C))
(define S63 ((run S62 decrypt) "1234" "newFolder1"))

#|
S56
S57
S58
S59
S60
S61
S62
S63|#

(provide (all-defined-out))