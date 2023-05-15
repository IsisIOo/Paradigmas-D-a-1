#lang racket
(require "tda_system_21168603_OyanedelAlvarez.rkt")
(require "tda_user_21168603_OyanedelAlvarez.rkt")
(require "tda_folder_21168603_OyanedelAlvarez.rkt")
(require "tda_file_21168603_OyanedelAlvarez.rkt")
(require "tda_drive_21168603_OyanedelAlvarez.rkt")

;este archivo será el main con todas las funciones obligatorias, despues procederé a
;distribuir los tda a su tda correspondiente
;Nombre:Isidora Oyanedel
;Profesor:Gonzalo Martínez
;Laboratorio n1 Paradigmas de programación

#|Funcion 1 SYSTEM-CONSTRUCTOR
DOMINIO: string
RECORRIDO: system (conformado por nombre sistema, drive y usuario/s)
DESCRIPCION: se ingresa un string en la funcion devolviendo un system
             conformado por una lista de elementos que complementan el system
RECURSION: no.
|#

(define (system string)
  (if(is-string string)
     (make-system string null null null null null) ;el primer elemento es el nombre, siguiente drive, sig usuario
     #f))

#|FUNCION 2 RUN
DOMINIO: System X Command (funcion =command)
RECORRIDO: system
RECURSION: no
DESCRIPCION: Función que permite ejecutar un comando (función)
sobre un sistema. Toda acción realizada con run relativa a creación de archivos,
carpetas, renombrar, copiar, mover, eliminar, debe dejar un registro de la fecha de
modificación, además de verificar los permisos del recurso que será alterado|#

(define (run system command) ;se aplica una funcion en la lista system por ejemplo add-rive
  (command system))


#|FUNCION 3 ADD-DRIVE
DOMINIO: system x/dominio del primer lambda
         letter(char) x name(string) x capacity(int) /este se refiere a el dominio de la currificacion
RECORRIDO: system
DESCRIPCION: Función que permite añadir una unidad a unsistema. La letra de la unidad es única.
RECURSION: no
|#
(define add-drive
  (lambda(system)
    (lambda (letter name capacity);info del drive
      (if(and (member letter (map car(get-system-drive system))))
         (make-system(get-system-name system) ;lo mantiene
                     (get-system-drive system) 
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))
         
         (if (and (integer? capacity)(is-string name) (is-char letter) (not(member letter (map car(get-system-drive system)))))
             (make-system (get-system-name system)
                          (cons(make-drive letter name capacity) ;;make-drive= lista que recibe 3 cosas, y le agrega algo adelante de 3 cosas
                               (get-system-drive system)) ;cadr de la lista system, system =lista
                          (get-system-usuarios system)
                          (get-system-usuario-conectado system)
                          (get-system-drive-seleccionado system)
                          (get-system-ruta system))
         
             (make-system(get-system-name system) ;lo mantiene
                         (get-system-drive system) 
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (get-system-ruta system)))))))

#|FUNCION 4  SWITCH-DRIVE
DOMINIO:system X letter (char)
RECORRIDO: system
DESCRIPCION: Permite fijar la unidad en la que el usuario
realizará acciones. La función solo debe funcionar cuando hay un usuario con sesión
iniciada en el sistema
RECURSION: no|#

(define switch-drive
  (lambda(system)
    (lambda(letter)
      (if(and(is-char letter)(member letter (map car(get-system-drive system)))) ;no necesito que la letra sea un string
         (make-system (get-system-name system)
                      (get-system-drive system) ;cadr de la lista system, system =lista
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (if (null?(get-system-drive-seleccionado system))
                          (cons letter (get-system-drive-seleccionado system)) ;verdaderp
                          (if(equal?  letter (car(get-system-drive-seleccionado system))) ;falso
                             (get-system-drive-seleccionado system) ;v
                             (cons letter null)))
                      
                      (cons(make-carpeta (string-downcase(string-append (string letter)":/"))
                                         '()
                                         (get-system-usuario-conectado system)
                                         '()
                                         '()) (get-system-ruta system)))
         
         (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                     (get-system-drive system) 
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system)))))) ;se supone que si no existe la letra en la lista no puede iniciar nada
  ;pero creo que falta que agregar que debe haber tambien un usuario iniciado, componer esa funciones.




#|FUNCION 5 REGISTER
constructor:crea usuario
DOMINIO: system x username (string)
RECORRIDO: system
RECURSION: NO
DESCRIPCION: Función que permite registrar un nuevo usuario al
sistema. El nombre de usuario es único y no puede ser duplicado.
|#
(define register
  (lambda(system)
    (lambda (string)
      (if(and(member string (map car(get-system-usuarios system)))(is-string string)) ;buen
         (make-system(get-system-name system)
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system)) ;if
         
         (make-system(get-system-name system);else
                     (get-system-drive system)
                     (cons(make-user string)(get-system-usuarios system))
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system)))))) ;no me guarda los otros usuarios

#|FUNCION 6 LOGIN
DOMINIO:system X userName (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION: Función que permite iniciar sesión con un usuario del sistema, solo si éste existe.|#

(define login
  (lambda(system)
    (lambda (string)
      (if (and (null? (get-system-usuario-conectado system))(is-string string))
          (if (is-member string system);verdadero
              (make-system(get-system-name system);verdadero
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (cons(is-status-user string)(get-system-usuario-conectado system))
                          (get-system-drive-seleccionado system)
                          (get-system-ruta system))
              
              (make-system(get-system-name system);falso
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (get-system-usuario-conectado system)
                          (get-system-drive-seleccionado system)
                          (get-system-ruta system)))
          
          (make-system(get-system-name system);falso
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))

#|FUNCION 7 LOGOUT
DOMINIO:system 
RECORRIDO: system
RECURSION: no
DESCRIPCION: Función que permite cerrar la sesión de un usuario en el sistema.
|#        

(define (logout system)
  (if (not(null?(get-system-usuario-conectado system)))
      (make-system(get-system-name system);verdadero
                  (get-system-drive system)
                  (get-system-usuarios system)
                  '();asigna lista vacia a los usuarios logeados
                  (get-system-drive-seleccionado system)
                  (get-system-ruta system))
      
      (make-system(get-system-name system);falso
                  (get-system-drive system)
                  (get-system-usuarios system)
                  (get-system-usuario-conectado system)
                  (get-system-drive-seleccionado system)
                  (get-system-ruta system))))

#|FUNCION 8 MD
DOMINIO: system X name (String) 
RECORRIDO: SYSTEM
RECURSION: no 
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
                                        (get-system-usuario-conectado system)
                                        '()
                                        '()) (get-system-ruta system)))
         
         (make-system(get-system-name system);falso
                     (get-system-drive system)
                     (get-system-usuarios system)
                     (get-system-usuario-conectado system)
                     (get-system-drive-seleccionado system)
                     (get-system-ruta system))))));que devuelva lo mismo en caso de que no cumpla


#|FUNCION 9 CD
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
                         (cons(make-carpeta (string-downcase(string-append (car(get-recuperar-ruta system)) path "/"))
                                            null
                                            (get-system-usuario-conectado system)
                                            (get-rec-archivos path system)
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
                                                (get-system-usuario-conectado system)
                                                (get-rec-archivos path system)
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
                                                   (get-system-usuario-conectado system)
                                                   (get-rec-archivos path system)
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
                                                      (get-system-usuario-conectado system)
                                                      (get-rec-archivos path system)
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


#|FUNCION 10 ADD-FILE
DOMINIO:system X file
RECORRIDO: system
RECURSION: NO
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
                                         (get-system-usuario-conectado system)
                                         (cons file (get-files system))
                                         '())           
                           (get-system-ruta system)))
          
          (make-system(get-system-name system)
                      (get-system-drive system)
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))

#|FUNCION 11 DEL
DOMINIO:system X fileName or fileNamePattern (string)
RECORRIDO: system
RECURSION: NO
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
                                            (get-system-usuario-conectado system)
                                            (get-remove-extension (car (string-split (car(string-split filename "*")) ".")) system)
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
                                                (get-system-usuario-conectado system)
                                                (get-letter-titles filename system)
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
                                                    (get-system-usuario-conectado system)
                                                    (remove-titulo filename system)
                                                    '())           
                                      (get-system-ruta system)))
                     
                     (if (member filename (map cadr(get-system-ruta system))) ;borra LA CARPETA SI ES QUE EXISTE folder, se devuelve uno al borrar 
                         (make-system(get-system-name system)
                                     (get-system-drive system)
                                     (get-system-usuarios system)
                                     (get-system-usuario-conectado system)
                                     (get-system-drive-seleccionado system)
                                     (get-remove-posicion filename system))
                         
                         (if(equal? "*.*" filename) ;borra todos los archivos DOC TXT
                            (make-system(get-system-name system)
                                        (get-system-drive system)
                                        (get-system-usuarios system)
                                        (get-system-usuario-conectado system)
                                        (get-system-drive-seleccionado system)
                                        (cons(make-carpeta (get-posicion system)
                                                           (car(reverse(string-split(get-posicion system)"/")))
                                                           (get-system-usuario-conectado system)
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

#|FUNCION 12 RD
DOMINIO:system X folderName or folderPath (string)
RECORRIDO: system
RECURSION: no
DESCRIPCION:  función para eliminar una carpeta, siempre y cuando ésta esté vacía.
Una carpeta se puede eliminar estando posicionado fuera de ésta.|#

(define rd
  (lambda (system)
    (lambda(foldername)
      (if (and(is-string foldername)(member foldername (map cadr (get-system-ruta system))) (not(null? (get-system-usuario-conectado system))))
          (if(not(null? (get-rec-archivos foldername system))) ;si es vacia la union del dueño y los archivos no se hace nada
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
                             (get-remove-posicion foldername system)))
          
          (make-system(get-system-name system) ;en caso contrario solo lo mantiene
                      (get-system-drive system) 
                      (get-system-usuarios system)
                      (get-system-usuario-conectado system)
                      (get-system-drive-seleccionado system)
                      (get-system-ruta system))))))

#|FUNCION 13 COPY
DOMINIO: system X source (file or folder) (String) x target path (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION:función para copiar un archivo o carpeta desde una ruta origen a una ruta destino.|#

(define copy ; no debe eliminar los archivos
  (lambda(system)
    (lambda(source target)
      ;AGREGAR QUE SE PUEDE HACER SOLO SI LA ULTIMA POSICION DE LOS ARCHIVOS NO SEA VACIA ;problema con get-files
      (if(and(is-string source)(is-string target)(not(null? (get-system-usuario-conectado system))))    ;si es miembro el ultimo elemento del path en las carpetas creadas y existe el archivo en los archivos
         (if (and(member(car(reverse(string-split target "/"))) (map cadr (get-system-ruta system))) (member source (map car(map car (get-remove-nulos-archivos system)))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase target) 
                                            (car(reverse(string-split target "/")))
                                            (get-system-usuario-conectado system)
                                            (get-no-remove-archivo source system)
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
                                                (get-system-usuario-conectado system)
                                                (get-rec-archivos source system)
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

#|FUNCION 14 MOVE
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
                                           (get-system-usuario-conectado system)
                                           (get-rec-archivos source system)
                                           '())      ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                             (get-remove-carpeta-drive source target system)))
            ;caso de archivo
         (if(and(member (car(reverse(string-split (string-downcase target) "/"))) (map cadr (get-system-ruta system))) (member (string-downcase target) (map car (get-system-ruta system))))

            (make-system(get-system-name system)
                        (get-system-drive system)
                        (get-system-usuarios system)
                        (get-system-usuario-conectado system)
                        (get-system-drive-seleccionado system)
                        (cons(make-carpeta (string-downcase target)
                                           (car(reverse(string-split target "/"))) ;DUEÑO DE FOLDER1, O LO QUE ENTRE
                                           (get-system-usuario-conectado system)
                                           (append(get-rec-archivos (car(reverse(string-split target "/"))) system) (get-no-remove-archivo source system))
                                           '()) ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                             (set-eliminar_archivo system source)))
             
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


#|FUNCION 15 REN (RENAME)
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
         (if (and (member name (map car(caddr(get-recuperar-ruta system))))
                  (not(member new-name (map car(caddr(get-recuperar-ruta system))))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase(car(get-recuperar-ruta system)))
                                                    (car(reverse(string-split (car(get-recuperar-ruta system)) "/")))
                                                    (get-system-usuario-conectado system)
                                                    (set-cambia-nombre-archivo system name new-name)
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
                                            (get-system-usuario-conectado system)
                                            (get-rec-archivos name system)
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

#|FUNCION 16 DIR
DOMINIO: system X params (String list)
RECORRIDO: string (formateado para poder visualizarlo con display)
RECURSION: SI, de cola en las funciones
DESCRIPCION:función para listar el contenido de un directorio específico o de toda una ruta,
lo que se determina mediante parámetros.
|#
;problema con entrada nula 75%
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

#|FUNCION 17 FORMAT
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

#|FUNCION 18 ENCRYPT
DOMINIO:system X
        encryptFn (fn: String->String) X decryptFn (fn: String->String) password (String) X folderName or Path (String)
RECORRIDO: system
RECURSION: Si, en las partes que hace la encriptacion en sí, usa de cola
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
                                             (get-system-usuario-conectado system)
                                             (set-encriptar system path plus-one)
                                             password)           
                               (get-system-ruta system)))
              
              (if(member path (map car(get-files system))) ;cuando entregan titulo de archivo
                 (make-system(get-system-name system)
                             (get-system-drive system)
                             (get-system-usuarios system)
                             (get-system-usuario-conectado system)
                             (get-system-drive-seleccionado system)
                             (cons(make-carpeta (get-posicion system)
                                                (car(reverse(string-split (car(get-recuperar-ruta system)) "/")))
                                                (get-system-usuario-conectado system)
                                                (set-encriptar-t system path plus-one)
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

#|FUNCION 19 DECRYPT
DOMINIO: system X  password (String) X folderName or Path (String)
RECORRIDO: system
RECURSION: Si, de cola
DESCRIPCION: función para desencriptar un archivo o carpeta y todo su contenido
(nombres de carpetas, subcarpetas, archivos y el contenido de éstos)..
|#
(define decrypt
  (lambda (system) ;carpeta-archivo
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
                                             (get-system-usuario-conectado system)
                                             (set-desencriptar-t system path plus-one minus-one)
                                             '())           
                               (get-system-ruta system)))
              ;caso carpetas  
              (if (and(equal? password (caddr (cdr(cdr(cadr(get-recuperar-listas system))))))  (member(plus-one path) (map cadr (get-system-ruta system))))
                  (make-system(get-system-name system)
                              (get-system-drive system)
                              (get-system-usuarios system)
                              (get-system-usuario-conectado system)
                              (get-system-drive-seleccionado system)
                              (cons(make-carpeta (string-downcase(string-append(get-posicion system)(minus-one (plus-one path))"/"))
                                                 (string-downcase(minus-one (plus-one path))) ;NOMBRE ENCRIPTADO
                                                 (get-system-usuario-conectado system)
                                                 (set-desencriptar system path plus-one minus-one)
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

#|FUNCION 20 PLUS-ONE
DOMINIO: String
RECORRIDO: String
RECURSION: no
DESCRIPCION: función que transforma un String sumando al código ASCII
de cada carácter un 1. Está función se puede usar en combinación con la función encrypt.|#
(define plus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (+ x 1)) (map char->integer (string->list string)))))
        #f)))
       


#|FUNCION 21 MINUS-ONE
DOMINIO: String
RECORRIDO: String
RECURSION: no
DESCRIPCION: función que transforma un String restando al código ASCII de
cada carácter un 1. Está función se puede usar en combinación con la función encrypt.|#
(define minus-one
  (lambda(string)
    (if (is-string string)
        (list->string(map integer->char(map (lambda (x) (- x 1)) (map char->integer (string->list string)))))
        #f)))


#|FUNCION 22 GREP
DOMINIO: system X pattern (String) X fileName or path (String)
RECORRIDO: String formateado para visualizarlo con display
RECURSION: -
DESCRIPCION: Función que permite buscar dentro del contenido de un archivo específico o dentro de una ruta.|#


#|FUNCION 23 VIEW-TRASH
DOMINIO: system
RECORRIDO: String formateado para visualizarlo con display
RECURSION: -
DESCRIPCION: función que permite obtener el contenido de la papelera de reciclaje de un sistema.|#


#|FUNCION 24 RESTORE
DOMINIO: system
         fileName or pattern (String)
RECORRIDO: String formateado para visualizarlo con display
RECURSION: -
DESCRIPCION: función que restaurar contenido específico dentro de la papelera para ubicarlo en su ubicación original.|#




;creando un sistema
(define S0 (system "newSystem"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))

;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))


;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))

;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))

;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K)) ;no existe este drive 
(define S12 ((run S11 switch-drive) #\C))

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
(define S46 ((run S35 copy) "foo1.txt" "C:/folder3/")) ;funciona por coincidencia
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


(define S56 ((run S55 encrypt) plus-one minus-one "1234" "newFolder1")) ;cambiar lo que hay dentro de la carpeta

;listando la información
(display ((run S16 dir)"")) ;aqui le coloque comillas para que corra
(newline)
(display ((run S55 dir)"")) ;aqui le coloque comillas para que corra
(newline)
(display ((run S55 dir) "/s")) ;muestra carpetas y subcarpetas de la unidad C
(newline)
(display ((run S55 dir) "/s /a")) ;muestra todo el contenido de carpetas y subcarpetas de la unidad C incluyendo archivo oculto goo4.docx

;(display ((run S16 dir) ""))
;(newline)
;(display ((run S55 dir)"/s"))

(define S57 ((run S56 switch-drive) #\D))
(define S58 ((run S57 cd) "folder3"))
(define S59 ((run S58 encrypt) plus-one minus-one "4321" "foo3.docx"))


;desencriptando archivos y carpetas
(define S60 ((run S59 decrypt) "1234" "foo3.docx")) ;no logra desencriptar por clave incorrecta
(define S61 ((run S60 decrypt) "4321" "foo3.docx"))
(define S62 ((run S61 switch-drive) #\C))
(define S63 ((run S62 decrypt) "1234" "newFolder1"))



;FUNCIONES NO HECHAS

;;buscando contenido
;(define S64 ((run S63 cd) "newFolder1"))
;(display ((run S64 grep) "hello" "newFoo1.txt"))
;(display ((run S64 grep) "hello" "*.*"))

;viendo la papelera
;(display (run S45 viewTrash))

;restaurando
;(define S65 ((run S45 restore) "folder1"))