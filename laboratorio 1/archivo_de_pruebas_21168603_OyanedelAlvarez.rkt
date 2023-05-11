#lang racket
;script de pruebas
(require "tda_system_21168603_OyanedelAlvarez.rkt")
(require "tda_user_21168603_OyanedelAlvarez.rkt")
(require "tda_folder_21168603_OyanedelAlvarez.rkt")
(require "tda_file_21168603_OyanedelAlvarez.rkt")
(require "tda_drive_21168603_OyanedelAlvarez.rkt")

#|Funcion 1 System - constructor
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

#|FUNCION 4  SWITCH DRIVE
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
          (if (comprobar string system);verdadero
              (make-system(get-system-name system);verdadero
                          (get-system-drive system)
                          (get-system-usuarios system)
                          (cons(status-user string)(get-system-usuario-conectado system))
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
#|FUNCION 8 md
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
                         (cons(make-carpeta (string-downcase(string-append (car(recuperar-ruta system)) path "/"))
                                            null
                                            (get-system-usuario-conectado system)
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
                                                (get-system-usuario-conectado system)
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
                                                   (get-system-usuario-conectado system)
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
                                                      (get-system-usuario-conectado system)
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


#|FUNCION 10  TDA system - add-file
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

#|TDA FUNCION 11 DEL
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
                                                (get-system-usuario-conectado system)
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

#|FUNCION 12 rd
DOMINIO:system X folderName or folderPath (string)
RECORRIDO: system
RECURSION: no
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

#|FUNCION 13 copy
DOMINIO: system X source (file or folder) (String) x target path (String)
RECORRIDO: system
RECURSION: NO
DESCRIPCION:función para copiar un archivo o carpeta desde una ruta origen a una ruta destino.|#

(define copy ; no debe eliminar los archivos
  (lambda(system)
    (lambda(source target)
      ;AGREGAR QUE SE PUEDE HACER SOLO SI LA ULTIMA POSICION DE LOS ARCHIVOS NO SEA VACIA ;problema con get-files
      (if(and(is-string source)(is-string target)(not(null? (get-system-usuario-conectado system))))    ;si es miembro el ultimo elemento del path en las carpetas creadas y existe el archivo en los archivos
         (if (and(member(car(reverse(string-split target "/"))) (map cadr (get-system-ruta system))) (member source (map car(map car (remove-nulos-archivos system)))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
                         (get-system-drive-seleccionado system)
                         (cons(make-carpeta (string-downcase target) 
                                            (car(reverse(string-split target "/")))
                                            (get-system-usuario-conectado system)
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
                                                (get-system-usuario-conectado system)
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
                                           (rec-archivos source system)
                                           '())      ;EL ARCHIVO DEBE IR CON TODAS SUS PROPIEDADES       
                             (remove-carpeta-drive source target system)))
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
         (if (and (member name (map car(caddr(recuperar-ruta system))))
                  (not(member new-name (map car(caddr(recuperar-ruta system))))))
             (make-system(get-system-name system)
                         (get-system-drive system)
                         (get-system-usuarios system)
                         (get-system-usuario-conectado system)
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
                                            (get-system-usuario-conectado system)
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

#|TDA FUNCION 18 ENCRYPT
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
                                                (car(reverse(string-split (car(recuperar-ruta system)) "/")))
                                                (get-system-usuario-conectado system)
                                                (encriptar-t system path plus-one)
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

#|TDA FUNCION 19 DECRYPT
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
                                             (desencriptar-t system path plus-one minus-one)
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

#|TDA FUNCION 20 PLUS-ONE
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
       


#|TDA FUNCION 21 MINUS-ONE
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


;SCRIPTS DE PRUEBA
;creando un sistema
(define S0A (system 123)) ;Deberia fallar porque no funciona con int
(define S0B (system #\D)) ;Deberia fallar porque no funciona con char
(define S0C (system "System"));SEGUIR CON ESTE
;S0A
;S0B
;S0C
(define S1A ((run S0C add-drive) "hola!" "SO" 1000)) ;No deberia funcionar porque el primer elemento debe ser char
(define S1B ((run S0C add-drive) #\C 123 1000)) ;No deberia funcionar porque el segundo elemento es un int
(define S1C ((run S0C add-drive) #\C "SO" "hola")) ;No deberia funcionar porque el tercer elemento es un string
(define S1D ((run S0C add-drive) #\C "SO" 1000)) ;FUNCIONA PORQUE CUMPLE LAS CONDICIONES
(define S1E ((run S1D add-drive) #\G "S1" 4000))
(define S1F ((run S1E add-drive) #\D "S34" 2000))
(define S1G ((run S1F add-drive) #\E "S12" 12000))
;S1A
;S1B
;S1C
;S1D ;SEGUIR ESTE PORQUE ES EL QUE FUNCIONA CON TODO
;S1E
;S1F
;S1G

(define S2A ((run S1G register) 123)) ;Arroja falso porque es un int LANZA #F
(define S2B ((run S1G register) #\C)) ;No deberia funcionar porque es un char LANZA #F NO SE PUEDE SEGUIR CON EL 
(define S2C ((run S1G register) "Isidora")) 
(define S2D ((run S2C register) "Lola"))
(define S2E ((run S2D register) "Karla"));SEGUIR CON ESTE
;S2A
;S2B
;S2C
;S2D
;S2E

(define S3A ((run S2E login) 123)) ;No inicia sesion porque 1. es un INT y no funciona con él 2. este usuario no existe
(define S3B ((run S3A login) #\C)) ;No inicia sesion porque 1. es un CHAR y no funciona con él 2. este usuario no existe
(define S3C ((run S3B login) "Sandra")) ;No inicia sesion porque este usuario no existe
(define S3D ((run S3C login) "Isidora")) ;Inicia sesion
;S3A
;S3B
;S3C
;S3D

;cerrando sesión user1 e iniciando con user2
(define S4A (run S3D logout)) ;CIERRA LA SESION
(define S4B ((run S4A login) "Isidora")) ;VUELVE A INICIAR SESION
(define S4C (run S4B logout))
(define S4D ((run S4C login) "Thomas")) ;No inicia porque no existe este usuario
(define S4E ((run S4D login) "Lola"))
(define S4F (run S4E logout))
(define S4G ((run S4F login) "Isidora")) ;AQUI

;S4A
;S4B
;S4C
;S4D
;S4E
;S4F
;S4G

;cambios de unidad, incluyendo unidad inexistente K
(define S5A ((run S4G switch-drive) #\K)) ;no existe este drive asi que no hace nada, tampoco funciona si no tengo usuario conectado
(define S5B ((run S5A switch-drive) #\G)) ;funciona porque existe el drive
(define S5C ((run S5B switch-drive) #\C)) ;funciona

;S5A
;S5B
;S5C

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S6A ((run S5C md) "carpeta1"))
(define S6B ((run S6A md) "carpeta2"))
(define S6C ((run S6B md) "carpeta1")) ;funciona
(define S6D ((run S6C md) "carpeta2"))

;S6A
;S6B
;S6C
;S6D

;ingresa a carpeta folder2
(define S7A ((run S6D cd) "carpeta2"))
;;vuelve a carpeta anterior
(define S7B ((run S7A cd) ".."));funciona
(define S7C ((run S7B cd) "carpeta1"))
;(define S7D ((run S7C md) "carpeta4")) ;cumple la creacion de subcarpetas
;(define S7E ((run S7D cd) "carpeta4"))
;S7A
;S7B
;S7C
;S7D
;S7E

;formateando drive D:
(define S8A ((run S7C format) #\G "GAGA")) ;FUNCIONAAAA 17-04-2023
(define S8B ((run S8A format) #\E "ELMO"))
(define S8C ((run S8B format) #\D "DEDENEE"))
;S8A
;S8B
;S8C

(define S9A ((run S8C add-file) (file "bruno.txt" "txt" "Te extraño dog")))
(define S9B ((run S9A add-file) (file "cool.txt" "txt" "Hola don cool")))
(define S9C ((run S9B add-file) (file "luna.docx" "docx" "Llorona")))
;S9A
;S9B
;S9C

(define S10A ((run S9C del) "*.txt")) ;borra todos los txt ;FUNCIONA 19-04
(define S10B ((run S9C del) "l*.docx")) ;borra los que comienzan con f y tengan extension .doc BORRA 1 ;FUNCIONA 19-04
(define S10C ((run S9C del) "bruno.txt")) ;borra el que tiene ese titulo ;FUNCIONA 19-04
(define S10D ((run S9C cd) ".."))
;S10A
;S10B
;S10C
;S10D

(define S11A ((run S10D rd) "carpeta1"))
(define S11A2 ((run S11A cd) "carpeta1")) 
(define S11B ((run S11A2 rd) "folder1")) ;no deberia hacer nada porque esta carpeta no existe
(define S11C ((run S11B del) "*.*")) ;borra los archivos de carpeta1
(define S11D ((run S11C rd) "carpeta1")) ;borra carpeta1 del sistema
(define S11E ((run S11D cd) "carpeta1")) ;ya no puede entrar porque no existe
;S11A
;S11A2
;S11B
;S11C
;S11D
;S11E

(define S12A ((run S9C copy) "bruno.txt" "C:/carpeta2/")) ;REAPARECE CARPETA1 PORQUE USA UNAA VERSION ANTIGUA
(define S12B ((run S12A copy) "luna.txt" "C:/carpeta2/")) ;no copia porque no existe archivo luna.txt
(define S12C ((run S12B copy) "carpeta1" "G:/"))
;S12A
;S12B
;S12C

(define S13A ((run S12C move) "folder3" "D:/"));NO DEBERIA HACER NADA
(define S13B ((run S13A move) "folder3" #\D)) ;NO DEBERIA HACER NADA
;(define S13C ((run S13B move)  "luna.docx" "F:/carpeta2/")) ;funciona bien pero al reves, como lo muevo al del drive, se borra del drive que no es el actual cuando debio moverse en el drive actual, pero el move solo funciona entre distintos drives?
(define S13C ((run S13B move) 123 "C:/carpeta2/")) ;no deberia hacer nada
;S13A
;S13B
;S13C

(define S14A ((run S13C ren) "bruno.txt" "newBruno.txt")) ;solo ejecuta este cambio porque en la ruta actual de c la ultima carpeta abierta solo tiene el archivo bruno
(define S14B ((run S14A ren) "luna.docx" "newLuna.docx")) ;no hace nada porque en la ruta actual este archivo no se encuentra
(define S14C ((run S14B ren) "newBruno.txt" "newLuna.docx"))

;S14A
;S14B
;S14C

(define S14D ((run S14C cd) ".."))
(define S14E ((run S14D ren) "carpeta2" "NuevitaCarpeta2")) 
;S14D
;S14E

(define S14F ((run S14E cd) "NuevitaCarpeta2"))
;S14F

;(display ((run S14D dir) "/s")) ;muestra carpetas y subcarpetas de la unidad C FUNCIONA
;(display ((run S14D dir) "/s /a"))
;(display ((run S14D dir) "/d")) ;NO EXISTE ESTA OPCION

(define S15A ((run S14F encrypt) plus-one minus-one "1234" "NuevitaCarpeta2"))
(define S15B ((run S15A switch-drive) #\G))
(define S15C ((run S15B cd) "carpeta1"))
(define S15D ((run S15C encrypt) plus-one minus-one "4321" "luna.docx"))
(define S15E ((run S15D encrypt) plus-one minus-one "9002" "carpeta3")) ;no deberia hacer nada porque la carpeta no existe 
;S15A
;S15B
;S15C
;S15D
;S15E
;no puede encriptar dos veces, se encripta el titulo pero no el contenido

;desencriptando archivos y carpetas
(define S16A ((run S15E decrypt) "5118" "luna.docx")) ;no logra desencriptar por clave incorrecta
(define S16B ((run S16A decrypt) "4321" "luna.docx"))

(define S16C ((run S16B switch-drive) #\C))
(define S16D ((run S16C decrypt) "1234" "NuevitaCarpeta2"))

;S16A
;S16B
;S16C
;S16D

;75% MOVE ARCHIVO A OTRO CARPETA O DRIVE Y 75% DIR Y 75% ENCRIPTAR PORQUE SOLO LO PUEDE HACER UNA VEZ Y DESENCRIPTAR