#lang racket
;(define str "Hello, world!")
;(substring str 7 12)

;(define my-string "Hola mundo")
;(display (string-length my-string))

(define my (car(car(get-system-ruta S17))))
> (display (string-length my))
11
>

> (substring my 1 7)
":/fold"
> (substring my 1 3)
":/"

(substring str (- (string-length str) 5) (string-length str))