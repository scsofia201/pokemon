(require (lib "graphics.ss" "graphics"))
(open-graphics)

(require racket/set)
(require racket/dict)
(require racket/hash)

(define SPRITE_HEIGHT 52)
(define SPRITE_WIDTH 34)
(define SPRITE_FEETS 20)

(define PROBABILIDAD_ATAQUE 10)

(define mapa-actual 1)

;'down  'left  'right

(define (cambiomapa? x y d ventana)
  (cond
    ((= mapa-actual 1) (if (< y -5) (begin (set! mapa-actual 2) (moverse x 480 'up d ventana)) #t))
    ((= mapa-actual 2) (cond ((= y 490) (begin (set! mapa-actual 1) (moverse x 10 'down d ventana)))
                      ((< y -5) (begin (set! mapa-actual 2.1) (moverse x 480 'up d ventana)))(else #t)))
    ((= mapa-actual 2.1) (cond ((= y 490) (begin (set! mapa-actual 2) (moverse x 10 'down d ventana)))
                        ((< y -5) (if (< x 285) (begin (set! mapa-actual 3) (moverse 558 480 'up d ventana))
                                      (begin (set! mapa-actual 3.2) (moverse 10 480 'up d ventana)))) (else #t)))

    ((= mapa-actual 3) (cond ((> x 570) (begin (set! mapa-actual 3.2) (moverse 10 y 'left d ventana)))
                      ((= y 490) (begin (set! mapa-actual 2.1) (moverse 260 10 'down d ventana)))
                      ((< y -5) (begin (set! mapa-actual 3.1) (moverse x 480 'up d ventana)))(else #t)))
    ((= mapa-actual 3.1) (cond ((> x 570) (begin (set! mapa-actual 3.3) (moverse 5 y 'left d ventana)))
                        ((< x 5)  (begin (set! mapa-actual 4.3) (moverse 570 10 'right d ventana)))
                        ((= y 490)  (begin (set! mapa-actual 3) (moverse x 10 'down d ventana))) (else #t)))
    ((= mapa-actual 3.2) (cond ((< x 5) (begin (set! mapa-actual 3) (moverse 570 y 'right d ventana)))
                        ((= y 490) (begin (set! mapa-actual 2.1) (moverse 310 10 'down d ventana)))
                        ((< y -5) (begin (set! mapa-actual 3.3) (moverse x 480 'up d ventana))) (else #t)))
    ((= mapa-actual 3.3) (cond ((< x 5) (begin (set! mapa-actual 3.1) (moverse 570 y 'left d ventana)))
                        ((= y 490) (begin (set! mapa-actual 3.2) (moverse x 10 'down d ventana)))(else #t)))


    
    ((= mapa-actual 4) (cond ((> y 480) (begin (set! mapa-actual 4.2) (moverse x 5 'down d ventana)))
                      ((> x 570) (begin (set! mapa-actual 4.1) (moverse 5 y 'left d ventana))) (else #t)))
    ((= mapa-actual 4.1) (cond ((> y 480) (begin (set! mapa-actual 4.3) (moverse x 5 'down d ventana)))
                        ((< x 5) (begin (set! mapa-actual 4) (moverse 570 y 'left d ventana))) (else #t)))
    ((= mapa-actual 4.2) (cond ((< y 5) (begin (set! mapa-actual 4) (moverse x 480 'down d ventana)))
                        ((> x 570) (begin (set! mapa-actual 4.3) (moverse 10 y 'down d ventana))) (else #t)))
    ((= mapa-actual 4.3)(cond ((> x 570)(begin (set! mapa-actual 3.1) (moverse 10 450 'down d ventana)))
                       ((< x 5) (begin (set! mapa-actual 4.2) (moverse 570 y 'down d ventana)))
                       ((< y -5) (begin (set! mapa-actual 4.1) (moverse x 480 'down d ventana))) (else #t)))) )



(define-struct obs (x y width height jumpable))
(define-struct grass (x y width height))

(define mapas (hash 1 "mapa 1 a.jpg"
                    2 "mapa 2 a.jpg"
                    2.1 "mapa 2 b.jpg"
                    3 "mapa 3 a.jpg"
                    3.1 "mapa 3 b.jpg"
                    3.2 "mapa 3 c.jpg"
                    3.3 "mapa 3 d.jpg"
                    4 "mapa 4 a.jpg"
                    4.1 "mapa 4 b.jpg"
                    4.2 "mapa 4 c.jpg"
                    5 "casa1.1.png"
                    5.1 "casa1.2.png"
                    5.2 "casa2.png"
                    5.3 "casa3.png"
                    5.4 "enfermeria.png"
                    5.5 "tienda.png"
                    5.6 "laboratorio.png"
                   ))

(define (obstaculos-mapa)
  (cond ((= mapa-actual 1) (set (make-obs  0 0 298 42 #f)
                    (make-obs  352 0 300 42 #f)
                    (make-obs  125 100 124 80 #f)
                    (make-obs  352 100 124 80 #f)
                    (make-obs  325 250 174 80 #f)
                    (make-obs  125 288 124 10 #f)
                    (make-obs  325 414 150 10 #f)
                    (make-obs  174 424 100 80 #f)
                    (make-obs  0 40 40 460 #f)
                    (make-obs  560 40 40 460 #f)
                    (make-obs  50 490 500 10 #f)))

 ((= mapa-actual 2) (set (make-obs  0 405 298 200 #f)
                    (make-obs  352 405 300 200 #f)
                    (make-obs  0 115 298 50 #f)
                    (make-obs  0 0 40 460 #f)
                    (make-obs  560 0 40 460 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  50 14 50 13 #t)
                    (make-obs  125 14 100 13 #t)
                    (make-obs  278 14 270 13 #t)
                    (make-obs  452 165 100 13 #t)
                    (make-obs  225 289 330 13 #t)
                    (make-obs  50 289 100 13 #t)))

  ((= mapa-actual 2.1) (set (make-obs  0 0 250 42 #f)
                    (make-obs  352 0 250 42 #f)
                    (make-obs  205 85 42 205 #f)
                    (make-obs  55 360 42 55 #f)
                    (make-obs  252 360 145 55 #f)
                    (make-obs  0 40 40 460 #f)
                    (make-obs  560 40 40 460 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  50 138 150 13 #t)
                    (make-obs  250 138 150 13 #t)
                    (make-obs  50 264 150 13 #t)
                    (make-obs  100 389 150 13 #t)))
  
  ((= mapa-actual 3) (set (make-obs  160 340 190 150 #f)
                    (make-obs  350 388 200 115 #f)
                    (make-obs  275 155 150 125 #f)
                    (make-obs  305 95 42 60 #f)
                    (make-obs  180 65 88 60 #f)
                    (make-obs  590 80 10 100 #f)
                    (make-obs  0 0 173 500 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  170 265 105 13 #t)
                    (make-obs  425 265 75 13 #t)
                    (make-obs  575 265 28 13 #t)))

  ((= mapa-actual 3.1) (set (make-obs  0 0 200 390 #f)
                    (make-obs  200 0 275 115 #f)
                    (make-obs  225 155 250 270 #f)
                    (make-obs  475 180 25 225 #f)
                    (make-obs  575 225 25 80 #f)
                    (make-obs  575 405 25 80 #f)))

  ((= mapa-actual 3.2) (set (make-obs  50 390 410 110 #f)
                    (make-obs  0 75 125 90 #f)
                    (make-obs  455 0 150 500 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  0 265 450 13 #t)))

  ((= mapa-actual 3.3) (set (make-obs  250 410 100 60 #f)
                    (make-obs  0 390 125 70 #f)
                    (make-obs  0 215 125 70 #f)
                    (make-obs  75 300 50 30 #f)
                    (make-obs  225 155 148 100 #f)
                    (make-obs  125 387 330 13 #f)
                    (make-obs  455 0 145 500 #f)
                    (make-obs  0 0 455 105 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  135 320 315 10 #t)))


  ((= mapa-actual 4) (set (make-obs  125 175 350 105 #f)
                    (make-obs  225 320 100 60 #f)
                    (make-obs  325 340 195 15 #f)
                    (make-obs  525 180 50 25 #f)
                    (make-obs  550 280 25 75 #f)
                    (make-obs  200 445 225 70 #f)
                    (make-obs  0 435 190 20 #f)
                    (make-obs  170 435 20 100 #f)
                    (make-obs  0 0 600 125 #f)
                    (make-obs  0 124 48 315 #f)))

 ((= mapa-actual 4.1) (set (make-obs  175 175 325 105  #f)
                    (make-obs  225 326 225 105 #f)
                    (make-obs  50 328 150 80 #f)
                    (make-obs  38 382 12 120 #f)
                    (make-obs  150 470 50 5 #f)
                    (make-obs  50 192 75 5 #f)
                    (make-obs  0 266 150 10 #f)
                    (make-obs  200 495 350 10 #f)
                    (make-obs  562 0 58 500 #f)
                    (make-obs  0 0 562 125 #f)))

  ((= mapa-actual 4.2) (set (make-obs  175 0 250 75 #f)
                    (make-obs  425 60 165 15 #f)
                    (make-obs  105 155 495 125 #f)
                    (make-obs  120 290 30 30 #f)
                    (make-obs  0 445 300 55 #f)
                    (make-obs  305 385 165 120 #f)
                    (make-obs  0 35 50 410 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  100 388 125 13 #t)
                    (make-obs  550 388 50 13 #t)
                    (make-obs  100 260 13 142 #t)))

 ((= mapa-actual 4.3) (set (make-obs  130 405 465 95 #f)
                    (make-obs  530 120 80 300 #f)
                    (make-obs  0 155 120 125 #f)
                    (make-obs  40 0 10 80 #f)
                    (make-obs  175 85 300 15 #f)
                    (make-obs  275 100 180 55 #f)
                    (make-obs  175 95 10 155 #f)
                    (make-obs  465 95 10 155 #f)
                    (make-obs  180 220 280 60 #f)
                    (make-obs  175 335 125 10 #f)
                    (make-obs  375 335 100 10 #f)
                    (make-obs  200 0 400 10 #f)
                    ;saltar de arriba a bajo:
                    (make-obs  0 388 125 13 #t)
                    (make-obs  112 250 13 150 #t)))

((= mapa-actual 5) (set (make-obs  0 0 600 100 #f)
                    (make-obs  0 100 190 40 #f)
                    (make-obs  245 100 55 40 #f)
                    (make-obs  505 180 100 10 #f)
                    (make-obs  250 220 100 100 #f)
                    (make-obs  350 200 50 75 #f)
                    (make-obs  0 350 45 85 #f)
                    (make-obs  555 350 45 85 #f)))
((= mapa-actual 5.1) (set (make-obs  0 0 600 100 #f)
                    (make-obs  0 100 275 20 #f)
                    (make-obs  270 215 60 145 #f)
                    (make-obs  375 100 25 100 #f)
                    (make-obs  375 185 115 30 #f)))

((= mapa-actual 5.2) (set (make-obs  0 0 600 140 #f)
                    (make-obs  20 140 280 40 #f)
                    (make-obs  240 270 110 110 #f)
                    (make-obs  0 270 15 250 #f)
                    (make-obs  565 215 35 300 #f)
                    (make-obs  425 140 45 120 #f)
                    (make-obs  485 140 50 80 #f)
                    (make-obs  350 260 40 70 #f)
                    (make-obs  245 220 40 50 #f)))

((= mapa-actual 5.3) (set (make-obs  0 0 600 110 #f)
                    (make-obs  0 110 250 55 #f)
                    (make-obs  390 110 55 60 #f)
                    (make-obs  510 110 90 60 #f)
                    (make-obs  110 230 170 60 #f)
                    (make-obs  172 342 100 95 #f)
                    (make-obs  115 340 45 50 #f)
                    (make-obs  285 210 45 70 #f)))

((= mapa-actual 5.4) (set (make-obs  0 0 535 140 #f)
                    (make-obs  85 140 400 65 #f)
                    (make-obs  480 340 100 100 #f)))

((= mapa-actual 5.5) (set (make-obs  0 0 600 110 #f)
                    (make-obs  0 0 140 320 #f)
                    (make-obs  325 255 110 200 #f)
                    (make-obs  545 0 55 450 #f)))

((= mapa-actual 5.6) (set (make-obs 0 0 600 80 #f)
                    (make-obs  0 130 140 106 #f)
                    (make-obs  0 290 235 80 #f)
                    (make-obs  365 290 235 80 #f)
                    (make-obs  365 160 145 50 #f)
                    (make-obs  263 130 60 85 #f)
                    (make-obs  0 440 40 60 #f)
                    (make-obs  560 440 40 60 #f)))))



(define (hierbas-mapa)
  (cond ((= mapa-actual 2) (set
                    (make-grass  300 102 150 125)
                    (make-grass  100 302 175 50)
                    (make-grass  50 352 175 50)
                    (make-grass  300 378 50 120)
                    (make-grass  425 302 125 50)
                    (make-grass  375 352 125 50)))
        ((= mapa-actual 2.1) (set
                    (make-grass  250 152 300 125)
                    (make-grass  400 325 150 125)))
        (else (set))))

(define ventana (open-viewport "pokemon" 600 500))
;limites:

(define (escenario ventana)
(begin
  ((draw-pixmap ventana) (dict-ref mapas mapa-actual) (make-posn 0 0) "blue")
  (for/set ([i (obstaculos-mapa)])
    (if (obs-jumpable i) ((draw-rectangle ventana) (make-posn (obs-x i) (obs-y i)) (obs-width i) (obs-height i) "brown")
        ((draw-rectangle ventana) (make-posn (obs-x i) (obs-y i)) (obs-width i) (obs-height i) "blue")))
  (for/set ([i (hierbas-mapa)])
    ((draw-rectangle ventana) (make-posn (grass-x i) (grass-y i)) (grass-width i) (grass-height i) "green") )

))

(define (sprite x y genero a ventana)
  (((draw-pixmap-posn (cond
                        ((= genero 2) (cond
                                        ((= a 1) "mujer iz.png")
                                        ((= a 2) "mujer de.png")
                                        ((= a 3) "mujer ar.png")
                                        ((= a 4) "mujer ab.png")
                                        ))
                        (else         (cond
                                        ((= a 1) "hombre iz.png")
                                        ((= a 2) "hombre de.png")
                                        ((= a 3) "hombre ar.png")
                                        ((= a 4) "hombre ab.png")
                                                             )))   'unknown/mask ) ventana) (make-posn x y)))

(define (muñeco x y flecha genero ventana)
  (cond
    ((equal? flecha 'izquierda)(begin (escenario ventana)(sprite x y genero 1 ventana)))
    ((equal? flecha 'derecha)  (begin (escenario ventana)(sprite x y genero 2 ventana)))
    ((equal? flecha 'arriba)   (begin (escenario ventana)(sprite x y genero 3 ventana)))
    ((equal? flecha 'abajo)    (begin (escenario ventana)(sprite x y genero 4 ventana)))
    ))

;movimiento
(define (movimiento x y tecla genero ventana)
  (if (cambiomapa? x y genero ventana)  
   (cond
    ((equal? tecla 'up)   (movimiento-arriba x y genero ventana))
    ((equal? tecla 'down) (movimiento-abajo x y genero ventana))
    ((equal? tecla 'left) (movimiento-izquierda x y genero ventana))
    ((equal? tecla 'right) (movimiento-derecha x y genero ventana))
    (else (movimiento x y (key-value (get-key-press ventana))genero ventana)))));si alguna tecla hace alguna accion usar este ringlon

(define (movimiento-arriba x y genero ventana)
  (if (puedo-mover? x (- y 10) 'arriba) (moverse x (- y 10) 'arriba genero ventana) (moverse x y 'arriba genero ventana)))

(define (movimiento-abajo x y genero ventana)
  (if (puedo-mover? x (+ y 10) 'abajo) (moverse x (+ y 10) 'abajo genero ventana) (moverse x y 'abajo genero ventana)))

(define (movimiento-izquierda x y genero ventana)
  (if (puedo-mover? (- x 10) y 'izquierda) (moverse (- x 10) y 'izquierda genero ventana) (moverse x y 'izquierda genero ventana)))

(define (movimiento-derecha x y genero ventana)
  (if (puedo-mover? (+ x 10) y 'derecha) (moverse (+ x 10) y 'derecha genero ventana) (moverse x y 'derecha genero ventana)))

(define (moverse x y tecla genero ventana)
  (begin
    (muñeco x y tecla genero ventana)
    ((draw-solid-rectangle ventana) (make-posn 500 465) 60 20 "white")
    ((draw-string ventana) (make-posn 500 475) (string-append "x:" (number->string x) " y:" (number->string y)) "black")
    (revisar-ataque x y ventana)
    (movimiento x y (key-value (get-key-press ventana))genero ventana)))

(define (puedo-mover? x y tecla)
  (and (and (and (>= x -20) (< (+ x SPRITE_WIDTH) 620)) (and (>= (+ y (- SPRITE_HEIGHT SPRITE_FEETS)) 0) (< y 500))) (not (hay-obstaculo? x y tecla)))
 )

(define (hay-obstaculo? x y tecla)
  (begin
    (define coincidencias (for/set ([i (obstaculos-mapa)]
              #:when (and (not (and (equal? tecla 'abajo) (obs-jumpable i))) (and (and (>= (+ x SPRITE_WIDTH) (obs-x i)) (<= x (+ (obs-x i) (obs-width i))))
         (and (>= (+ y SPRITE_HEIGHT) (obs-y i)) (<= (+ y (- SPRITE_HEIGHT SPRITE_FEETS)) (+ (obs-y i) (obs-height i)))))))
      i))
    (> (set-count coincidencias) 0) ))

(define (revisar-ataque x y ventana)
  (if (and (sobre-hierba? x y) (recibir-ataque?)) ((draw-solid-rectangle ventana) (make-posn x y) SPRITE_WIDTH 20 "red")) )

(define (sobre-hierba? x y)
  (begin
    (define coincidencias (for/set ([i (hierbas-mapa)]
                                    #:when (and (and (> (+ x SPRITE_WIDTH) (grass-x i)) (< x (+ (grass-x i) (grass-width i))))
                                                (and (> (+ y SPRITE_HEIGHT) (grass-y i)) (< (+ y (- SPRITE_HEIGHT SPRITE_FEETS)) (+ (grass-y i) (grass-height i))))))
                            i))
    (> (set-count coincidencias) 0) ))

(define (recibir-ataque?)
  (> (random 100) (- 100 PROBABILIDAD_ATAQUE)) )


(define-struct pokemon (tipo nivel))

(define uno (make-pokemon "charmander" 15))
(pokemon-tipo uno)


(define mis-pokemones (hash))
(set! mis-pokemones (dict-set mis-pokemones 1 (make-pokemon "bulbasur" 4)))
(set! mis-pokemones (dict-set mis-pokemones 2 (make-pokemon "charmander" 6)))
(dict-ref mis-pokemones 2)





(movimiento 90 190 'up 1 ventana)