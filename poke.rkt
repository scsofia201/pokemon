(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define-struct player (nombre  genero objetos dinero pokemon))


(define (fondooak ventana)
  (begin
    ((draw-viewport ventana) (make-rgb 0.6 0.850922 0.917))
    (((draw-pixmap-posn "profesor oak.png" 'unknown/mask ) ventana) (make-posn 175 0))
    (((draw-pixmap-posn "nueve1.png" 'unknown/mask ) ventana) (make-posn 0 300))
    ))

(define (dialogo-oak cont datosplayer ventana)
  (if (equal? (key-value (get-key-press ventana)) (integer->char 13))
      (begin ((draw-viewport ventana) "white")
             (if (>= 3 cont)
                 (begin
                   (if (= cont 1)(begin((draw-viewport ventana) "black")(sleep 3)))
                   (fondooak ventana)
                   (cond
                     ((= cont 1)
                      (begin
                        (sleep 1)((draw-string ventana) (make-posn 50 330) "¡Hola a todos! ¡Bienvenidos al mundo de POKÉMON! ¡Me llamo OAK!" "black")
                        (sleep 1) ((draw-string ventana) (make-posn 50 355) "¡Pero la gente me llama el PROFESOR POKÉMON! ¡Este mundo " "black")
                        (sleep 1) ((draw-string ventana) (make-posn 50 380) "está habitado por unas criaturas llamadas POKÉMON! Para algunos," "black")
                        (sleep 1) ((draw-string ventana) (make-posn 50 405) " los POKÉMON son mascotas. Pero otros los usan para pelear." "black")
                        (dialogo-oak (+ cont 1) datosplayer ventana)))
                     ((= cont 2)
                      (begin
                        (sleep 1) ((draw-string ventana) (make-posn 50 350) "En cuanto a mí... Estudio a los POKÉMON como profesión." "black")
                        (sleep 1) ((draw-string ventana) (make-posn 50 370) "Asi que tu nombre es:" "black")
                        (sleep 1) ((draw-string ventana) (make-posn 50 390) (player-nombre datosplayer) "black")
                        (dialogo-oak (+ cont 1) datosplayer ventana)))
                     ((= cont 3)
                      (begin
                        (sleep 1)((draw-string ventana) (make-posn 50 350) "¡Tu propia leyenda POKÉMON está a punto de comenzar!" "black")
                        (sleep 1)((draw-string ventana) (make-posn 50 370) "¡Te espera un mundo de sueños y aventuras con los POKÉMON!" "black")
                        (sleep 1)((draw-string ventana) (make-posn 50 390) "¡Adelante!" "black")
                        (dialogo-oak (+ cont 1) datosplayer ventana)))))(begin ((draw-viewport ventana) "black") (sleep 2))
                                                                        )) (dialogo-oak cont datosplayer ventana)) )


(define (sprite x y datosplayer a ventana)
  (((draw-pixmap-posn (cond
                        ((= (player-genero datosplayer) 2) (cond
                                                             ((= a 1) "mujer iz.png")
                                                             ((= a 2) "mujer de.png")
                                                             ((= a 3) "mujer ar.png")
                                                             ((= a 4) "mujer ab.png")))
                        (else (cond
                                ((= a 1) "hombre iz.png")
                                ((= a 2) "hombre de.png")
                                ((= a 3) "hombre ar.png")
                                ((= a 4) "hombre ab.png")))) 'unknown/mask ) ventana) (make-posn x y)))  



(define (llamado x y datosplayer ventana num)
  (movimiento x y (key-value (get-key-press ventana))datosplayer ventana num))

(define (condiciones x y d ventana mapa)
  (cond
    ((= mapa 1)  (cond
                   
                   ((> x 520) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
                   ((< x 50) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
                   ((> y 420) (begin (muñeco x (- y 10) 'abajo d ventana mapa)  (llamado x (- y 10) d ventana mapa)))
                   (else #t) ) )
   ((= mapa 2)  (cond
                   ((> x 520) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
                   ((< x 50) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
                   (else #t) ) )
      ((= mapa 2.1)  (cond
                   ((> x 520) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
                   ((< x 50) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
                   (else #t) ) )
       ((= mapa 3)  (cond
                  ((< x 170) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
                   (else #t) ) )
       ((= mapa 3.1)  (cond
                 ((< y -10) (begin (muñeco x (+ y 10) 'arriba d ventana mapa)  (llamado x (+ y 10) d ventana mapa)))
                  (else #t) ) )

       ((= mapa 3.2)  (cond
                  ((> x 420) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
                   (else #t) ) )
       ((= mapa 3.3)  (cond
                  ((> x 420) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
                 ((< y 100) (begin (muñeco x (+ y 10) 'arriba d ventana mapa)  (llamado x (+ y 10) d ventana mapa)))
                  (else #t) ) )

       ((= mapa 4)  (cond
                 ((< y 100) (begin (muñeco x (+ y 10) 'arriba d ventana mapa)  (llamado x (+ y 10) d ventana mapa)))
                 ((< x 50) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
               (else #t)) )
        ((= mapa 4.1)  (cond
                 ((< y 100) (begin (muñeco x (+ y 10) 'arriba d ventana mapa)  (llamado x (+ y 10) d ventana mapa)))
                 ((> x 520) (begin (muñeco (- x 10) y 'derecha d ventana mapa)  (llamado (- x 10) y d ventana mapa)))
               (else #t)) )
        ((= mapa 4.2)  (cond
                 ((> y 450) (begin (muñeco x (- y 10) 'abajo d ventana mapa)  (llamado x (- y 10) d ventana mapa)))
                 ((< x 50) (begin (muñeco (+ x 10) y 'izquierda d ventana mapa)  (llamado (+ x 10) y d ventana mapa)))
               (else #t)))
        ((= mapa 4.3)  (cond
                 ((> y 450) (begin (muñeco x (- y 10) 'abajo d ventana mapa)  (llamado x (- y 10) d ventana mapa)))
                 
               (else #t)))



      
    (else #t)))

(define (mapa1 x ventana)
  (cond
    ((= x 1)   ((draw-pixmap ventana) "mapa 1 a.jpg" (make-posn 0 0) "blue"))
    ((= x 2)   ((draw-pixmap ventana) "mapa 2 a.jpg" (make-posn 0 0) "blue"))
    ((= x 2.1) ((draw-pixmap ventana) "mapa 2 b.jpg" (make-posn 0 0) "blue"))
    ((= x 3)   ((draw-pixmap ventana) "mapa 3 a.jpg" (make-posn 0 0) "blue"))
    ((= x 3.1) ((draw-pixmap ventana) "mapa 3 b.jpg" (make-posn 0 0) "blue"))
    ((= x 3.2) ((draw-pixmap ventana) "mapa 3 c.jpg" (make-posn 0 0) "blue"))
    ((= x 3.3) ((draw-pixmap ventana) "mapa 3 d.jpg" (make-posn 0 0) "blue"))
    ((= x 4)   ((draw-pixmap ventana) "mapa 4 a.jpg" (make-posn 0 0) "blue"))
    ((= x 4.1) ((draw-pixmap ventana) "mapa 4 b.jpg" (make-posn 0 0) "blue"))
    ((= x 4.2) ((draw-pixmap ventana) "mapa 4 c.jpg" (make-posn 0 0) "blue"))
    ((= x 4.3) ((draw-pixmap ventana) "mapa 4 d.jpg" (make-posn 0 0) "blue"))
    ))
    
(define (muñeco x y flecha datosplayer ventana mapa)
  (cond
    ((equal? flecha 'izquierda)(begin (mapa1 mapa ventana)(sprite x y datosplayer 1 ventana)))
    ((equal? flecha 'derecha)  (begin (mapa1 mapa ventana)(sprite x y datosplayer 2 ventana)))
    ((equal? flecha 'arriba)   (begin (mapa1 mapa ventana)(sprite x y datosplayer 3 ventana)))
    ((equal? flecha 'abajo)    (begin (mapa1 mapa ventana)(sprite x y datosplayer 4 ventana)))
    ))

;movimiento
(define (movimiento x y tecla datosplayer ventana mapa)
  (if (equal? #t (condiciones x y datosplayer ventana mapa))
      (if (equal? #t (escenario x y datosplayer ventana mapa))
          (cond
            ((equal? tecla 'up)   (begin (muñeco x (- y 10) 'arriba datosplayer ventana mapa)   (movimiento x (- y 10) (key-value (get-key-press ventana))datosplayer ventana mapa)))
            ((equal? tecla 'down) (begin (muñeco x (+ y 10) 'abajo datosplayer ventana mapa)    (movimiento x (+ y 10) (key-value (get-key-press ventana))datosplayer ventana mapa)))
            ((equal? tecla 'left) (begin (muñeco (- x 10) y 'izquierda datosplayer ventana mapa)(movimiento (- x 10) y (key-value (get-key-press ventana))datosplayer ventana mapa)))
            ((equal? tecla 'right)(begin (muñeco (+ x 10) y 'derecha datosplayer ventana mapa)  (movimiento (+ x 10) y (key-value (get-key-press ventana))datosplayer ventana mapa)))
            (else (movimiento x y (key-value (get-key-press ventana))datosplayer ventana mapa))));si alguna tecla hace alguna accion usar este ringlon
      ))
(define (player)
  (begin
    (make-player
     (and (displayln "bienvenido a pokemon, por favor ingrese el nombre del jugador (no olvide ponerlo entre comillas)") (read))
     (and (displayln "por favor indique su genero (1/hombre 2/mujer) ") (read))
     0
     0
     0)))

(define (escenario x y d ventana mapa)
  (cond
    ((= mapa 1) (if (< y 10) (llamado x 490 d ventana 2) #t))
    ((= mapa 2) (cond ((> y 490) (llamado x 10 d ventana 1))
                      ((< y 10) (llamado x 490 d ventana 2.1))(else #t)))
    ((= mapa 2.1) (cond ((> y 490) (llamado x 10 d ventana 2))
                        ((< y 10) (if (< x 285) (llamado 558 489 d ventana 3)
                                      (llamado 10 490 d ventana 3.2))) (else #t)))
    ((= mapa 3) (cond ((> x 591) (llamado -20 y d ventana 3.2))
                      ((> y 490) (llamado 260 10 d ventana 2.1))
                      ((< y 10)  (llamado x 490 d ventana 3.1))(else #t)))
    ((= mapa 3.1) (cond ((> x 591) (llamado -20 y d ventana 3.3))
                        ((< x 10)  (llamado 589 10 d ventana 4.3))
                        ((> y 490)  (llamado x 10 d ventana 3)) (else #t)))
    ((= mapa 3.2) (cond ((< x -20) (llamado 590 y d ventana 3))
                        ((> y 490) (llamado 310 10 d ventana 2.1))
                        ((< y 10)  (llamado x 490 d ventana 3.3)) (else #t)))
    ((= mapa 3.3) (cond ((< x -20) (llamado 590 y d ventana 3.1))
                        ((> y 490) (llamado x 10 d ventana 3.2))(else #t)))
    ((= mapa 4) (cond ((> y 490) (llamado x -20 d ventana 4.2))
                      ((> x 590) (llamado -20 y d ventana 4.1)) (else #t)))
    ((= mapa 4.1) (cond ((> y 490) (llamado x -20 d ventana 4.3))
                        ((< x -20) (llamado 590 y d ventana 4)) (else #t)))
    ((= mapa 4.2) (cond ((< y -20) (llamado x 490 d ventana 4))
                        ((> x 590) (llamado 10 y d ventana 4.3)) (else #t)))
    ((= mapa 4.3)(cond ((> x 600)(llamado 10 450 d ventana 3.1))
                       ((< x 10) (llamado 590 y d ventana 4.2))
                       ((< y -20) (llamado x 490 d ventana 4.1)) (else #t)))) )

(define (principal)
  (begin   
    (define datosplayer (player))
    (define ventana (open-viewport "pokemon" 600 500))
    ;((draw-viewport ventana) "black")
    ;(((draw-pixmap-posn "marca_UTP.png" 'unknown/mask ) ventana) (make-posn 175 175))(sleep 7)
    ;(play-sound "musica inicio.mp3" #t)
    ;((draw-pixmap ventana) "portada inicio.jpg" (make-posn 0 0) "blue")
    ;(dialogo-oak 1 datosplayer ventana)
    ((draw-pixmap ventana) "mapa 1 a.jpg" (make-posn 0 0) "blue")
    (movimiento 300 250 'up datosplayer ventana 1)
    )) (principal)
       