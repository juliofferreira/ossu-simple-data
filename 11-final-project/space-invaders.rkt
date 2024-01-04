;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 70)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define BLANK (square 0 "solid" "white"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y xdir))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader is moving left if xdir = -1
;;         the invader is moving right if xdir = 1

(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI0 empty)
(define LOI1 (list I1))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)           
                   (fn-for-loi (rest loi)))]))


;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of missiles

(define LOM0 empty)
(define LOM1 (list M1))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom)           
                   (fn-for-lom (rest lom)))]))



;; =================
;; Functions:

;; Game -> Game
;; start world with (main G0)

(define (main gs)
  (big-bang gs
    (on-tick next-game)
    (to-draw render-game)
    (stop-when finish-game?)
    (on-key handle-key)))


;; Game -> Game
;; produce the next game state
(check-expect (next-game G0)
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))     ; going right
(check-expect (next-game (make-game empty empty T2))
              (make-game empty empty (make-tank (- 50 TANK-SPEED) -1)))  ;going left

(define (next-game s)
  (next-game-intermediate (game-invaders s)
                          (next-lom (game-missiles s))
                          (next-tank (game-tank s))))


;; ListOfInvader ListOfMissile Tank -> Game
;; produce next game, function purpose is to reduce times next-lom has to run
;;  by receiving it as parameter
(define (next-game-intermediate old-loi new-lom new-tank)
  (make-game (next-loi (spawn-invader old-loi) new-lom) new-lom new-tank))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; produce the next list of invader state
(check-expect (next-loi empty empty) empty)
(check-expect (next-loi (cons (make-invader 30 50 -1) empty) empty)
              (cons (make-invader (+ (* INVADER-X-SPEED -1) 30) (+ INVADER-Y-SPEED 50) -1) empty))

(define (next-loi loi new-lom)
  (cond [(empty? loi) empty]
        [else (if (areColliding? new-lom (first loi))
                  (next-loi (rest loi) new-lom)           
                  (cons (next-invader (first loi)) (next-loi (rest loi) new-lom)))]))


;; ListOfInvader -> ListOfInvader
;; produce new list of invader with a new invader

(define (spawn-invader loi)
  (if (= (random INVADE-RATE) (- INVADE-RATE 1))
      (cons (make-invader (random WIDTH) 0 (if (= (random 2) 0) 1 -1)) loi)
      loi))


;; Invader -> Invader
;; produce next invader
(check-expect (next-invader (make-invader 30 50 -1))
              (make-invader (+ (* INVADER-X-SPEED -1) 30) (+ INVADER-Y-SPEED 50) -1))
(check-expect (next-invader (make-invader 20 60 1))
              (make-invader (+ (* INVADER-X-SPEED 1) 20) (+ INVADER-Y-SPEED 60) 1))
(check-expect (next-invader (make-invader 0 60 -1))
              (make-invader (+ 0 INVADER-WIDTH/2) (+ INVADER-Y-SPEED 60) 1))
(check-expect (next-invader (make-invader WIDTH 60 1))
              (make-invader (- WIDTH INVADER-WIDTH/2) (+ INVADER-Y-SPEED 60) -1))

(define (next-invader i)
  (cond [(and (check-side-collision (+ (invader-x i) (* INVADER-X-SPEED (invader-xdir i))) INVADER)
              (= (invader-xdir i) -1))
         (make-invader (+ 0 INVADER-WIDTH/2) (+ INVADER-Y-SPEED (invader-y i)) 1)]
        [(and (check-side-collision (+ (invader-x i) (* INVADER-X-SPEED (invader-xdir i))) INVADER)
              (= (invader-xdir i) 1))
         (make-invader (- WIDTH INVADER-WIDTH/2) (+ INVADER-Y-SPEED (invader-y i)) -1)]
        [else (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-xdir i)))
                            (+ INVADER-Y-SPEED (invader-y i))
                            (invader-xdir i))]))


;; ListOfMissile -> ListOfMissile
;; produce the next list of missile state
(check-expect (next-lom empty) empty)
(check-expect (next-lom (cons (make-missile 50 50) empty))
              (cons (make-missile 50 (- 50 MISSILE-SPEED)) empty))
(check-expect (next-lom (cons (make-missile 30 70) (cons (make-missile 50 50) empty)))
              (cons (make-missile 30 (- 70 MISSILE-SPEED))
                    (cons (make-missile 50 (- 50 MISSILE-SPEED)) empty)))
(check-expect (next-lom (cons (make-missile 50 -9) empty)) empty)                        ;;pass top wall fully
(check-expect (next-lom (cons (make-missile 30 70) (cons (make-missile 50 -10) empty)))  ;;pass top wall fully
              (cons (make-missile 30 (- 70 MISSILE-SPEED)) empty))
(check-expect (next-lom (cons (make-missile 50 5) empty))                                ;;pass top wall partially
              (cons (make-missile 50 (- 5 MISSILE-SPEED)) empty))                        
(check-expect (next-lom (cons (make-missile 30 70) (cons (make-missile 50 6) empty)))   ;;pass top wall partially
              (cons (make-missile 30 (- 70 MISSILE-SPEED))
                    (cons (make-missile 50 (- 6 MISSILE-SPEED)) empty)))

(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else (if (delete-missile? (missile-y (next-missile (first lom))))
                  (next-lom (rest lom))
                  (cons (next-missile (first lom)) (next-lom (rest lom))))]))


;; Missile -> Missile
;; produce next missile
(check-expect (next-missile (make-missile 50 50)) (make-missile 50 (- 50 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 30 70)) (make-missile 30 (- 70 MISSILE-SPEED)))

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Number Image -> Boolean
;; produce true if image is colliding with top wall
(check-expect (delete-missile? 50) false)
(check-expect (delete-missile? 0) false)
(check-expect (delete-missile? -5) false)
(check-expect (delete-missile? -10) true)

(define (delete-missile? y)
  (if (<= (+ y MISSILE-HEIGHT/2) 0) 
      true
      false))


;; Missile Invader -> Boolean
;; produce true if missile is colliding with invader
(check-expect (isColliding? (make-missile 50 50) (make-invader 50 50 1)) true)
(check-expect (isColliding? (make-missile 50 50) (make-invader 100 100 -1)) false)

(define (isColliding? m i)
  (if (or (> (- (missile-x m) MISSILE-WIDTH/2) (+ (invader-x i) INVADER-WIDTH/2))
          (< (+ (missile-x m) MISSILE-WIDTH/2) (- (invader-x i) INVADER-WIDTH/2))
          (> (- (missile-y m) MISSILE-HEIGHT/2) (+ (invader-y i) INVADER-HEIGHT/2))
          (< (+ (missile-y m) MISSILE-HEIGHT/2) (- (invader-y i) INVADER-HEIGHT/2)))
      false
      true))


;; ListOfMissile Invader -> Boolean
;; produces true if invader is colliding with any missile

(define (areColliding? lom i)
  (cond [(or (empty? lom)
             (empty? i))
         false]
        [else (if (isColliding? (first lom) i)
                  true
                  (areColliding? (rest lom) i))]))


;; Tank -> Tank
;; produce next tank state
(check-expect (next-tank T1) (make-tank 52 1))        ;going right
(check-expect (next-tank T2) (make-tank 48 -1))       ;going left
(check-expect (next-tank (make-tank 1 -1))            ;changing direction when hitting left wall
              (make-tank (+ 0 TANK-WIDTH/2) 1))  
(check-expect (next-tank (make-tank (- WIDTH 1) 1))   ;changing direction when hitting right wall
              (make-tank (- WIDTH TANK-WIDTH/2) -1))  
              
(define (next-tank t)
  (cond [(and (check-side-collision (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK)
              (= (tank-dir t) -1))
         (make-tank (+ 0 TANK-WIDTH/2) 1)]
        [(and (check-side-collision (+ (tank-x t) (* TANK-SPEED (tank-dir t))) TANK)
              (= (tank-dir t) 1))
         (make-tank (- WIDTH TANK-WIDTH/2) -1)]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))


;; Number Image -> Boolean
;; produce true if image is colliding with side wall
(check-expect (check-side-collision 50 TANK) false)
(check-expect (check-side-collision 10 TANK) true)
(check-expect (check-side-collision 0 TANK) true)
(check-expect (check-side-collision -1 TANK) true)
(check-expect (check-side-collision (- WIDTH 9) TANK) true)
(check-expect (check-side-collision WIDTH TANK) true)
(check-expect (check-side-collision (+ WIDTH 1) TANK) true)

(define (check-side-collision x img)
  (if (or (>= (+ x (/ (image-width img) 2)) WIDTH)
          (<= (- x (/ (image-width img) 2)) 0))
      true
      false))


;; Game -> Image
;; produce image with given game state
(check-expect (render-game G0) (render-tank (game-tank G0) BACKGROUND))
(check-expect (render-game G2)
              (render-loi (game-invaders G2)
                          (render-lom (game-missiles G2)
                                      (render-tank (game-tank G2) BACKGROUND))))

(define (render-game s)
  (put-image BLANK 0 0 (render-loi (game-invaders s)
                                   (render-lom (game-missiles s)
                                               (render-tank (game-tank s) BACKGROUND)))))


;; ListOfInvader Image -> Image
;; produce invaders image with given list of invaders, in given image
(check-expect (render-loi empty BACKGROUND) BACKGROUND)
(check-expect (render-loi (cons (make-invader 100 100 12) empty) BACKGROUND)
              (place-image INVADER 100 100 BACKGROUND))
(check-expect (render-loi (cons (make-invader 50 50 10) (cons (make-invader 100 100 11) empty)) BACKGROUND)
              (place-image INVADER 50 50 (place-image INVADER 100 100 BACKGROUND)))

(define (render-loi loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))  
                      (render-loi (rest loi) img))]))


;; ListOfMissile Image -> Image
;; produce missiles image with given list of missile, in given image
(check-expect (render-lom empty BACKGROUND) BACKGROUND)
(check-expect (render-lom (cons (make-missile 100 100) empty) BACKGROUND)
              (place-image MISSILE 100 100 BACKGROUND))
(check-expect (render-lom (cons (make-missile 50 50) (cons (make-missile 100 100) empty)) BACKGROUND)
              (place-image MISSILE 50 50 (place-image MISSILE 100 100 BACKGROUND)))

(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))  
                      (render-lom (rest lom) img))]))


;; Tank Image -> Image
;; produce tank image with given tank, in given image
(check-expect (render-tank (make-tank 50 1) BACKGROUND)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2 1) BACKGROUND))
(check-expect (render-tank (make-tank 30 -1) BACKGROUND)
              (place-image TANK 30 (- HEIGHT TANK-HEIGHT/2 1) BACKGROUND))

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2 1) img))


;; Game -> Boolean
;; end game if an invader reaches the bottom of screen

(define (finish-game? g)
  (invaders-hit-bottom? (game-invaders g)))


;; ListOfInvader -> Boolean
;; produce true if any invader is colliding with bottom wall
(check-expect (invaders-hit-bottom? empty) false)
(check-expect (invaders-hit-bottom? (cons (make-invader 10 20 1) empty)) false)
(check-expect (invaders-hit-bottom? (cons (make-invader 40 HEIGHT 1) empty)) true)
(check-expect (invaders-hit-bottom? (cons (make-invader 60 (- HEIGHT 2) 1) empty)) true)

(define (invaders-hit-bottom? loi)
  (cond [(empty? loi) false]
        [else (if (invader-hit-bottom? (first loi))
                  true
                  (invaders-hit-bottom? (rest loi)))]))


;; Invader -> Boolean
;; produce true if invader is colliding with bottom wall
(check-expect (invader-hit-bottom? (make-invader 10 20 1)) false)
(check-expect (invader-hit-bottom? (make-invader 40 HEIGHT 1)) true)
(check-expect (invader-hit-bottom? (make-invader 60 (- HEIGHT 2) 1)) true)

(define (invader-hit-bottom? i)
  (if (>= (+ INVADER-HEIGHT/2 (invader-y i)) HEIGHT) 
      true
      false))


;; Game KeyEvent -> Game
;; handle spacebar and left or right arrow keys press
(check-expect (handle-key G0 " ") (handle-spacebar G0))
(check-expect (handle-key G0 "left") (handle-arrow G0 "left"))
(check-expect (handle-key G0 "right") (handle-arrow G0 "right"))
(check-expect (handle-key G0 "a") G0)

(define (handle-key g ke)
  (cond [(key=? " " ke) (handle-spacebar g)]
        [(or (key=? "left" ke) (key=? "right" ke))
         (handle-arrow g ke)]
        [else g]))


;; Game KeyEvent -> Game
;; change tank direction when pressing left or right arrow key
(check-expect (handle-arrow G0 "left") (make-game empty empty (make-tank (tank-x (game-tank G0)) -1)))
(check-expect (handle-arrow G0 "right") (make-game empty empty (make-tank (tank-x (game-tank G0)) 1)))
(check-expect (handle-arrow G0 "f") G0)

(define (handle-arrow g ke)
  (if (key=? "left" ke)
      (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))
      (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))))


;; Game -> Game
;; shoot a missile when pressing space bar
(check-expect (handle-spacebar G0) (make-game
                                    empty
                                    (cons (make-missile (tank-x T0) (- HEIGHT (+  (* 2 TANK-HEIGHT/2) MISSILE-HEIGHT/2))) empty)
                                    T0))

(define (handle-spacebar g)
  (make-game (game-invaders g)
             (cons (make-missile (tank-x (game-tank g)) (- HEIGHT (+ (* 2 TANK-HEIGHT/2) MISSILE-HEIGHT/2))) (game-missiles g))
             (game-tank g)))