;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-DX 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-DY 1.5)
(define TANK-DX 2)
(define MISSILE-DY 10)

(define MARGIN -10) ; remove object if beyound the screen edge + margin

(define HIT-RANGE 10)

(define INVADE-RATE 100) ; what is this?

(define MTS (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick:
;;                                          - left if dir -1,
;;                                          - right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. list of invaders
(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I2  LOI1))
(define LOI3 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi)) ; reference
                   (fn-for-loi  (rest loi)))])) ; self-reference


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; interp. a list of missiles
(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M2  LOM1))
(define LOM3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom)) ; reference
                   (fn-for-lom  (rest lom)))])) ; self-reference


(define G0 (make-game empty empty T0)) ; tank in middle, going right
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions


;; Game -> Game
;; starts the world with only the tank in the middle
;; start by running (main G0)

(define (main g)
  (big-bang g                           ; Game
            (on-tick   handle-tick)     ; Game -> Game
            (to-draw   render-game)     ; Game -> Image
            (stop-when finish-game?)    ; Game -> Boolean
            (on-key    handle-keys)))   ; Game KeyEvent -> Game

;; Game -> Game
;; composition: process collisions and them move all elements
(check-expect (handle-tick G0)
              (make-game empty
                         empty
                         (make-tank (+ (/ WIDTH 2) TANK-DX)
                                    1)))
(check-expect (handle-tick G1)
              (make-game empty
                         empty
                         (make-tank (+ 50 TANK-DX)
                                    1)))
(check-expect (handle-tick
               (make-game (list (make-invader 150 100 12) ;not landed, going right, hit by missile
                                (make-invader 150 HEIGHT -10)) ;exactly landed, moving left
                          (list (make-missile 150 300)                               ; not hit
                                (make-missile
                                 (invader-x (make-invader 150 100 12))
                                 (+ (invader-y (make-invader 150 100 12)) 10))) ; exactly hits
                          (make-tank 50 1))) ; tank going right
              (make-game (list (make-invader (- 150 10) (+ HEIGHT 10) -10))
                         (list (make-missile 150 (+ 300 MISSILE-DY)))
                         (make-tank (+ 50 TANK-DX) 1))) ; G3 -> new state

;(define (handle-tick g) (make-game empty empty (make-tank (/ WIDTH 2) 1)));stub: G0
;; <template from Game>
(define (handle-tick g)
  (next-game (process-collisions g)))


;; Game -> Game
;; detect collisions and remove corresponding invaders and rockets
;; !!!
(define (process-collisions g) g) ; stub


;; Game -> Game
;; produce next game by properly moving invaders, missiles and tank
;; tests handled by helpers

;(define (next-game g) g) ; stub
(define (next-game s)
  (make-game (next-invaders (game-invaders s))
             (next-missiles (game-missiles s))
             (next-tank     (game-tank s))))


;; ListOfInvader -> ListOfInvader
;; produce next list of invaders
;; !!!
(define (next-invaders loi) loi) ; stub


;; ListOfMissile -> ListOfMissile
;; produce next list of missiles
(check-expect (next-missiles empty) empty) ; base case
(check-expect (next-missiles (list (make-missile 50 75)                   ; move missile
                                   (make-missile 0 HEIGHT)                ; move missile
                                   (make-missile 100 MARGIN)))            ; remove missile
              (list (make-missile 50 (- 75 MISSILE-DY)) ; minus because flies up
                    (make-missile 0 (- HEIGHT MISSILE-DY))))

;(define (next-missiles lom) lom) ; stub
(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (<= (missile-y (first lom)) MARGIN)
                  (next-missiles  (rest lom))
                  (cons (next-missile (first lom))
                        (next-missiles  (rest lom))))]))


;; Missile -> Missile
;; move missile by MISSILE-DY
(check-expect (next-missile M1) (make-missile 150 (- 300 MISSILE-DY)))
 
;(define (next-missile m) (make-missile 0 0)) ; stub
(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-DY)))


;; Tank -> Tank
;; produce next tank with correctly changing direction at edges of screen
(check-expect (next-tank (make-tank (/ WIDTH 2) 1))    ; center, going right
              (make-tank (+ (/ WIDTH 2) TANK-DX) 1))
(check-expect (next-tank (make-tank (/ WIDTH 2) -1))   ; center, going left
              (make-tank (- (/ WIDTH 2) TANK-DX) -1))
(check-expect (next-tank (make-tank (- WIDTH 1) 1))    ; right edge, going right
              (make-tank WIDTH -1))
(check-expect (next-tank (make-tank 1 -1))             ; left edge, going left
              (make-tank 0 1))

;(define (next-tank t) (make-tank 50 1)) ; stub
(define (next-tank t)
  (cond [(and (> (+ (tank-x t) TANK-DX) WIDTH) (= (tank-dir t) 1)) ; right edge, going right
         (make-tank WIDTH -1)]
        [(and (< (- (tank-x t) TANK-DX) 0) (= (tank-dir t) -1))    ; left edge, going left
         (make-tank 0 1)]
        [(= (tank-dir t) 1)                                        ; no edge, going right
         (make-tank (+ (tank-x t) TANK-DX) (tank-dir t))]
        [(= (tank-dir t) -1)                                       ; no edge, going left
         (make-tank (- (tank-x t) TANK-DX) (tank-dir t))]))


;; Game -> Image
;; render ... 
;; !!!
(define (render-game g) ...)


;; Game -> Boolean
;; check if game needs to end
;; !!!
(define (finish-game? g) ...)


;; Game KeyEvent -> Game
;; change tank's movement direction, fire rockets
;; !!!
;; <template from the HtDW recipe>
(define (handle-keys g ke)
  (cond [(key=? ke " ") (... g)]
        [else 
         (... g)]))
