;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname free-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; My world program  (make this more specific)

;; =================
;; Constants:

(define WIDTH 640)
(define HEIGHT 480)
(define MTS (empty-scene 640 480))
(define BALL (circle 20 "solid" "red"))
(define DX 5)
(define DY 5)
(define MID-X (/ WIDTH 2))
(define MID-Y (/ HEIGHT 2))


;; =================
;; Data definitions:

(define-struct ball (x y dx dy))
;; Ball is (make-ball Natural[0,WIDTH] Natural[0,HEIGHT] Natural Natural)
;; interp. (make-ball x y dx dy) is a ball with:
;;     - x and y are coordinates in the scene
;;     - dx and dy are velocities in x and y directions
(define B1 (make-ball  MID-X       MID-Y         5 -5)) ; ball in center, moving NE
(define B2 (make-ball  (- WIDTH 2) MID-Y         5 -5))  ; ball next to right wall, moving NE
(define B3 (make-ball  MID-X       3            -5 -5)) ; ball next to up wall, moving NW
(define B4 (make-ball  1           MID-Y        -5 -5)) ; ball next to left wall, moving NW
(define B5 (make-ball  MID-X       (- HEIGHT 4)  5  5))  ; ball next to down wall, moving SE
(define B6 (make-ball  (- WIDTH 1) 1             5 -5)) ; ball in NE corner, moving NE

#;
(define (fn-for-ball b)
  (... (ball-x b)      ; Natural[0,WIDTH]
       (ball-y b)      ; Natural[0,HEIGHT]
       (ball-dx b)     ; Natural
       (ball-dy b)))   ; Natural

;; Template rules used:
;; Compound: 4 fields


;; =================
;; Functions:

;; Ball -> Ball
;; start the world with (main B2)
;; ball is in the middle, moving South-East
;; 
(define (main b)
  (big-bang b                         ; Ball
            (on-tick   next-ball)     ; Ball -> Ball
            (to-draw   render-image)  ; Ball -> Image
            (on-key    handle-key)))  ; Ball KeyEvent -> Ball

;; Ball -> Ball
;; produce the next ball with appropriate velocities in x and y directions
(check-expect (next-ball B1) (make-ball  (+ MID-X  5)  (+ MID-Y -5)   5  -5))
(check-expect (next-ball B2) (make-ball  WIDTH         (+ MID-Y -5)  -5  -5))
(check-expect (next-ball B3) (make-ball  (+ MID-X -5)  0             -5   5))
(check-expect (next-ball B4) (make-ball  0             (+ MID-Y -5)   5  -5))
(check-expect (next-ball B5) (make-ball  (+ MID-X  5)  HEIGHT         5  -5))
(check-expect (next-ball B6) (make-ball  WIDTH         0             -5   5))
  
(define (next-ball b) (make-ball 0 0 0 0)) ; stub

;; <template from Ball>
#;
(define (next-ball b)
  (... (ball-x b)      ; Natural[0,WIDTH]
       (ball-y b)      ; Natural[0,HEIGHT]
       (ball-dx b)     ; Natural
       (ball-dy b)))   ; Natural

(define (next-ball b)
  (cond [(and (< (+ (ball-x b) (ball-dx b)) 0) (< (+ (ball-y b) (ball-dy b)) 0)) (make-ball 0 0 (- (ball-dx b)) (- (ball-dy b)))]
        [(and (< (+ (ball-x b) (ball-dx b)) 0) (> (+ (ball-y b) (ball-dy b)) HEIGHT)) (make-ball 0 HEIGHT (- (ball-dx b)) (- (ball-dy b)))]
        [(and (> (+ (ball-x b) (ball-dx b)) WIDTH) (< (+ (ball-y b) (ball-dy b)) 0)) (make-ball WIDTH 0 (- (ball-dx b)) (- (ball-dy b)))]
   ... (ball-x b)      ; Natural[0,WIDTH]
       (ball-y b)      ; Natural[0,HEIGHT]
       (ball-dx b)     ; Natural
       (ball-dy b)))   ; Natural


;; Ball -> Image
;; render ... 
;; !!!
(define (render-image b) ...)

;; Ball KeyEvent -> Ball
;; handle key ...
;; !!!
(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))