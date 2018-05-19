;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;(circle 20 "solid" "red")

;(text "Hello, world!" 36 "blue")

;(above (circle 10 "solid" "green")
;       (circle 20 "solid" "orange")
;       (circle 30 "solid" "blue"))

;(beside (circle 10 "solid" "green")
;       (circle 20 "solid" "orange")
;       (circle 30 "solid" "blue"))

(overlay (circle 10 "solid" "red")
       (circle 20 "solid" "white")
       (circle 30 "solid" "blue"))