;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname MASTER) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| Problem Set 8
   Authors: Nadine Shaalan and Bahar 
   Emails:  Shaalan.n@husky.neu.edu and 
|#
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Block is a (make-block Number Number String)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;; ---- CONSTANTS: Properties that will remain the same
 
(define GRID-SIZE 20)     ; width of a game board sqaure in pixels
(define BOARD-HEIGHT 20)  ; height in grid squares
(define BOARD-WIDTH 10)   ; width in grid squares
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT)) ; Board height in pixels
(define BOARD-WIDTH-PIXELS  (* GRID-SIZE BOARD-WIDTH))  ; Board width in pixels

(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))

;;BSet examples

(define bset0 (list (make-block 5 10 'blue)
                    (make-block 6 10 'blue)
                    (make-block 7 10 'blue)
                    (make-block 8 10 'blue)))

(define bset1 (list (make-block 6 10 'blue) 
                    (make-block 7 10 'blue) 
                    (make-block 8 10 'blue) 
                    (make-block 9 10 'blue)))

;;; ---- Tetra Examples

(define O (make-tetra (make-posn 5 19)
                      (list (make-block 4 19 'green)
                            (make-block 5 19 'green)
                            (make-block 4 18 'green)
                            (make-block 5 18 'green))))

(define I (make-tetra (make-posn 6 20)
                      (list (make-block 4 19 'blue)
                            (make-block 5 19 'blue)
                            (make-block 6 19 'blue)
                            (make-block 7 19 'blue))))

(define L (make-tetra (make-posn 6 18)
                      (list (make-block 6 19 'purple)
                            (make-block 6 18 'purple)
                            (make-block 5 18 'purple)
                            (make-block 4 18 'purple))))

(define J (make-tetra (make-posn 6 18)
                      (list (make-block 4 19 'turquoise)
                            (make-block 4 18 'turquoise)
                            (make-block 5 18 'turquoise)
                            (make-block 6 18 'turquoise))))

(define T (make-tetra (make-posn 5 18)
                      (list (make-block 5 19 'orange)
                            (make-block 5 18 'orange)
                            (make-block 4 18 'orange)
                            (make-block 6 18 'orange))))

(define Z (make-tetra (make-posn 6 19)
                      (list (make-block 4 19 'pink)
                            (make-block 5 19 'pink)
                            (make-block 5 18 'pink)
                            (make-block 6 18 'pink))))

(define S (make-tetra (make-posn 5 19)
                      (list (make-block 6 19 'red)
                            (make-block 5 19 'red)
                            (make-block 5 18 'red)
                            (make-block 4 18 'red))))


;;; --------- FUNCTIONS ---------

;Function that takes a function:
;Symbol [Num->Num] Posn -> Posn
;creates a posn with a function applied to a coordinate of the posn.
;used to clean code later!
(define (make-coord-posn sym f p)
    (if (symbol=? 'x sym) (make-posn  (f (posn-x p)) 
                                          (posn-y p))
         (make-posn   (posn-x p) 
                      (f (posn-y p)))))

(check-expect (make-coord-posn 'x add1 (make-posn 5 5))
              (make-posn 6 5))

(check-expect (make-coord-posn 'y sub1 (make-posn 8 10))
              (make-posn 8 9))



;Function that takes a function:
;Symbol [Num->Num] Block-> Block
;creates a block with a function applied to the coordinate of the block
;used to clean code later!
(define (make-coord-block sym f b)
  (if (symbol=? 'x sym)  (make-block (f (block-x b))
                                     (block-y b)
                                     (block-color b))
                           (make-block  (block-x b)
                                        (f (block-y b))
                                        (block-color b))))

(check-expect (make-coord-block 'x add1 (make-block 5 5 'red))
              (make-block 6 5 'red))

(check-expect (make-coord-block 'y sub1 (make-block 8 10 'red))
              (make-block 8 9 'red))

;Although make-coord-block and make-coord-posn are very similar, it is messy to abstract them because make-block
;takes in 3 arguments and make-posn only takes in two. 

;;; ---- Image-painting Functions

;;;Image Number Number Scene --> Scene
;;; Just like PLACE-IMAGE, but use grid coordinates.
(define (place-image/grid img x y scn)
  (place-image img
               (round (* GRID-SIZE (+ 1/2 x)))
               (round (- BOARD-HEIGHT-PIXELS 
			 (* GRID-SIZE (+ 1/2 y))))
               scn))

;;;Block -> Image
;;;Creates an image of block 
 (define (draw-block b )
    (square GRID-SIZE 'solid (block-color b)))

(check-expect (draw-block (make-block 5 10 'blue)) (square 20 'solid 'blue))                   


;;; BSet scene -> image
;;; Adds aBSet to a scene
(define (bset+scene b scene)
(cond [(empty? b) scene]
        [else (place-image/grid 
               (draw-block (first b))
               (block-x (first b))
               (block-y (first b))
               (bset+scene (rest b) scene))]))

(check-expect (bset+scene (list (make-block 6 6 'blue)) BACKGROUND)
              (place-image/grid (draw-block (make-block 6 6 'blue))
                                   (block-x (make-block 6 6 'blue))
                                   (block-y (make-block 6 6 'blue))
                                   BACKGROUND))

;; tetra scene -> image
;; Adds a tetra to a scene
(define (tetra+scene t scene)
  (bset+scene (tetra-blocks t) scene))

(check-expect (tetra+scene J BACKGROUND)
               (bset+scene (tetra-blocks J) BACKGROUND))

;;; World --> Image
;;; Create an image of the world
(define (world->scene w)
  (tetra+scene (world-tetra w) 
                (bset+scene (world-pile w) BACKGROUND))) 


(check-expect (world->scene (make-world I '()))
              (tetra+scene I (bset+scene '() BACKGROUND)))
               
;;; Number -> Tetra
;;; Creates a tetra based off of the number input (assigns a tetra to a number)
(define (assign-tetra n)
  (cond [(= n 0) O]
        [(= n 1) I]
        [(= n 2) L]
        [(= n 3) J]
        [(= n 4) T]
        [(= n 5) Z]
        [(= n 6) S]))

(check-expect (assign-tetra 0) O)
(check-expect (assign-tetra 1) I)
(check-expect (assign-tetra 2) L)
(check-expect (assign-tetra 3) J)
(check-expect (assign-tetra 4) T)
(check-expect (assign-tetra 5) Z)
(check-expect (assign-tetra 6) S)


;;; Initial State of World
(define INITIAL-WORLD
  (make-world (assign-tetra (random 7)) empty))

;;; ---- MOVING

;;;MOVEMENT DEFINITIONS
;;; --- Input can only move right or left

;;;move-block:
;;; Symbol Block -> Block
;;; Moves the block ONE grid unit in the specified direction
(define (move-block s b)
  (local [(define (block-direction dir b)
          (cond
            [(symbol=? 'right dir) (make-coord-block 'x add1 b)]
            [(symbol=? 'down  dir) (make-coord-block 'y sub1 b)]
            [(symbol=? 'left  dir) (make-coord-block 'x sub1 b)]
            ))]
    (block-direction s b)))


(check-expect (move-BSet 'right bset0) bset1)              
(check-expect (move-BSet 'left  bset1) bset0)        
(check-expect (move-BSet 'down  bset1)
              (list (make-block 6 9 'blue) 
                    (make-block 7 9 'blue) 
                    (make-block 8 9 'blue) 
                    (make-block 9 9 'blue)))


;;; Symbol BSet -> BSet
;;; Moves set of blocks 1 grid unit in specified direction
(define (move-BSet s bs)
  (local [(define (new-func x) (move-block s x))]
  (map new-func bs)))
  
;;;Check:
(check-expect (move-BSet 'right (list (make-block 5 10 'blue)
                                      (make-block 6 10 'blue)
                                      (make-block 7 10 'blue)
                                      (make-block 8 10 'blue)))
              (list (make-block 6 10 'blue) 
                    (make-block 7 10 'blue) 
                    (make-block 8 10 'blue) 
                    (make-block 9 10 'blue)))

(check-expect (move-BSet 'left  (list (make-block 6 10 'blue)
                                      (make-block 7 10 'blue)
                                      (make-block 8 10 'blue)
                                      (make-block 9 10 'blue)))
              (list (make-block 5 10 'blue) 
                    (make-block 6 10 'blue) 
                    (make-block 7 10 'blue) 
                    (make-block 8 10 'blue)))

(check-expect (move-BSet 'down  (list (make-block 6 10 'blue)
                                      (make-block 7 10 'blue)
                                      (make-block 8 10 'blue)
                                      (make-block 9 10 'blue)))
              (list (make-block 6 9 'blue) 
                    (make-block 7 9 'blue) 
                    (make-block 8 9 'blue) 
                    (make-block 9 9 'blue)))

;;; Symbol Posn -> Posn
;;; Moves the posn one grid unit in specified direction
(define (move-posn s p)
  (local [ (define (posn-right p)
            (make-posn (+ 1 (posn-x p)) (posn-y p)))
          (define (posn-left p)
            (make-posn (- (posn-x p) 1) (posn-y p)))
          (define (posn-down p)
            (make-posn (posn-x p) (- (posn-y p) 1)))]
          
  (cond [(symbol=? 'right s) (posn-right p)]
        [(symbol=? 'left  s) (posn-left  p)]
        [(symbol=? 'down  s) (posn-down  p)])))

;;;Checks:
(check-expect (move-posn 'right (make-posn 12 4)) (make-posn 13 4))
(check-expect (move-posn 'left (make-posn 21 4)) (make-posn 20 4))
(check-expect (move-posn 'down (make-posn 21 4)) (make-posn 21 3))

;;; Symbol Tetra -> Tetra
;;; Moves Tetra one grid unit in specified direction

(define (move-tetra s t)
  (make-tetra (move-posn s (tetra-center t)) (move-BSet s (tetra-blocks t))))

;;;Checks:
(check-expect (move-tetra 'right (make-tetra (make-posn 5 5)
                                            (list (make-block 4 2 'blue)
                                                  (make-block 7 6 'blue))))
              (make-tetra (make-posn 6 5) (list (make-block 5 2 'blue)
                                                (make-block 8 6 'blue))))

(check-expect (move-tetra 'left (make-tetra 
                                (make-posn 5 5)
                                (list (make-block 4 2 'blue)
                                      (make-block 7 6 'blue))))
              (make-tetra (make-posn 4 5) 
                          (list (make-block 3 2 'blue)
                                (make-block 6 6 'blue))))

(check-expect (move-tetra 'down (make-tetra (make-posn 5 5)
                                            (list (make-block 4 2 'blue)
                                                  (make-block 7 6 'blue))))
              (make-tetra (make-posn 5 4) (list (make-block 4 1 'blue)
                                                (make-block 7 5 'blue))))

;;; Posn Block -> Block
;;; Rotates the block 90 degrees counterclockwise around the posn.
(define (block-rotate-ccw c b)
  (make-block (+ (posn-x c)
                 (- (posn-y c)
                    (block-y b)))
              (+ (posn-y c)
                 (- (block-x b)
                    (posn-x c)))
              (block-color b)))

(check-expect (block-rotate-ccw (make-posn 4 5) 
                                (make-block 5 10 'green))
              (make-block -1 6 'green))

;;; Posn Block -> Block
;;; Rotates a block 90 degrees clockwise around the posn
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

(check-expect (block-rotate-cw (make-posn 4 5) 
                               (make-block 5 10 'green))
              (make-block 9 4 'green))

;;; Posn BSet -> BSet
;;; Rotates a BSet counter clockwise around the posn
;;(define (BSet-rotate-ccw p bs)
 (define (bset-rotate-ccw c bs)
   (local [(define (rotate x) (block-rotate-ccw c x))]
    (if (reach-side-bset? bs) bs
        (map rotate bs))))
        
;;;Check:
(check-expect (bset-rotate-ccw (make-posn 5 10) 
                               (list (make-block 5 10 'green)
                                     (make-block 6 10 'green)
                                     (make-block 7 10 'green)
                                     (make-block 8 10 'green)))
              (list (make-block 5 10 'green) 
                    (make-block 5 11 'green) 
                    (make-block 5 12 'green) 
                    (make-block 5 13 'green)))

(check-expect (bset-rotate-ccw (make-posn 5 5) empty)
              empty)

;;; bset-rotate-cw
(define (bset-rotate-cw c bs)
  (local [(define (rotate x) (block-rotate-cw c x))]
    (if (reach-side-bset? bs) bs
        (map rotate bs))))
        
(check-expect (bset-rotate-cw (make-posn 5 10) 
                              (list (make-block 5 10 'blue)
                                    (make-block 6 10 'blue)
                                    (make-block 7 10 'blue)
                                    (make-block 8 10 'blue)))
              (list (make-block 5 10 'blue)
                    (make-block 5 9 'blue)
                    (make-block 5 8 'blue) 
                    (make-block 5 7 'blue)))
              
;;; ---- Collision Detection ----
;;; -- Bottom Collision Detection: Keeps within bounds

;;; Bset -> boolean
;;; Has the BSet reached the bottom of the screen?
(define (reach-bottom-tetra? t)
  (ormap (lambda (b) (<= (block-y b) 0)) (tetra-blocks t)))

;;; Check:
(check-expect (reach-bottom-tetra? (make-tetra (make-posn 5 1)
                                               (list (make-block 5 1 'blue)
                                                    (make-block  5 0 'turquoise))))
              true)
(check-expect (reach-bottom-tetra? (make-tetra (make-posn 5 10)
                                              (list (make-block 5 10 'pink)
                                                    (make-block 6 10 'pink))))
              false)

;;;; ---- Side Collision Detection: Keeps within bounds

;;; BSet -> Boolean
;;; Has the BSet reached the right edge of the screen?
(define (reach-right-bset? bs)
  (ormap (lambda (b) (>= (block-x b) (- BOARD-WIDTH 1))) bs))

;;;Checks:
(check-expect (reach-right-bset? (list (make-block 9 3 'blue)
                                       (make-block 3 4 'blue))) true)
(check-expect (reach-right-bset? (list (make-block 5 0 'blue))) false)

;;; BSet -> Boolean
;;; Has the BSet reached the right edge of the screen?
(define (reach-left-bset? bs)
  (ormap (lambda (b) (<= (block-x b) 0)) bs))
 
;;;Checks:
(check-expect (reach-left-bset? (list (make-block 0 1  'blue)
                                (make-block 9 4 'blue))) true)
(check-expect (reach-left-bset? (list (make-block 5 10 'blue))) false)

;;; BSet -> Boolean
;;; Has the bset reached either side of the board?
(define (reach-side-bset? bs)
 (or (reach-left-bset? bs)
     (reach-right-bset? bs)))

(check-expect (reach-side-bset? (list (make-block 0 5 'blue))) true)
(check-expect (reach-side-bset? (list (make-block 9 4 'blue)
                                      (make-block 8 4 'blue)))
              true)
(check-expect (reach-side-bset? (list (make-block 5 10 'blue)
                                      (make-block 5 10 'blue)))
              false)

;;;; ---- Top Collision Detection: Alert for End of Game:

;;; BSet -> Boolean
;;; Has the BSet reached the top of the screen?
(define (at-top-bset? bs)
   (ormap (lambda (b) (>= (block-y b) (- BOARD-HEIGHT 1))) bs))

;;; Checks:

(check-expect (at-top-bset? (list (make-block 5 20 'purple)
                                  (make-block 6 21 'purple))) true)
(check-expect (at-top-bset? (list (make-block 10 3 'orange))) false)

;;; World -> Boolean
;;; Has the pile reached the top of the screen?
(define (at-top-pile? w)
  (at-top-bset? (world-pile w)))

;;; Checks:
(check-expect (at-top-pile? (make-world L (list (make-block 5 5 'green)
                                                (make-block 4 5 'green))))
              false)
(check-expect (at-top-pile? (make-world L (list (make-block 5 19 'green)
                                                (make-block 4 19 'green))))
              true)

;;; ---- Tetris Collison: Are Tetris peices on top of each other?

;;; block BSet -> Boolean
;;; Is the block on top of the BSet?
(define (block-on-bset? b bs)
  (local [(define (b-on-b x) (and (= (block-x b) (block-x x))
                                  (= (block-y b) (+ 1 (block-y x)))))]
    (ormap b-on-b bs)))
          
(check-expect (block-on-bset? (make-block 6 20 'blue) 
                                  (list (make-block 6 19 'blue)
                                        (make-block 6 18 'blue)))
              true)
(check-expect (block-on-bset? (make-block 5 5 'blue) empty)
              false)

;;; BSet Bset -> Boolean
;;; Is the BSet on top of the second BSet?
(define (bset-on-bset? bs1 bs2)
  (local [(define (helper x) (block-on-bset? x bs2))]
  (ormap helper bs1)))
 
 
(check-expect (bset-on-bset? 
               (list (make-block 4 1 'green) (make-block 5 1 'green))
               (list (make-block 2 0 'green) (make-block 3 0 'green))) false )
(check-expect (bset-on-bset? 
               (list (make-block 2 1 'green) (make-block 2 2 'green))
               (list (make-block 2 0 'green) (make-block 3 0 'green))) true)

;;; World -> Boolean
;;; Is the world-tetra on any of the blocks in the pile?
(define (tetra-on-pile? w)
  (bset-on-bset? (tetra-blocks (world-tetra w)) (world-pile w)))

;;; Check
(check-expect (tetra-on-pile?
               (make-world (make-tetra (make-posn 5 5)
                                       (list (make-block 4 1 'blue)
                                             (make-block 5 1 'blue)))
                           (list (make-block 2 0 'blue)
                                 (make-block 3 0 'blue)))) false)
(check-expect (tetra-on-pile?
               (make-world (make-tetra (make-posn 5 5)
                                       (list (make-block 2 1 'pink)
                                             (make-block 2 1 'pink)))
                           (list (make-block 2 0 'pink)
                                 (make-block 3 0 'pink)))) true)

;;; Block BSet -> Boolean
;;; Is the block next to the BSet?
(define (block-next-to-bset? b bs)
  (local [(define (b-n-b? x) (or (and (= (block-x b) (+ 1 (block-x x))) 
                                       (= (block-y b)      (block-y x)))
                                  (and (= (block-x b) (-   (block-x x) 1))
                                       (= (block-y b)      (block-y x)))))]
    (ormap b-n-b? bs)))
          

(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 5 1 'blue) empty))
              true)
(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 3 1 'blue) empty))
              true)
(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 3 2 'blue) empty))
              false)

;;; BSet BSet -> Boolean
;;; Is the BSet next to the BSet?
(define (bset-next-to-bset? bs1 bs2)
  (local [(define (new-bool x) (block-next-to-bset? x bs2))]
  (ormap new-bool bs1)))

(check-expect (bset-next-to-bset? (list (make-block 0 0 'pink)
                                        (make-block 0 1 'pink))
                                  (list (make-block 1 0 'green)
                                        (make-block 2 0 'green)))
              true)

(check-expect (bset-next-to-bset? (list (make-block 1 0 'green)
                                        (make-block 2 0 'green))
                                  (list (make-block 0 0 'pink)
                                        (make-block 0 1 'pink)))
              true)

(check-expect (bset-next-to-bset? (list (make-block 1 5 'green)
                                        (make-block 2 5 'green))
                                  (list (make-block 0 0 'pink)
                                        (make-block 0 1 'pink)))
              false)
;;;---------------------------------------------
(define ROW-LIST (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
(define NO-FULL-ROWS (list (make-block 0 0 'pink)
                           (make-block 1 0 'pink)
                           (make-block 0 1 'pink)))

(define ONE-FULL-ROW (list (make-block 5 1 'red)
                           (make-block 10 0 'red)
                           (make-block 9 0 'red)
                           (make-block 8 0 'red)
                           (make-block 7 0 'red)
                           (make-block 6 0 'red)
                           (make-block 5 0 'red)
                           (make-block 4 0 'red)
                           (make-block 3 0 'red)
                           (make-block 2 0 'red)
                           (make-block 1 0 'red)
                           (make-block 2 1 'red)
                           (make-block 1 1 'red)
                           (make-block 0 1 'red)))
                                                           

;;;Bset Ys-> List of Numbers (ys)
;;;Creates a list of the y coords that have full rows.
(define (blocks-in-rows bs)
  (local [(define (count-ys y)
            (if ( = (length (filter (lambda (b) (= (block-y b) y)) bs)) 10)
                                    y
                                   -1))
          (define (full? x) (not (= x -1)))]
    (filter full? (map count-ys ROW-LIST))))


(check-expect (blocks-in-rows NO-FULL-ROWS) '())
                              
              
(check-expect (blocks-in-rows ONE-FULL-ROW)
              (list 0))

;;; BSet --> BSet
;;; Returns Bset without full rows & moves remaining blocks
(define (delete-row bs)
  (local [(define (full-ys bs) (blocks-in-rows bs))
          ;; does the y coord of the block exist in full-ys
          (define (not-full? block) (not (ormap (lambda (fulls) (= (block-y block) fulls)) (full-ys bs))))
          ;;; Is the block below ALL full rows?
          (define (below-ys? block) (andmap (lambda (fulls) (< (block-y block) fulls)) (full-ys bs)))
          (define (move-down block) (if (not (below-ys? block)) (make-block (block-x block)
                                                                            (- (block-y block) 1)
                                                                            (block-color block))
                                        block))] 
    (map move-down (filter not-full? bs))))

;;;Checks
(check-expect (delete-row ONE-FULL-ROW)
                          
              (list (make-block 5 0 'red)
                    (make-block 2 0 'red)
                    (make-block 1 0 'red)
                    (make-block 0 0 'red)))

(check-expect (delete-row (list (make-block 5 1 'red)
                                (make-block 2 1 'red)
                                (make-block 1 1 'red)
                                (make-block 0 1 'red)))
              (list (make-block 5 1 'red)
                    (make-block 2 1 'red)
                    (make-block 1 1 'red)
                    (make-block 0 1 'red))) 
 
;;; ---- Post-Collision Detection: What do when the block hits the pile

;;; add-to-pile:
;;; Tetra BSet -> Bset
;;; Add the Tetra to the pile
(define (add-to-pile t bs)
  (append (tetra-blocks t) bs))


;;World -> World
(define (post-collision-world w)
  (make-world (assign-tetra (random 7))
              (delete-row (add-to-pile (world-tetra w) (world-pile w)))))
      


;;; ---- Big Bang Functions

;;; World -> Boolean
;;; Has collision occured?
(define (need-post-world? w)
  (or (reach-bottom-tetra? (world-tetra w)) (tetra-on-pile? w)))

;;; next-world
;;; World->World
;;; Creates a world one tick later (tetra has moved 1 grid unit down
(define (next-world w)
  (if (need-post-world? w) (post-collision-world w)
                           (make-world (move-tetra 'down (world-tetra w)) (world-pile w))))

(check-expect (next-world (make-world I (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))))
              (make-world (make-tetra (make-posn 6 19)
                      (list (make-block 4 18 'blue)
                            (make-block 5 18 'blue)
                            (make-block 6 18 'blue)
                            (make-block 7 18 'blue)))
                          (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))))

;;;World -> Boolean
(define (can-move-right? w)
  (and (not (reach-right-bset?  (tetra-blocks (world-tetra w))))
       (not (bset-next-to-bset? (tetra-blocks (world-tetra w)) (world-pile w)))))

;;;Can-move-left?
(define (can-move-left? w)
  (and (not (reach-left-bset?  (tetra-blocks (world-tetra w))))
       (not (bset-next-to-bset? (tetra-blocks (world-tetra w)) (world-pile w)))))

;;; World KE -> WORLD
(define (key-handler w ke)
  (local [(define (correct-symbol? dir) (symbol=? (string->symbol ke) dir))]
    (cond [(and (correct-symbol? 'right) (can-move-right? w))
           (make-world (move-tetra 'right (world-tetra w)) (world-pile w))]
          [(and (correct-symbol? 'left)(can-move-left? w))
           (make-world (move-tetra 'left  (world-tetra w)) (world-pile w))]
          [(correct-symbol? 'a) (make-world 
                                 (make-tetra 
                                  (tetra-center (world-tetra w))
                                  (bset-rotate-cw (tetra-center (world-tetra w))
                                                  (tetra-blocks (world-tetra w))))
                                 (world-pile w))]
          [(correct-symbol? 's) (make-world 
                                 (make-tetra 
                                  (tetra-center (world-tetra w))
                                  (bset-rotate-ccw (tetra-center (world-tetra w))
                                                   (tetra-blocks (world-tetra w))))
                                 (world-pile w))]
          [else w])))

(check-expect (key-handler (make-world (make-tetra (make-posn 5 10) (list (make-block 5 10 'red) (make-block 6 10 'red)))
                                       (list (make-block 5 0 'red) (make-block 10 0 'red))) "right") 
              (make-world (make-tetra  (make-posn 6 10) (list (make-block 6 10 'red) (make-block 7 10 'red)))
                                       (list (make-block 5 0 'red)  (make-block 10 0 'red))))
;;; World -> Number
;;; counts number of tetras in pile
(define (score w)
  (/ (count-pile (world-pile w)) 4))

(check-expect (score (make-world (make-tetra (make-posn 5 10) (list (make-block 5 10 'red) (make-block 6 10 'red)))
              (list (make-block 5 0 'red) (make-block 10 0 'red) ))) .5)

;;; Bset -> Number
;;; counts number of blocks in pile

(define (count-pile bs)
  (length bs)) 

(check-expect (count-pile (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))) 2)

;;; World -> Image
;;; displays final score
(define (display-score w)
  (place-image (text "Game Over" 12 "red") 50 15
               (place-image (text (number->string (score w)) 12 "red") 50 40 BACKGROUND)))

(check-expect (display-score (make-world (make-tetra (make-posn 6 10) (list  (make-block 6 10 'red)
                                                                             (make-block 5 0 'red) ))
                                         (list (make-block 10 0 'red)
                                               (make-block 7 10 'red)
                                               (make-block 8 0 'red)
                                               (make-block 6 10 'red))))
              (place-image (text "Game Over" 12 "red") 50 15 (place-image (text "1" 12 "red") 50 40 BACKGROUND)))


(big-bang  INITIAL-WORLD
          (on-tick next-world .3)
          (to-draw world->scene)
          (on-key key-handler)
          (stop-when at-top-pile? display-score))



              
              
              