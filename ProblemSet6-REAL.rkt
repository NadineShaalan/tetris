;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ProblemSet6-REAL) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| Problem Set 6
   Authors: Nadine Shaalan and Noelle Wong
   Emails:  Shaalan.n@husky.neu.edu and Wong.No@husky.neu.edu
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
;; Order does not matter.  Repetitions are NOT allowed.

;; A World is a (make-world Tetra BSet)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;A grid is in grid/cell units
;; 0,0 is on the top left

;;; ---- CONSTANTS: Properties that will remain the same
 
(define GRID-SIZE 10)     ; width of a game board sqaure in pixels
(define BOARD-HEIGHT 20)  ; height in grid squares
(define BOARD-WIDTH 10)   ; width in grid squares
(define BOARD-HEIGHT-PIXELS (* GRID-SIZE BOARD-HEIGHT)) ; Board height in pixels
(define BOARD-WIDTH-PIXELS  (* GRID-SIZE BOARD-WIDTH))  ; Board width in pixels

(define BACKGROUND (empty-scene BOARD-WIDTH-PIXELS BOARD-HEIGHT-PIXELS))

;;; ---- Tetra Examples

(define O (make-tetra (make-posn 5 18)
                      (list (make-block 4 19 'green)
                            (make-block 5 19 'green)
                            (make-block 4 18 'green)
                            (make-block 5 18 'green))))

(define I (make-tetra (make-posn 5 19)
                      (list (make-block 4 19 'blue)
                            (make-block 5 19 'blue)
                            (make-block 6 19 'blue)
                            (make-block 7 19 'blue))))

(define L (make-tetra (make-posn 4 18)
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

(define Z (make-tetra (make-posn 6 18)
                      (list (make-block 4 19 'pink)
                            (make-block 5 19 'pink)
                            (make-block 5 18 'pink)
                            (make-block 6 18 'pink))))

(define S (make-tetra (make-posn 6 19)
                      (list (make-block 6 19 'red)
                            (make-block 5 19 'red)
                            (make-block 5 18 'red)
                            (make-block 4 18 'red))))


;;; --------- FUNCTIONS ---------

;;; ---- Image-painting Functions

;;; place-image/grid
;;;Image Number Number Scene --> Scene
;;; Just like PLACE-IMAGE, but use grid coordinates.
(define (place-image/grid img x y scn)
  (place-image img
               (round (* GRID-SIZE (+ 1/2 x)))
               (round (- BOARD-HEIGHT-PIXELS 
			 (* GRID-SIZE (+ 1/2 y))))
               scn))

;;;draw-block
;;;Block -> Image
;;;Creates an image of block 
 (define (draw-block b )
    (square GRID-SIZE 'solid (block-color b)))

 (check-expect (draw-block (make-block 5 10 'blue)) (square 10 'solid 'blue))                   


;;; BSet scene -> image
;;; Adds aBSet to a scene
(define (BSet->scene b scene)
  (cond [(empty? b) scene]
        [else (place-image/grid 
               (draw-block (first b))
               (block-x (first b))
               (block-y (first b))
               (BSet->scene (rest b) scene))]))

(check-expect (BSet->scene (list (make-block 6 6 'blue)) BACKGROUND)
              (place-image/grid (draw-block (make-block 6 6 'blue))
                                   (block-x (make-block 6 6 'blue))
                                   (block-y (make-block 6 6 'blue))
                                   BACKGROUND))

;; tetra->scene : tetra scene -> image
;; Adds a tetra to a scene
(define (tetra->scene t scene)
  (BSet->scene (tetra-blocks t) scene))

(check-expect (tetra->scene J BACKGROUND)
               (BSet->scene (tetra-blocks J) BACKGROUND))

;;; world->scene:
;;; World --> Image
;;; Create an image of the world
(define (world->scene w)
  (tetra->scene (world-tetra w) 
                (BSet->scene (world-pile w) BACKGROUND))) 
                              

;;; assign-tetra:
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
;;; Will have a random element
(define INITIAL-WORLD
  ;; DON'T PUT BACKGROUND because BACKGROUND =/= bset
  (make-world (assign-tetra (random 7)) empty))

;;; ---- MOVING

;;;MOVEMENT DEFINITIONS
;;; --- Input can only move right or left
;;; Should we still create something to move it down at a constant rate?

;;; If the block moves right the x-coord increments by 1
;;; y coord and block color stays the same
(define (block-right b)
  (make-block (+ (block-x b) 1)  (block-y b)
                               (block-color b)))

(check-expect (block-right (make-block 5 0 'blue)) (make-block 6 0 'blue))

;;; If the block moves left the x coord decrements by 1
;;; y coord and block color stays same
(define (block-left b)
  (make-block (- (block-x b) 1)  (block-y b)
                               (block-color b)))

(check-expect (block-left (make-block 5 0 'blue)) (make-block 4 0 'blue))

;;; If the block moves down (state of the game) the y coord decrements by 1
;;; x coord and block color stays the same
(define (block-down b)
  (make-block (block-x b) (- (block-y b) 1)
                               (block-color b)))
;;;Checks:
(check-expect (block-right (make-block 5 10 'blue)) (make-block 6 10 'blue))
(check-expect (block-left (make-block 5 10 'blue)) (make-block 4 10 'blue))
(check-expect (block-down (make-block 5 10 'blue)) (make-block 5 9 'blue))

;;;move-block:
;;; Symbol Block -> Block
;;; Moves the block ONE grid unit in the specified direction
(define (move-block s b)
  (cond [(symbol=? 'right s) (block-right b)]
        [(symbol=? 'left s)  (block-left b) ]
        [(symbol=? 'down s)  (block-down b)]))

;;;CHECK:
(check-expect (move-block 'right (make-block 5 10 'blue)) (make-block 6 10 'blue))
(check-expect (move-block 'left (make-block 5 10 'blue)) (make-block 4 10 'blue))
(check-expect (move-block 'down (make-block 5 10 'blue)) (make-block 5 9 'blue))

;;; move-BSet
;;; Symbol BSet -> BSet
;;; Moves set of blocks 1 grid unit in specified direction
(define (move-BSet s bs)
  (cond [(empty? bs) empty ]
        [(cons? bs) (cons (move-block s (first bs)) (move-BSet s (rest bs)))]))

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

;;; If the posn moves right the x coord increments by 1
;;; y coord stays same
;;; Similar to block-right
(define (posn-right p)
  (make-posn (+ 1 (posn-x p)) (posn-y p)))

(check-expect (posn-right (make-posn 5 4)) (make-posn 6 4))

;;; If the block moves left the x coord decrements by 1
;;; y coord stays same
;;; similar to block-left
(define (posn-left p)
  (make-posn (- (posn-x p) 1) (posn-y p)))

(check-expect (posn-left (make-posn 5 4)) (make-posn 4 4))

;;; If the block moves down the y coord decrements by 1
;;; x coord stays the same
;;; similar to block-down
(define (posn-down p)
  (make-posn (posn-x p) (- (posn-y p) 1)))

;;;Checks:
(check-expect (posn-right (make-posn 2 3)) (make-posn 3 3))
(check-expect (posn-left  (make-posn 2 3)) (make-posn 1 3))
(check-expect (posn-down  (make-posn 2 3)) (make-posn 2 2))

;;; move-posn:
;;; Symbol Posn -> Posn
;;; Moves the posn one grid unit in specified direction
(define (move-posn s p)
  (cond [(symbol=? 'right s) (posn-right p)]
        [(symbol=? 'left  s) (posn-left  p)]
        [(symbol=? 'down  s) (posn-down  p)]))

;;;Checks:
(check-expect (move-posn 'right (make-posn 12 4)) (make-posn 13 4))
(check-expect (move-posn 'left (make-posn 21 4)) (make-posn 20 4))
(check-expect (move-posn 'down (make-posn 21 4)) (make-posn 21 3))

;;;Tetra -> BSet
(define (block-helper t)
  (tetra-blocks t))

;;; move-tetra
;;; Symbol Tetra -> Tetra
;;; Moves Tetra one grid unit in specified direction

(define (move-tetra s t)
  (make-tetra (move-posn s (tetra-center t)) (move-BSet s (block-helper t))))

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

;;; block-rotate-ccw : Posn Block -> Block
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

;;; block-rotate-cw
;;; Posn Block -> Block
;;; Rotates a block 90 degrees clockwise around the posn
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))

(check-expect (block-rotate-cw (make-posn 4 5) 
                               (make-block 5 10 'green))
              (make-block 9 4 'green))

;;; bset-rotate-ccw
;;; Posn BSet -> BSet
;;; Rotates a BSet counter clockwise around the posn
;;(define (BSet-rotate-ccw p bs)
 (define (bset-rotate-ccw c bs)
    (if (reach-side-bset? bs) bs  
         (cond [(empty? bs) empty]
                     [(cons? bs) (cons (block-rotate-ccw c (first bs))
                                       (bset-rotate-ccw c (rest bs)))])))
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
    (if (reach-side-bset? bs) bs  
         (cond [(empty? bs) empty]
                     [(cons? bs) (cons (block-rotate-cw c (first bs))
                                       (bset-rotate-cw c (rest bs)))])))


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

;;; reach-bottom-block?:
;;; Block -> Boolean
;;; Is the block on the bottom of the grid?
(define (reach-bottom-block? b)
  ;; <= is better than =
  (<= (block-y b) 0))

;;;Checks:
(check-expect (reach-bottom-block? (make-block 5 2 'blue)) false)
(check-expect (reach-bottom-block? (make-block 5 0 'blue)) true)

;;; reach-bottom-bset:
;;; Bset -> boolean
;;; Has the BSet reached the bottom of the screen?
(define (reach-bottom-bset? bs)
  (cond [(empty? bs) false]
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (reach-bottom-block? (first bs))) true]
        [ else (reach-bottom-bset? (rest bs))]))

;;;Checks:
(check-expect (reach-bottom-bset? (list (make-block 5 1 'turquoise)
                                        (make-block 5 4 'turquoise)))
              false)

(check-expect (reach-bottom-bset? (list (make-block 5 1 'blue)
                                        (make-block 5 0 'turquoise)))
              true)

;;; reach-bottom-tetra?:
;;; Tetra -> Boolean
;;; Has the tetra reached the bottom of the screen?
(define (reach-bottom-tetra? t)
  (reach-bottom-bset? (tetra-blocks t)))

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
;;; reach-right-block?:
;;; Block -> Boolean
;;; Has the block reached the right edge of the screen?
(define (reach-right-block? b)
  (>= (block-x b) (- BOARD-WIDTH 1)))

;;; Checks:
(check-expect (reach-right-block? (make-block 19 5 'pink)) true)
(check-expect (reach-right-block? (make-block 5 10 'red)) false)

;;; reach-left-block?:
;;; Block -> Boolean
;;; Has the block reached the left edge of the screen?
(define (reach-left-block? b)
  (<= (block-x b) 0))

;;Checks:
(check-expect (reach-left-block? (make-block 0 5 'pink)) true)
(check-expect (reach-left-block? (make-block 5 10 'red)) false)

;;; reach-right-bset?
;;; BSet -> Boolean
;;; Has the BSet reached the right edge of the screen?
(define (reach-right-bset? bs)
  (cond [(empty? bs) false] 
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (reach-right-block? (first bs))) true]
        [ else (reach-right-bset? (rest bs))]))

;;;Checks:
(check-expect (reach-right-bset? (list (make-block 9 3 'blue)
                                       (make-block 3 4 'blue))) true)
(check-expect (reach-right-bset? (list (make-block 5 0 'blue))) false)


;;; reach-left-bset?
;;; BSet -> Boolean
;;; Has the BSet reached the right edge of the screen?
(define (reach-left-bset? bs)
   (cond [(empty? bs) false] 
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (reach-left-block? (first bs))) true]
        [ else (reach-left-bset? (rest bs))]))

;;;Checks:
(check-expect (reach-left-bset? (list (make-block 0 1  'blue)
                                (make-block 9 4 'blue))) true)
(check-expect (reach-left-bset? (list (make-block 5 10 'blue))) false)

;;; reach-side-bset?
;;; BSet -> Boolean
;;; Has the bset reached either side of the board?
(define (reach-side-bset? bs)
  (or (reach-right-bset? bs)
      (reach-left-bset? bs)))

(check-expect (reach-side-bset? (list (make-block 0 5 'blue))) true)
(check-expect (reach-side-bset? (list (make-block 9 4 'blue)
                                      (make-block 8 4 'blue)))
              true)
(check-expect (reach-side-bset? (list (make-block 5 10 'blue)
                                      (make-block 5 10 'blue)))
              false)

;;;; ---- Top Collision Detection: Alert for End of Game:

;;; at-top-block?:
;;; Block -> Boolean
;;; Has the block reached the top of the screen?
(define (at-top-block? b)
  (>= (block-y b) (- BOARD-HEIGHT 1)))

;;; Checks:
(check-expect (at-top-block? (make-block 5 19 'green)) true)
(check-expect (at-top-block? (make-block 5 10 'green)) false)


;;; at-top-bset?:
;;; BSet -> Boolean
;;; Has the BSet reached the top of the screen?
(define (at-top-bset? bs)
   (cond [(empty? bs) false] 
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (at-top-block? (first bs))) true]
        [ else (at-top-bset? (rest bs))]))

;;; Checks:

(check-expect (at-top-bset? (list (make-block 5 20 'purple)
                                  (make-block 6 21 'purple))) true)
(check-expect (at-top-bset? (list (make-block 10 3 'orange))) false)

;;; at-top-pile?:
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

;;; block-on-block?:
;;; Block Block -> Boolean
;;; Is the block on top of the second block?
;;; ENGLISH: X-coord is the same and y coord is 1 unit above 
(define (block-on-block? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (block-y b1) (+ 1 (block-y b2)))))

;;;Checks:
(check-expect (block-on-block? (make-block 4 1 'red) (make-block 4 0 'red)) true)
(check-expect (block-on-block? (make-block 4 1 'red) (make-block 5 2 'red)) false)

;;; block-on-bset?
;;; block BSet -> Boolean
;;; Is the block on top of the BSet?
(define (block-on-bset? b bs)
  (cond [(empty? bs) false]
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (block-on-block? b (first bs) )) true]
        [ else (block-on-bset? b (rest bs))]))

(check-expect (block-on-bset? (make-block 6 20 'blue) 
                                  (list (make-block 6 19 'blue)
                                        (make-block 6 18 'blue)))
              true)
(check-expect (block-on-bset? (make-block 5 5 'blue) empty)
              false)


;;; bset-on-bset?
;;; need block-on-bset? first
;;; Step 1: Is first block in the list on top of the bset?
;;; BSet Bset -> Boolean
;;; Is the BSet on top of the second BSet?
(define (bset-on-bset? bs1 bs2)
  (cond [(empty? bs1) false]
        [(and (cons? bs1) ( block-on-bset? (first bs1) bs2)) true]
        [ else (bset-on-bset? (rest bs1) bs2)])) 
 
(check-expect (bset-on-bset? 
               (list (make-block 4 1 'green) (make-block 5 1 'green))
               (list (make-block 2 0 'green) (make-block 3 0 'green))) false )
(check-expect (bset-on-bset? 
               (list (make-block 2 1 'green) (make-block 2 2 'green))
               (list (make-block 2 0 'green) (make-block 3 0 'green))) true)

;;; tetra-on-pile?
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

;;; block-next-to-block
;;; Block Block -> Boolean
;;; Is the block next to the second block?
(define (block-next-to-block? b1 b2)
  (or (and (= (block-x b1) (+ 1 (block-x b2))) 
           (= (block-y b1)  (block-y b2)))
      (and (= (block-x b1) (- (block-x b2) 1))
           (= (block-y b1)  (block-y b2)))))

;;;Check
(check-expect (block-next-to-block? (make-block 4 1 'blue) (make-block 5 1 'blue)) true)
(check-expect (block-next-to-block? (make-block 4 1 'blue) (make-block 3 1 'blue)) true)
(check-expect (block-next-to-block? (make-block 2 3 'blue) (make-block 5 1 'blue)) false)

;;; block-next-to-bset
;;; Block BSet -> Boolean
;;; Is the block next to the BSet?
(define (block-next-to-bset? b bs)
  (cond [(empty? bs) false]
        ;; if bs is a list AND the first element is  true
        [(and (cons? bs) (block-next-to-block? b (first bs) )) true]
        [ else (block-on-bset? b (rest bs))]))
(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 5 1 'blue) empty))
              true)
(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 3 1 'blue) empty))
              true)
(check-expect (block-next-to-bset? (make-block 4 1 'blue) (cons (make-block 3 2 'blue) empty))
              false)
;;; bset-next-to-bset
;;; BSet BSet -> Boolean
;;; Is the BSet next to the BSet?
(define (bset-next-to-bset? bs1 bs2)
  (cond [(empty? bs1) false]
        [(and (cons? bs1) ( block-next-to-bset? (first bs1) bs2)) true]
        [ else (bset-next-to-bset? (rest bs1) bs2)]))

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
;;; ---- Post-Collision Detection: What do when the block hits the pile

;;; add-to-pile:
;;; Tetra BSet -> Bset
;;; Add the Tetra to the pile
(define (add-to-pile t bs)
  (append (tetra-blocks t) bs))

;;;CHECK

;;World -> World
(define (post-collision-world w)
  (make-world (assign-tetra (random 7))
              (add-to-pile (world-tetra w) (world-pile w))))
      


;;; ---- Big Bang Functions

;;;We already have a World Scene - > Image


;;;WORLD TEMPLATE
#; (define (world-template w)
     (...(world-tetra w)...
      ...(world-pile)..))

;;; next-world
;;; World->World
;;; Creates a world one tick later (tetra has moved 1 grid unit down
(define (next-world w)
  (cond [(or (reach-bottom-tetra? (world-tetra w)) (tetra-on-pile? w)) (post-collision-world w)]
        [ else (make-world (move-tetra 'down (world-tetra w)) (world-pile w))]))

(check-expect (next-world (make-world I (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))))
              (make-world (make-tetra (make-posn 5 18)
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
  (cond [(and (symbol=? (string->symbol ke) 'right)
              (can-move-right? w))
                   (make-world (move-tetra 'right (world-tetra w)) (world-pile w))]
        [(and (symbol=? (string->symbol ke) 'left )
             (can-move-left? w))
                   (make-world (move-tetra 'left  (world-tetra w)) (world-pile w))]
        [(symbol=? (string->symbol ke) 'a    ) (make-world 
                                                (make-tetra 
                                                 (tetra-center (world-tetra w))
                                                 (bset-rotate-cw (tetra-center (world-tetra w))
                                                                 (tetra-blocks (world-tetra w))))
                                                                 (world-pile w))]
        [(symbol=? (string->symbol ke) 's    ) (make-world 
                                                (make-tetra 
                                                 (tetra-center (world-tetra w))
                                                 (bset-rotate-ccw (tetra-center (world-tetra w))
                                                                  (tetra-blocks (world-tetra w))))
                                                                  (world-pile w))]
        [else w]))

(check-expect (key-handler (make-world (make-tetra (make-posn 5 10) (cons (make-block 5 10 'red) (cons (make-block 6 10 'red) empty)))
                                       (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))) "right") 
              (make-world (make-tetra (make-posn 6 10) (cons (make-block 6 10 'red) (cons (make-block 7 10 'red) empty)))
                                      (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))))
;;; score
;;; World -> Number
;;; counts number of tetras in pile
(define (score w)
  (/ (count-pile (world-pile w)) 4))

(check-expect (score (make-world (make-tetra (make-posn 5 10) (cons (make-block 5 10 'red) (cons (make-block 6 10 'red) empty)))
                                       (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty)))) .5) 
;;; count-pile
;;; Bset -> Number
;;; counts number of blocks in pile

(define (count-pile bs)
  (cond [(empty? bs) 0]
        [(cons? bs) (+ 1 (count-pile (rest bs)))]))

(check-expect (count-pile (cons (make-block 5 0 'red) (cons (make-block 10 0 'red) empty))) 2)

;;; display-score
;;; World -> Image
;;; displays final score
(define (display-score w)
  (place-image (text "Game Over" 12 "red") 50 15
               (place-image (text (number->string (score w)) 12 "red") 50 40 BACKGROUND)))

(check-expect (display-score (make-world (make-tetra (make-posn 6 10) (cons (make-block 6 10 'red)
                                                                            (cons (make-block 5 0 'red) empty)))
                                                                                  (cons (make-block 10 0 'red)
                                                                                        (cons (make-block 7 10 'red)
                                                                                              (cons (make-block 8 0 'red)
                                                                                                    (cons (make-block 6 10 'red)empty))))))
                                                     (place-image (text "Game Over" 12 "red") 50 15 (place-image (text "1" 12 "red") 50 40 BACKGROUND)))

;;;BIG BANG
(big-bang  INITIAL-WORLD
          (on-tick next-world .5)
          (to-draw world->scene)
          (on-key key-handler)
          (stop-when at-top-pile? display-score))
