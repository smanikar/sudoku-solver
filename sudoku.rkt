#lang racket

;; A cell is
;; - number
;; - #f

;; A zone is 
;; - vector of cell
;; - m (rows) x n (cols) vector of cells

;; A board is 
;; - vector of zones.
;; - x (rows) x y (cols) vector of zones

;; Sudoku dimensions
;; -----------------
;; Board - m * n * x * y cells
;;       - m * x rows
;;       - n * y cols
;;       - x * y zones

;; Zone  - m * n cells
;;       - m rows
;;       - n cols

;;                 |<--n-->|
;; +-------+-------+-------+ - 
;; | 1 2 3 | 4 5 6 | 7 8 9 | | 
;; | 4 5 6 | 7 8 9 | 1 2 3 | m 
;; | 7 8 9 | 1 2 3 | 4 5 6 | | 
;; +-------+-------+-------+ - 
;; | 2 3 4 | 5 6 7 | 8 9 1 |   
;; | 5 6 7 | 8 9 1 | 2 3 4 |  
;; | 8 9 1 | 2 3 4 | 5 6 7 |   
;; +-------+-------+-------+   
;; | 3 4 5 | 6 7 8 | 9 1 2 |   
;; | 6 7 8 | 9 1 2 | 3 4 5 |   
;; | 9 1 2 | 3 4 5 | 6 7 8 |   
;; +-------+-------+-------+ 

(define ex0 #(#(1 2 3 4 5 6 7 8 9) 
              #(4 5 6 7 8 9 1 2 3)
              #(a 8 9 1 2 3 4 5 6)
              #(2 #f 4 5 6 7 8 9 1)
              #(5 6 7 8 9 1 2 3 4)
              #(b 9 1 2 3 4 5 6 7)
              #(3 4 5 6 7 8 9 1 2)
              #(6 7 8 9 1 2 3 4 5)
              #(c 1 2 3 4 5 6 7 8)))

(define ex1 #(#(1 2 3  4 5 6  7 8 9) 
              #(4 5 6  7 8 9  1 2 3)
              #(a 8 9  1 2 3  4 5 6)
              
              #(2 3 4  5 6 7  8 9 1)
              #(5 6 7  8 9 1  2 3 4)
              #(b 9 1  2 3 4  5 6 7)))

(define (board/row->board/zone b x y m n)
  (if (and (zero? (remainder x m))
           (zero? (remainder y m)))
      (for/vector ([i (range (* m n))])
        (for/vector ([j (range n)])
          (vector-take b m)
        
        

;; --------------------------------------
;; transpose : board/rows -> board/cols

(define (transpose b m n)
  (for/vector ([i (range n)])
    (for/vector ([j (range m)])
      (vector-ref (vector-ref b j) i))))

(transpose ex0 9 9)
(printf "\n")
(transpose ex1 6 9)
(printf "\n")
(transpose (transpose ex1 6 9) 9 6)

;(board/rows->board/cols ex0 9 9)
;; --------------------------------------
;; print-row : row num -> 
;;  Prints a row of the board

(define (print-row r n)
  (printf "| ")
  (for ([j (range n)])
    (define v (vector-ref r j))
    ;(printf "~a" v)
    (cond
      [(zero? (remainder (add1 j) 3)) 
       (printf (string-append (~a v #:width 1) " | "))]
      [else 
       (printf (string-append (~a v #:width 1) " "))]))
  (printf "\n"))

;(print-row #(1 2 3 4 5 6 7 8 9) 9)

;; --------------------------------------
;; print-board : board num num num num ->
;;  Prints a board

(define (print-board b m n)
  (printf"+-------+-------+-------+\n")
  (for ([i (range m)])
     (cond
       [(zero? (remainder (add1 i) 3))
        (print-row (vector-ref b i) n)
        (printf "+-------+-------+-------+\n")]
       [else
        (print-row (vector-ref b i) n)])))

(print-board ex0 9 9)  

;; solve-sudoku : board -> (listof board) num
;;  Returns all possible soutions to given sudoku 'b' and the number of solutions
;(define (solve-sudoku b m n x y)
; (define-values (nx ny) ((* m x) (* n y))))
; (for ([i in nx])