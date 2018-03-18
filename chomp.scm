#lang scheme

(define (print_table table)
  (define (print_single_row list)
    (cond [(null? list) (display "\n")]
          [else (display " ")
                (display (car list))
                (print_single_row (cdr list))]))
  (cond [(null? table) (display "\n")]
        [else (print_single_row (car table))
              (print_table (cdr table))]))

(define (reverse_given_list l)
      (define (temp input output)
        (cond [(null? input) output]
              [else (temp (cdr input) (cons (car input) output))]))
      (temp l '()))

(define (length list)
  (let loop ((list list) (counter 0))
    (cond [(null? list) counter]
          [else (loop (cdr list) (+ 1 counter))])))

(define (eat_cookies coordinates table)
  (define (temp rowN colN table)
    (define (eat_cookies_row_wise rowN colN table empty_table)
      (define (create_empty_row_of_length x emptyRow)
        (cond [(= x 0) emptyRow]
              [(create_empty_row_of_length (- x 1) (cons 0 emptyRow))]))
      (define (empty_a_row_starting_from colN colNPerm row newRow)
        (cond [(> colN 1) (empty_a_row_starting_from (- colN 1) colNPerm (cdr row) (cons (car row) newRow))]
              [else (append (reverse_given_list newRow) (create_empty_row_of_length (- (+ 1 (length (car table))) colNPerm) '()))]))

      (cond [(> rowN 1) (eat_cookies_row_wise (- rowN 1) colN (cdr table) (cons (car table) empty_table))]
            [else (append (reverse_given_list empty_table) (cons (empty_a_row_starting_from colN colN (car table) '()) (cdr table)))]))
    (cond [(< rowN (+ 1 (length table))) (temp (+ rowN 1) colN (eat_cookies_row_wise rowN colN table '()))]
          [else table]))
  (temp (car coordinates) (car (cdr coordinates)) table))

(define (playcomputer table)
  (define (temp liste y x)
    (cond [(scan liste y x) (cons y (cons x null))]
          [else (temp liste (random (+ 1 (length table))) (random (+ 1 (length (car table)))))]))
  (define (search arr i)
    (if (= i 1) (car arr)
      (search (cdr arr) (- i 1))))
  (define (scan liste y x)
    (cond [(or (= x 0) (= y 0)) false]
        [(= (search (search liste y) x) 1) true]
        [else false]))
  (temp table 1 1))

(define (lose? table)
  (cond [(and (= (cadar table) 0) (= (car (car (cdr table))) 0) (= (car (cdr (car (cdr table)))) 0)) true]
        [else false]))

(define (check_cookie_1? input table)
  (let loop ((x (car input)) (y (car (cdr input))) (table table))
    (cond [(> x 1) (loop (- x 1) y (cdr table))]
          [(= x 1) (loop 0 y (car table))]
          [(> y 1) (loop x (- y 1) (cdr table))]
          [(= y 1) (loop x 0 (car table))]
          [(= table 1) true]
          [else false])))

(define (get_input table)
  (let loop ((input (map string->number (string-split (read-line)))) (table table))
    (cond [(and (= (car input) 1) (= (car (cdr input)) 1)) (begin
                                                             (display "You can not eat the poisoned cookie, please try again.\n")
                                                             (loop (map string->number (string-split (read-line))) table))]
          [(and (> (car input) 0) (> (car (cdr input)) 0) (< (car input) (+ 1 (length table))) (< (car (cdr input)) (+ 1 (length (car table))))) (cond [(check_cookie_1? input table) input]
                                                                                                           [else (begin
                                                                                                                   (display "There is no cookie to eat, please try again.\n")
                                                                                                                   (loop (map string->number (string-split (read-line))) table))])]
          [else (begin
                  (display "You are out of table bounds, please try again.\n")
                  (loop (map string->number (string-split (read-line))) table))])))

(define (chomp table)
  (let loop ((table table) (turn_decider 0) (turn_count 0))
    (if (> turn_count 0)
        (begin
         (cond [(= 0 (modulo turn_decider 2)) (begin
                                           (display "After computers turn;\n")
                                           (print_table table))]
          [else (begin
                  (display "After your turn;\n")
                  (print_table table))]))
        (print_table table))
    (cond [(= 0 (modulo turn_decider 2)) (cond [(lose? table) (display "You lose!")]
                                               [else (loop (eat_cookies (get_input table) table) (+ 1 turn_decider) (+ 1 turn_count))])]
          [else (cond [(lose? table) (display "You win!")]
                        [else (loop (eat_cookies (playcomputer table) table) (+ 1 turn_decider) (+ 1 turn_count))])])))


(display "2 = Poisoned cookie\n\n")
(display "1 = Cookie\n\n")
(display "0 = Empty\n\n")
(display "Enter your inputs by row number first, column number second. (e.g. 2 3)\n")
(display "Top-left corner is 1 1 and bottom-right corner is 4 5\n\n")
(display "You start first!\n\n")
(chomp '((2 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1)))
