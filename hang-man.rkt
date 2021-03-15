;; hang-man for REPL Scheme


;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define word-to-guess null)
(define partial-sol null)

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))





;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))

          

;;;
;;  PURELY FUNCTIONAL
;;


(define (occurrences word char) 
   (let  ( x (word.split("(?!^)"))  )     )
    


)


(define (indices word char) null)
  
(define (replace-indices word idx new) null)


(define (noOfHits hidden) 0)


;; Side effects
;; IO(String)
(define (restart)
  (begin
    ;; some statements
    ;; last statement
    (game-status)))


;; Char -> IO(String)
(define (guess char) (void))


;; IO(String)
(define (solve word) (void))


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
(define (words-containing all-words char ) null)


;; p: all-words as list of list of char
;;  : chars as a list of char
(define (words-containing-ext all-words chars) null)

;; IO([String])
;; this is very hard.
(define (sieve chars) (void))

