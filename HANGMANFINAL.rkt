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

;;(display glossary)


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


(define (occurrences word x) ;;; F1 : i
    (define (occurencesI word x occ)
        (cond
            ;;; the reccursion is broken when the length of the word equals to 0
            [(equal?(length (indicies word)) 0) 
                occ
            ]
            ;;; if the first index of the word equals to the char specified then occurences is increased 
            [(equal? (car (indicies word)) x )
            ;;; the fucntion is then called again with all characters infront of the first index 
                (occurrences   (list->string(cdr (indicies word))) x (+ occ 1))
            ]

            [else
            ;;; the occurences are not rasied and the function is reccured
                (occurrences   (list->string(cdr (indicies word))) x occ)
            ]
        )
    )
    
)


(define (indicies str);;; <- F1 : ii
  (cond
    [empty? str
    str
    ]
    [(list? str) ;;; this function is simple it uses an inbuilt function 
                 ;;; to convert a string into a list of char
     str
     ]
    [else
     (string->list str)
     ]
    )
)
  
(define (replace-indices lst ind ele)
    
    ( define (replace-indicesI lst ind ele acc str max) ;;recursivly compares whether the index of the car of lst equals to the car of the ind var 
        (cond
              [(and (empty? lst) (empty? ind) ) ;; break point : if the lst is empty and the ind list is empty
             (string->list str) ;; converts the str to list
            ]
          
            [(and (not(equal? acc max)) (empty? ind) ) ;;; this case insures that even when ind is empty the func will continue to add "*" char
             (replace-indicesI (rest lst) '() ele (+ acc) (string-append str (string(car lst))) max) ;; calls func again with cdr of lst and empty ind 
             ]

            [(equal? acc (car ind)) ;; if the counter (acc) is equal to the car of ind then the ele is appended to the str var
                (replace-indicesI (rest lst) (cdr ind) ele (+ acc 1) (string-append str (string ele)) max) ;; function is called with the cdr of lst and ind
            ]

            [else  ;; function is called with the cdr of lst and ind
                (replace-indicesI (rest lst) ind ele (+ acc 1) (string-append str (string(car lst))) max)
            ]
        )
    )
    
    ;;; extra arguments : 0 (counter/acc) "" (string where the final output is stored) length of list (the maximum ammount of times the function should reccur)
    (replace-indicesI lst ind ele 0 "" (length lst))
)

(define (noOfHits lst );;; F1 : iii
  (define (noOfHitsI lst hitemp)
    (cond
            [(equal?(length lst) 0) ;;; this function returns the number of hits once the acc reaches 
                                   ;;; the length of 0  
              hitemp
            ]
            [(not(equal? (car lst) #\* )) 
                (noOfHits( cdr lst) (+ hitemp 1))
            ]
            [else 
                (noOfHits(cdr lst) hitemp)
            ]

            
        )
  )
)

;; Side effects
;;functions for F2
(define (findwtg wrdlist) ;; remade
   
    (define (findwtgI wrdlist rndind acc wrd)
        (cond
            [(not(empty? wrd)) ;;; if word contains a value then return that value
                wrd
            ]
            [(equal? acc rndind) ;; if acc is the same value as the random number then pass the car of the list into wrd 
                (findwtgI wrdlist 0 0 (car wrdlist))
            ]
            [else
                (findwtgI (cdr wrdlist) rndind (+ acc 1) null) ;; add 1 to acc and pass cdr through the reccured function
            ]
        )
    )
    ;;; extra arguments : wrdlist (glossary) rndind (random number in the range of the length of the glossary) acc (counter) wrd (a null value to be set to the final output)
    (findwtgI wrdlist (random (length glossary)) 0 null)
)

(define (genpartsol )
  (cond 
    [(equal? (length word-to-guess) (length partial-sol) )
     partial-sol
    ]
    [else
      (set! partial-sol(append partial-sol '(#\*)))
      (genpartsol)
    ]

  )
)

(define (findind char lst)
  (define (findindI char lst acc indlst)
    (cond 
    
      [(equal? (length lst) 0) ;; if the list of char is empty then return indlst
        indlst
      ]

      [(equal? (car lst) char) 
        (findindI char (cdr lst) (+ acc 1) (append indlst (list acc))) ;;; if the car of the lst is equal to the char then add the acc (index to a list)
      ]

      [else 
        (findindI char (cdr lst) (+ acc 1) indlst) ;; reccurs the function , increases the acc and passes an unchanged index list 
      ]

    )
  )
  (findindI char lst 0 '())
)

(define (guess gChar)
  (begin 
   (set! plays (+ plays 1))    
  )
  ;; sets a new partial solution , based on the char entered and the index's of where the character exists
  (set! partial-sol(replace-indices partial-sol (findind gChar word-to-guess) gChar)) 
  (set! hits (noOfHits partial-sol))
    (cond
      ;; increasing failiures if the character does not appear in the word
      [(equal? (occurrences partial-sol gChar ) 0) 
        (set! failures (+ failures 1))
      ]  
    )
  ;;; resetting index list
  (set! indexlst null)
  (game-status)
)


;; IO(String)
(define (restart)
  (begin
    (set! word-to-guess(findwtg glossary)) ;; sets word to guess to a random word in the glossary 
    (set! partial-sol (genpartsol))
    (define indexlst null) ;;; <- this var was created to complete f2
    (set! hits 0)
    (set! plays 0)
    (set! failures 0)
    (set! total-failures 6)
    (set! total-hits (length word-to-guess))
    (game-status)))


;; Char -> IO(String)
;;(define (guess char) (void))


;; IO(String)
(define (solve word) ;;; CURRENTLY NOT WORKING
  (define (solveI word acc guessedwrd wtg)
    (cond
        [(equal? acc (length word-to-guess))  ;;; when the acc equals to the length of the word then simulate a guess 
            (set! partial-sol (string->list word)) 
            (set! hits (noOfHits partial-sol))
            (set! plays (+ plays 1)) 
            (game-status)
        ]
        [(equal? (car guessedwrd) (car wtg)) ;;; increase acc and reccur function if the car of both the guessed word and wtg are equal
            (solveI word (+ acc 1) (cdr guessedwrd)(cdr wtg))
        ]
        [else ;; this is the fail state , if the acc does not equal to the length of the correct word
            (set! plays (+ plays 1))
            (set! failures (+ failures 1))
            (game-status)
        ]
    )
  )
    (solveI word 0 (string->list word) word-to-guess)
)


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
(define (words-containing lst ele)
            (cond 
                [(empty? lst)
                 empty
                ]
                [(member ele (car lst)) ;;; if the list[0] has contains the element 
                    ;;; create a list with list[0] and the reccusion of the function
                    (cons (car lst)(words-containing (cdr lst) ele ))
                ]
                [else ;;; otherwise reccur the function without returning the list[0] into the list
                    (words-containing (cdr lst) ele )
                ]
            )
)
;; p: all-words as list of list of char
;;  : chars as a list of char
(define (words-containing-ext lst ele) 
  ;;; to allow for multiple char to be added a lambda calls the words containing function 
  ;;; with one of the char at a time thanks to foldl
  (foldl
   (lambda (x y)
     ( words-containing y x ))
    lst ele)
)
;; IO([String])
;; this is very hard. ;;; NOT COMPELTED !!!
(define (sieve chars) (void))


;;;TESTIING/GAME
(restart)
;(display word-to-guess)
;(guess #\a)
;(guess #\b)
;(guess #\c)
;(guess #\d)
;(guess #\e)

;(solve "guessedword")
;(solve (list->string word-to-guess))


