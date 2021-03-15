(require racket/trace)

;;; immersion version of replace indicies
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
    (trace replace-indicesI)
    ;;; extra arguments : 0 (counter/acc) "" (string where the final output is stored) length of list (the maximum ammount of times the function should reccur)
    (replace-indicesI lst ind ele 0 "" (length lst))
)

;;(replace-indices '(#\a #\* #\*  ) '(1 2) #\b)
;;(replace-indices '(#\a #\* #\* #\a ) '(1 2) #\b)
;;(replace-indices '(#\a #\* #\* #\a ) '(1 2 3) #\b)



;;; immersive function of find word to guess 
(define glossary '("woard" "anot" " Staat" "otahe" "suaare"))
(define word-to-guess null)

(define (findwtg wrdlist) ;; remade
   
    (define (findwtgI wrdlist rndind acc wrd)
        (cond
            [(not(empty? wrd)) ;;; if word contains a value then return that value
                (string->list wrd)
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

(set! word-to-guess(findwtg glossary))
;;word-to-guess

;;; immersive function of findind 
(define indexlst null)

(define (findind char lst)
  (define (findindI char lst acc indlst)
    (cond 
    
      [(equal? (length lst) 0)
        indlst
      ]

      [(equal? (car lst) char)
        (findindI char (cdr lst) (+ acc 1) (append indlst (list acc)))
      ]

      [else 
        (findindI char (cdr lst) (+ acc 1) indlst)
      ]

    )
  )
  (findindI char lst 0 '())
)

(set! indexlst (findind #\a word-to-guess ))
indexlst

;;; immersive function of solve 

(define (solve word) ;;; CURRENTLY NOT WORKING
  (define (solveI word acc guessedwrd wtg)
    (cond
        [(equal? acc (length word-to-guess))
            (game-status)
        ]
        [(equal? (car guessedwrd) (car wtg))
            (solveI word (+ acc 1) (cdr guessedwrd)(cdr wtg))
        ]
        [else
            (game-status)
        ]
    )
  )
    (solveI word 0 (string->list word) word-to-guess)
)


(display word-to-guess)
(guess #\a)
(guess #\b)
(guess #\c)
(guess #\d)
(guess #\e)
