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
(define occ 0) ;;; <- this var created by me to complete f1
(define indexlst null) ;;; <- this part was created to complete f2
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


(define (occurrences word x) ;;; <- completed in f1
  
        (cond
            [(equal?(length (indicies word)) 0)
                occ
            ]

            [(equal? (car (indicies word)) x )
                (set! occ (+ occ 1))
                (occurrences   (list->string(cdr (indicies word))) x )
            ]

            [else
                (occurrences   (list->string(cdr (indicies word))) x )
            ]
        )
    
)


(define (indicies str);;; <- completed in f1
  (cond
    [(list? str)
     str
     ]
    [else
     (string->list str)
     ]
    )
)
  
(define (replace-indices lst ind ele)
    
    (let ([str ""][count 0])
        (for-each (lambda (arg)
                (cond

         [(empty? ind)
          (set! count (+ count 1))
            (set! str (string-append str (string arg)))
          ]
         
        [(equal? count (length lst))
         lst
         ]
                  
        [(equal? count (car ind))
           (set! count (+ count 1))
           (set! str (string-append str (string ele)))
           (set! ind ( cdr ind))
         ]
        [else
            (set! count (+ count 1))
            (set! str (string-append str (string arg)))
            
        ]
        )

                 23)
       lst)
      (cond
        [empty?
         (set! partial-sol(string->list str))
         ]
        [else
         lst
        ]
      )
      )

)

(define (noOfHits lst );;; <- completed in f1
    (cond
            [(equal?(length lst) 0)
                occ
            ]

            [(not(equal? (car lst) #\* ))
                (set! occ (+ occ 1))
                (noOfHits( cdr lst))
            ]

            [else 
                (noOfHits(cdr lst))
            ]

            
        )
)

;; Side effects
;;functions for F2
(define (findwtg wrdlist) 
   (let ([count 0] [randind (random (length glossary))])
        (for-each 
        
        (lambda (arg)
                (cond
        [(equal? count randind )
          (set! word-to-guess arg )
          (set! randind null)
         ]
                  
        [else
            (set! count (+ count 1))

            
        ]
        )

                glossary) 
       glossary)
      )
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
  
    (cond
      [(equal? (length lst) 0)
        indexlst]

      [(equal? (car lst) char)
        (set! occ (+ occ 1))
        (set! indexlst(append indexlst (list (- occ 1))))
        (findind char (cdr lst))]


      [else 
        (set! occ (+ occ 1))
        (findind char (cdr lst))]   
     )
)

(define (guess gChar)
  (set! occ 0)
  (begin 
   (set! plays (+ plays 1))   
  )
  (findind gChar word-to-guess)
  (set! occ 0)
  (replace-indices partial-sol indexlst gChar)
  (set! hits (noOfHits partial-sol))
  (set! occ 0)
  (occurrences partial-sol gChar )
    (cond
      [(equal? occ 0)
        (set! failures (+ failures 1))
      ]  
    )
  (set! occ 0)
  (set! indexlst null)
  (game-status)
)


;; IO(String)
(define (restart)
  (begin
    (findwtg glossary) ;; sets word to guess to a random word in the glossary 
    (set! partial-sol (genpartsol))
    (set! occ 0) ;;; <- this var created by me to complete f1
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
  (let([x 0] [y (string->list word)] [z word-to-guess])
    (for-each(lambda (arg)
          (cond
            [(equal? x (length word-to-guess))
              (game-status)
            ]

            [(equal? (car y) (car z ))
              (set! x (+ x 1))
              (set! y (cdr y))
              (set! z (cdr z))
            ]

            [else
              (game-status)
            ]
          )
    y)
    y)
    (game-status)
    )
)


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

(restart)
(display word-to-guess)
(guess #\a)
(guess #\b)
(guess #\c)
(guess #\d)
(guess #\e)
;;(solve (list->string word-to-guess))



