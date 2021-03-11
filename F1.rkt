
(define occ 0)
(define hits 0)

(define (indicies str)
    (string->list str)
)

(define (noOfHits lst )
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


(define (occurrences word x)
  
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

(define (replace-indices lst ind ele)
    (cond

        [(equal?(length ind) 0)
            lst
        ]

        [(empty? ele)
            lst
        ]


        [else
            (list-set lst (car ind) (car ele))
            (replace-indices lst (cdr ind)(cdr ele))
        ]

    )

)




;; F1 : i / F1 : ii
(occurrences "word" #\o )
(set! occ 0)
(occurrences "woord" #\o )
(set! occ 0)
(occurrences "wooord" #\o )
(set! occ 0)

;; F1 : iii
;;(noOfHits '(#\a #\* #\* ))
;;(set! hits 0)
;;(noOfHits '(#\a #\a #\* ))
;;(set! hits 0)

;;F1 : iv
;(replace-indices '(#\a #\* #\* ) '(1 2) #\b)
;(replace-indices '(#\a #\* #\* ) '() #\b)


  