

(define list-of-words '("car""bar""bus"))
(define glossary (map string->list list-of-words))


(define (words-containing lst ele)
            (cond 
                [(empty? lst)
                 empty
                ]
                [(member ele (car lst))
                    (cons (car lst)(words-containing (cdr lst) ele ))
                ]
                [else
                    (words-containing (cdr lst) ele )
                ]
            )
)

(define (words-containing-ext lst ele)
  (foldl
   (lambda (x y)
     ( words-containing y x ))
    lst ele)
)

(words-containing glossary #\a)
(words-containing-ext glossary '(#\b #\a ))
