#lang racket
(require racket/gui)

(define lista
  '())

(define (gerar-resposta input db)
  (let* ([case-flag (string=? (string-downcase input) input)]
         [f-filtro (λ (x) (string-prefix? x (string-downcase input)))]
         [l-filtrada (filter f-filtro db)]
         [f-ordenada (λ (word1 word2) (< (string-length word1) (string-length word2)))]
         [l-ordenada (if case-flag
                         (sort l-filtrada f-ordenada)
                         (map string-upcase (sort l-filtrada f-ordenada)))])
    (cond
      [(>= (length l-ordenada) 3) (take l-ordenada 3)]
      [(= (length l-ordenada) 2) (append l-ordenada (list ""))]
      [(= (length l-ordenada) 1) (list (car l-ordenada) "" "")]
      [else '("" "" "")])))

(define test-db
  (string-split (file->string"substantivos.txt") "\n"))

(define tela
  (new frame%
       [label "Previsão de Palavras"]
       [width 200]
       [height 300]
       [border 10]
       [spacing 10]))

(define caixa-texto
  (new text-field%
       [label #f]
       [parent tela]
       [style '(multiple)]
       [callback (λ (a e) (let* ([texto (string-split (send caixa-texto get-value) " ")]
                                 [texto (if (null? texto)
                                            ""
                                            (last texto))])
                            (set! lista (gerar-resposta texto test-db))
                            (print lista)
                            (update-msg lista)))]))

(define (insert-word f lista)
  (if (null? (reverse (cdr (reverse (string-split (send caixa-texto get-value) " ")))))
      (cond
        [(= f 1) (send caixa-texto set-value (string-join (cons (first lista) (cons " " '()))))]
        [(= f 2) (send caixa-texto set-value (string-join (cons (second lista) (cons " " '()))))]
        [(= f 3) (send caixa-texto set-value (string-join (cons (third lista) (cons " " '()))))])
      (let ([lista-palavras (string-split (send caixa-texto get-value) " ")])
        (cond
          [(= f 1) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (first lista)) '(" "))))]
          [(= f 2) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (second lista)) '(" "))))]
          [(= f 3) (send caixa-texto set-value (string-join (append (take lista-palavras (- (length lista-palavras) 1)) (list (third lista)) '(" "))))]))))

; mudar para atualizar e nao criar novos botoes
(define (update-msg l)
  (send btn1 set-label (first l))
  (send btn2 set-label (second l))
  (send btn3 set-label (third l)))

(define btn1
  (new button%
       [label ""]
       [parent tela]
       [callback (λ (a e) (insert-word 1 lista))]))

(define btn2
  (new button%
       [label ""]
       [parent tela]
       [callback (λ (a e) (insert-word 2 lista))]))

(define btn3
  (new button%
       [label ""]
       [parent tela]
       [callback (λ (a e) (insert-word 3 lista))]))
  
(send tela show #t)