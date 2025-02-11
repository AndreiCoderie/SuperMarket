#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define maxPossible (expt 2 31))


(define-struct counter (index tt et queue close) #:transparent)


; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue 0))

(define (counter-empty? C)
  (if (and (queue-empty? (counter-queue C)) (= 0 (counter-tt C))) #t #f))

;;FUNCTII COPIATE DIN ETAPA 3

(define (update f counters index)
  (update-helper f counters index '()))

(define (update-helper f counters index acc)
  (cond
    ((null? counters) acc)
      ((= index (counter-index (car counters))) (update-helper f (cdr counters) index (append acc (list (f (car counters))))))
      (else (update-helper f (cdr counters) index (append acc (list (car counters)))))))




(define tt+
  (lambda (minutes)
    (lambda (C)
      (match C
        [(counter index tt et queue close)
     (struct-copy counter C (index index) (tt (+ minutes tt)) (et et))
     ]))))
(define et+
    (lambda (minutes)
    (lambda (C)
      (match C
        [(counter index tt et queue close)
     (struct-copy counter C (index index) (tt tt) (et (+ minutes tt)))
     ]))))


(define ttandet+
  (lambda (minutes)
    (lambda (C)
      (match C
        [(counter index tt et queue close)
     (struct-copy counter C (index index) (tt (+ minutes tt)) (et (+ minutes et)))
     ]))))


(define (general-func f L)
  (if (or (null? L) (null? (general-func-helper f L maxPossible '()))) (cons 0 maxPossible)
  (cons (counter-index (car (general-func-helper f L maxPossible '()))) (f (car (general-func-helper f L maxPossible '()))))))


;; presupunem ca nu avem un tt mai mare de 10^31, ceea ce este destul de logic

(define (general-func-helper f L checker acc)
  (cond
    ((null? L) acc)
    ((= 1 (counter-close (car L))) (general-func-helper f (cdr L) checker acc))
    ((< (f (car L)) checker)  (general-func-helper f (cdr L) (f (car L)) (cons (car L) acc)))
    (else (general-func-helper f (cdr L) checker acc))
  ))

(define (nonNullQueue L)
    (nonNullQueue-helper L '()))
  
  (define (nonNullQueue-helper L acc)
    (cond
      ((null? L) acc)
      (else
       (if (queue-empty? (counter-queue (car L))) (nonNullQueue-helper (cdr L) acc) 
        (nonNullQueue-helper (cdr L) (append acc (list (car L))))))))


(define (min-tt L) (general-func counter-tt L)) ; folosind funcția de mai sus
(define (min-et L)
      (general-func counter-et (nonNullQueue L)))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
           ; (if (= 0 (counter-et C)) (struct-copy counter C [tt items] [et items] [queue (enqueue (cons name items) (counter-queue C))])
               (struct-copy counter C  [tt (counter-tt ((tt+ items) C))] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C) )])

                (struct-copy counter C [tt (counter-tt ((tt+ items) C))] [et (counter-et C)]  [queue (enqueue (cons name items) (counter-queue C))]))))

    


; FUNCTII DIN ETAPA 3 PT X

(define (subWithoutNegative x y)
  (if (> 0 (- x y)) 0
      (- x y)))

(define (pass-time-through-counter minutes)
  (λ (C)
    (if (= minutes (counter-et C))
        (if (queue-empty? (dequeue (counter-queue C))) (struct-copy counter C [tt (subWithoutNegative (counter-tt C) minutes)] [et 0] [queue empty-queue])
            (struct-copy counter C [tt (subWithoutNegative (counter-tt C) minutes)] [et (cdr (top (dequeue (counter-queue C))))] [queue (dequeue (counter-queue C))]))
    (struct-copy counter C [index (counter-index C)] [tt (subWithoutNegative (counter-tt C) minutes)] [et (subWithoutNegative (counter-et C) minutes)] [queue (counter-queue C)]))))


; functie care obtione perechile dintre index si numele din queue
(define (pass-time C x)
  (pass-time-helper x C '()))

(define (pass-time-helper x C acc)
  (if (counter-empty? C) acc
  (if (queue-empty? (counter-queue C)) acc
  (if  (> (counter-et C) x)
      acc
      (if (queue-empty? (dequeue (counter-queue C))) (append acc (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- x (counter-et C)) )))
      (pass-time-helper (subWithoutNegative x (counter-et C ))      (make-counter (counter-index C) (subWithoutNegative (counter-tt C) x) (cdr (top (dequeue (counter-queue C)))) (dequeue (counter-queue C)) (counter-close C))
                        (append acc  (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- x (counter-et C))))))))))) 

(define (pass-time-list L x)
  (pass-time-list-helper L x '()))

(define (pass-time-list-helper L x acc )
  (if (null? L) acc
      (if (null? (pass-time (car L) x)) (pass-time-list-helper (cdr L) x acc)
          (pass-time-list-helper (cdr L) x (append acc (pass-time (car L) x))))))

(define (sort-by-cdr-number lst) 
  (define (object-greater? a b)
    (> (cdr a) (cdr b)))
  (sort lst object-greater?))

; functie care trece casele prin timpul x
(define (pass-time-counter x)
  (lambda (C)
    (if (queue-empty?(counter-queue C)) C
    (if (> (counter-et C) x)
        ((pass-time-through-counter x) C)
        (if (queue-empty? (dequeue (counter-queue C))) ((pass-time-counter (subWithoutNegative x (counter-et C))) (struct-copy counter C [index (counter-index C)]
            [tt 0] [et 0] [queue empty-queue]))
        ((pass-time-counter (- x (counter-et C)))
                            (struct-copy counter C
                                         [index (counter-index C)]
                                         [tt (subWithoutNegative (counter-tt C) (counter-et C))]
                                         [et (cdr (top (dequeue (counter-queue C))))]
                                         [queue (dequeue (counter-queue C))]
                                         [close (counter-close C)])))))))


; functie pt close

(define close-the-counter
  (lambda (C)
    (struct-copy counter C [index (counter-index C)]
                 [tt (counter-tt C)]
                 [et (counter-et C)]
                 [queue (counter-queue C)]
                 [close 1])))
(define (is-in-List L index)
  (if (null? L) #f
  (if (= index (counter-index (car L))) #t
      (is-in-List (cdr L) index))))

; functii pt ensure


  (define (averageee L)
    (/ (average-helper L 0) (average-counter L 0 )))
  (define (average-helper L acc)
    (if (null? L) acc
        (if (= (counter-close (car L)) 1) (average-helper (cdr L) acc) 
        (average-helper (cdr L) (+ acc (counter-tt (car L)))))))
  (define (average-counter L acc)
    (if (null? L) acc
        (if (= (counter-close (car L)) 1) (average-counter (cdr L) acc) 
        (average-counter (cdr L) (+ acc 1)))))

  (define (last_element L)
    (if (null? (cdr L)) (car L)
        (last_element (cdr L))))

; functii pt delay


  (define (contains list x)
    (cond [(null? list) #f]
         [(equal? (car list) x) #t]
         [else (contains (cdr list) x)]))
  
  (define (make-it-List L)
    (make-it-List-helper L '()))
  
  (define (make-it-List-helper L acc)
    (if (null? L) acc
        (make-it-List-helper (cdr L) (append acc (list (counter-index (car L)))))))

  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (beauty-output L)
  (if (null? L)
      '()
      (if (queue-empty? (counter-queue L)) '()
          (cons (counter-index L) (counter-queue  L)))))

(define (beauty-output-final L)
  (if (null? L) '()
  (if (null? (beauty-output (car L))) (beauty-output-final (cdr L))
      (append (list (beauty-output (car L))) (beauty-output-final (cdr L))))))



(define (serve-helper requests queueOut fast-counters slow-counters)

  (if (null? requests)
      (cons queueOut (beauty-output-final
                      (append fast-counters slow-counters)))

       (match (car requests)

      ;  [(list 'delay index minutes)
 ;            (serve-helper (cdr requests) queueOut fast-counters slow-counters)]
                [(list 'delay index minutes)
        (if (contains (make-it-List fast-counters) index) (serve-helper (cdr requests) queueOut (update (ttandet+ minutes) fast-counters index) slow-counters)
            (serve-helper (cdr requests) queueOut fast-counters (update (ttandet+ minutes) slow-counters index)))
        ]

                  [(list 'close index)
          (if (is-in-List fast-counters index) (serve-helper (cdr requests) queueOut (update (lambda (C) (close-the-counter C)) fast-counters index) slow-counters)
          (serve-helper (cdr requests) queueOut fast-counters (update (lambda (C) (close-the-counter C)) slow-counters index)))]

                 [(list 'ensure average)
 
                 (if (> (averageee (append fast-counters slow-counters)) average)
            (serve-helper requests queueOut fast-counters (append slow-counters (list (empty-counter (+ (counter-index (last_element slow-counters)) 1)))))
            (serve-helper (cdr requests) queueOut fast-counters slow-counters))]

          [(list name n-items)
                  (cond
                    ((> n-items ITEMS) (serve-helper (cdr requests) queueOut fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters)))))
                    (else
                     (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                         (serve-helper (cdr requests) queueOut (update (λ (C) ((add-to-counter name n-items) C)) fast-counters  (car (min-tt fast-counters))) slow-counters)
                         (serve-helper (cdr requests) queueOut fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters)))))))]
         
        
        
         [x
         (if (null? (pass-time-list (append fast-counters slow-counters) x)) (serve-helper (cdr requests) queueOut
                                                                                           (map (pass-time-through-counter x) fast-counters) (map (pass-time-through-counter x) slow-counters))
             (serve-helper (cdr requests) (append queueOut (pass-time-list (append fast-counters slow-counters) x)) (map (pass-time-counter x) fast-counters)
                           (map (pass-time-counter x) slow-counters)))])))
                           
             
 






(define (serve requests fast-counters slow-counters)
 
      (serve-helper requests '() fast-counters slow-counters))