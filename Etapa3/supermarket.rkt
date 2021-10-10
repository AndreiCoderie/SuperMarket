#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define maxPossible (expt 2 31))


(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeuea
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (counter-empty? C)
  (if (and (queue-empty? (counter-queue C)) (= 0 (counter-tt C))) #t #f))

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
        [(counter index tt et queue)
     (struct-copy counter C (index index) (tt (+ minutes tt)) (et et))
     ]))))
(define et+
    (lambda (minutes)
    (lambda (C)
      (match C
        [(counter index tt et queue)
     (struct-copy counter C (index index) (tt tt) (et (+ minutes tt)))
     ]))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (if (queue-empty? (counter-queue C))
            (if (= 0 (counter-et C)) (struct-copy counter C [tt items] [et items] [queue (enqueue (cons name items) (counter-queue C))])
                (struct-copy counter C  [tt (counter-tt ((tt+ items) C))] [et (+ (counter-et C) items)] [queue (enqueue (cons name items) (counter-queue C) )]))

                (struct-copy counter C [tt (counter-tt ((tt+ items) C))] [et (counter-et C)]  [queue (enqueue (cons name items) (counter-queue C))]))))

;(define add-to-counter
 ; (lambda (name)
  ;  (lambda (n-items)
   ;   (lambda (C)
    ;    (if (queue-empty (counter-queue C))
     ;       (if (= 0 (counter-et C)) (make-counter (counter-index C) (counter-tt ((tt+ n-items) C)) n-items (qneue (cons name items) ))
                ;(make-counter (counter-index C) (counter-tt ((tt+ n-items) C)) (+ (counter-et C) n-items) (list (cons name n-items)))) 
  ;(make-counter (counter-index C) (counter-tt ((tt+ n-items) C)) (cdr (car (counter-queue C))) (append (counter-queue C) (list(cons name n-items )))))))))


(define (general-func f L)
  (if (null? L) (cons 0 maxPossible)
  (cons (counter-index (car (general-func-helper f L maxPossible '()))) (f (car (general-func-helper f L maxPossible '()))))))


;; presupunem ca nu avem un tt mai mare de 10^31, ceea ce este destul de logic

(define (general-func-helper f L checker acc)
  (cond
    ((null? L) acc)
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

(define (makeList queue)
  (append (queue-left queue) (queue-right queue)))

(define (remove-first-from-counter C)   ; testată de checker
      (match C
    [(counter index tt et queue)
     (cond
       ((queue-empty? (counter-queue C)) (struct-copy counter C (index (counter-index C)) (tt 0) (et 0) (queue empty-queue)))
       (else
         
     (if (queue-empty? (dequeue (counter-queue C)))
         (struct-copy counter C (index (counter-index C)) (tt 0) (et 0) (queue empty-queue))
  (struct-copy counter C (index (counter-index C)) (tt (apply + (map cdr  (makeList (dequeue (counter-queue C)))))) (et (cdr (top (dequeue (counter-queue C))))) (queue (dequeue (counter-queue C)))))))]))


(define ttandet+
  (lambda (minutes)
    (lambda (C)
      (match C
        [(counter index tt et queue)
     (struct-copy counter C (index index) (tt (+ minutes tt)) (et (+ minutes et)))
     ]))))



; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (subWithoutNegative x y)
  (if (> 0 (- x y)) 0
      (- x y)))


(define (pass-time-through-counter minutes)
  (λ (C)
    
    (struct-copy counter C [index (counter-index C)] [tt (subWithoutNegative (counter-tt C) minutes)] [et (subWithoutNegative (counter-et C) minutes)] [queue (counter-queue C)])))

; upgrade de la pass-time, care si scoate din queue
(define (pass-time-through-counter-2 minutes)
  (λ (C)
    (if (= minutes (counter-et C))
        (if (queue-empty? (dequeue (counter-queue C))) (struct-copy counter C [tt (subWithoutNegative (counter-tt C) minutes)] [et 0] [queue empty-queue])
            (struct-copy counter C [tt (subWithoutNegative (counter-tt C) minutes)] [et (cdr (top (dequeue (counter-queue C))))] [queue (dequeue (counter-queue C))]))
    (struct-copy counter C [index (counter-index C)] [tt (subWithoutNegative (counter-tt C) minutes)] [et (subWithoutNegative (counter-et C) minutes)] [queue (counter-queue C)]))))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

;functii ajutoatoare pentru serve
  (define (contains list x)
    (cond [(null? list) #f]
         [(equal? (car list) x) #t]
         [else (contains (cdr list) x)]))
  
  (define (make-it-List L)
    (make-it-List-helper L '()))
  
  (define (make-it-List-helper L acc)
    (if (null? L) acc
        (make-it-List-helper (cdr L) (append acc (list (counter-index (car L)))))))

  (define (averageee L)
    (/ (average-helper L 0) (average-counter L 0 )))
  (define (average-helper L acc)
    (if (null? L) acc
        (average-helper (cdr L) (+ acc (counter-tt (car L))))))
  (define (average-counter L acc)
    (if (null? L) acc
        (average-counter (cdr L) (+ acc 1))))

  (define (last_element L)
    (if (null? (cdr L)) (car L)
        (last_element (cdr L))))

(define (sort-by-cdr-number lst) 
  (define (object-greater? a b)
    (> (cdr a) (cdr b)))
  (sort lst object-greater?))

; functie care obtione perechile dintre index si numele din queue
(define (pass-time C x)
  (pass-time-helper x C '()))

(define (pass-time-helper x C acc)
  (if (counter-empty? C) acc
  (if (queue-empty? (counter-queue C)) acc
  (if  (> (counter-et C) x)
      acc  
      (if (queue-empty? (dequeue (counter-queue C))) (append acc (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- x (counter-et C)) )))
      (pass-time-helper (subWithoutNegative x (counter-et C ))      (struct-copy counter C [index (counter-index C)]
                                                                                 [tt (subWithoutNegative (counter-tt C) x)]
                                                                                 [et (cdr (top (dequeue (counter-queue C))))]
                                                                                 [queue (dequeue (counter-queue C))])
                        (append acc  (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- x (counter-et C)))))))))))

(define (pass-time-list L x)
  (pass-time-list-helper L x '()))

(define (pass-time-list-helper L x acc )
  (if (null? L) acc
      (if (null? (pass-time (car L) x)) (pass-time-list-helper (cdr L) x acc)
          (pass-time-list-helper (cdr L) x (append acc (pass-time (car L) x))))))


; functie care trece casele prin timpul x
(define (pass-time-counter x)
  (lambda (C)
    (if (queue-empty?(counter-queue C)) ((pass-time-through-counter x) C)
    (if (>= (counter-et C) x)
        ((pass-time-through-counter-2 x) C)
        (if (queue-empty? (dequeue (counter-queue C))) ((pass-time-counter (subWithoutNegative x (counter-et C))) (struct-copy counter C [index (counter-index C)]
            [tt 0] [et 0] [queue empty-queue]))
        ((pass-time-counter (- x (counter-et C)))
                            (struct-copy counter C
                                         [index (counter-index C)]
                                         [tt (subWithoutNegative (counter-tt C) (counter-et C))]
                                         [et (cdr (top (dequeue (counter-queue C))))]
                                         [queue (dequeue (counter-queue C))]
                                         )))))))




(define (serve-helper requests queueOut fast-counters slow-counters)

  (define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

  (if (null? requests)
      (if (null? queueOut) (append (cons '() fast-counters) slow-counters)
      (cons queueOut (append fast-counters slow-counters)))
      (match (car requests)

        [(list 'ensure average)
         (if (> (averageee (append fast-counters slow-counters)) average)
            (serve-helper requests queueOut fast-counters (append slow-counters (list (empty-counter (+ (counter-index (last_element slow-counters)) 1)))))
            (serve-helper (cdr requests) queueOut fast-counters slow-counters))
         ]

        
       [(list 'delay index minutes)
        (if (contains (make-it-List fast-counters) index) (serve-helper (cdr requests) queueOut (update (ttandet+ minutes) fast-counters index) slow-counters)
            (serve-helper (cdr requests) queueOut fast-counters (update (ttandet+ minutes) slow-counters index)))
        ]

                [(list name n-items)
                  (cond
                    ((> n-items ITEMS) (serve-helper (cdr requests) queueOut fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters)))))
                    (else
                     (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                         (serve-helper (cdr requests) queueOut (update (λ (C) ((add-to-counter name n-items) C)) fast-counters  (car (min-tt fast-counters))) slow-counters)
                         (serve-helper (cdr requests) queueOut fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (car (min-tt slow-counters)))))))]

        
        
        
         [x
         (if (null? (pass-time-list (append fast-counters slow-counters) x)) (serve-helper (cdr requests) queueOut
                                                                                           (map (pass-time-through-counter-2 x) fast-counters) (map (pass-time-through-counter-2 x) slow-counters))
             (serve-helper (cdr requests) (append queueOut (map car (sort-by-cdr-number(pass-time-list (append fast-counters slow-counters) x)))) (map (pass-time-counter x) fast-counters)
                           (map (pass-time-counter x) slow-counters)))])))



  


(define (serve requests fast-counters slow-counters)
  
      (serve-helper requests '() fast-counters slow-counters))
     
        
