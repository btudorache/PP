#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et queue closed) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 empty-queue #f))

(define (update f counters index)
  (map (lambda (x) (if (= index (counter-index x)) (f x) x)) counters))

(define tt+
  (lambda (tt-time)
    (lambda (C)
      (define updatedC (struct-copy counter C [tt (+ (counter-tt C) tt-time)]))
      updatedC)))

(define et+
  (lambda (et-time)
    (lambda (C)
      (define updatedC (struct-copy counter C [et (+ (counter-et C) et-time)]))
      updatedC)))

(define (add-to-counter name items)
  (lambda (C)
      (struct-copy counter C
                   [tt (+ (counter-tt C) items)]
                   [queue (enqueue (cons name items) (counter-queue C))]
                   [et (if (queue-empty? (counter-queue C)) (+ (counter-et C) items) (counter-et C))])))

(define (min-time counters key)
  (min-time-iter counters key (cons 0 +inf.0)))

(define (min-time-iter counters key min)
  (cond
    ; daca counters e gol se intoarce minimul
    ((null? counters)
     min)

    ; daca se gaseste un tt mai mic decat minimul curent, se continua recursivitatea cu minimul actualizat
    ((< (key (car counters)) (cdr min))
     (min-time-iter (cdr counters) key (cons (counter-index (car counters)) (key (car counters)))))

    ; daca se gasesc doi tt egali, se verifica daca cel gasit are indicele mai mic, si se actualizeaza in caz afirmativ
    ((= (key (car counters)) (cdr min))
      (if (< (counter-index (car counters)) (car min))
          (min-time-iter (cdr counters) key (cons (counter-index (car counters)) (key (car counters))))
          (min-time-iter (cdr counters) key min)))
    (else
     (min-time-iter (cdr counters) key min))))

(define (min-tt counters) (min-time counters counter-tt)) ; folosind funcția de mai sus
(define (min-et counters) (min-time counters counter-et)) ; folosind funcția de mai sus

(define (get-total-tt q)
  (get-total-tt-iter q 0))

(define (get-total-tt-iter q acc)
  (if (queue-empty? q)
      acc
      (get-total-tt-iter (dequeue q) (+ acc (cdr (top q))))))

(define (remove-first-from-counter C)
  (struct-copy counter C
               [tt (if (queue-empty? (dequeue (counter-queue C))) 0 (get-total-tt (dequeue (counter-queue C))))]
               [queue (dequeue (counter-queue C))]
               [et (if (queue-empty? (dequeue (counter-queue C))) 0 (cdr (top (dequeue (counter-queue C)))))]))

(define (pass-time-through-counter minutes)
  (lambda (C)
    (struct-copy counter C
                   [tt (if (< (counter-tt C) minutes) 0 (- (counter-tt C) minutes))]
                   [et (if (< (counter-et C) minutes) 0 (- (counter-et C) minutes))])))
  
  
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
(define (serve requests fast-counters slow-counters)
  (better-serve '() requests fast-counters slow-counters))

(define (better-serve done-clients requests fast-counters slow-counters)

  ; functiile necesare pentru a implementa requestul add-person-to-queue
  (define (add-person-to-queue name n-items)
    (if (<= n-items ITEMS)
        (better-serve
         done-clients
         (cdr requests)
         (update (add-to-counter name n-items) fast-counters (car (min-tt (filter (lambda (c) (not (counter-closed c))) (append fast-counters slow-counters)))))
         (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (lambda (c) (not (counter-closed c))) (append fast-counters slow-counters))))))
        (better-serve
         done-clients
         (cdr requests)
         fast-counters
         (update (add-to-counter name n-items) slow-counters (car (min-tt (filter (lambda (c) (not (counter-closed c))) slow-counters)))))))

  ; functiile necesare pentru a implementa requestul add-delay
  (define (add-delay index minutes)
    (better-serve
     done-clients
     (cdr requests)
     (update (tt+ minutes) (update (et+ minutes) fast-counters index) index)
     (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)))

  ; functiile necesare pentru a implementa requestul ensure
  (define (get-median-tt scounters)
    (/ (foldr (lambda (x acc) (+ acc (counter-tt x))) 0 (filter (lambda (c) (not (counter-closed c))) (append fast-counters scounters)))
       (length (filter (lambda (c) (not (counter-closed c))) (append fast-counters scounters)))))

  (define (get-next-index scounters)
    (add1 (length (append fast-counters scounters))))

  (define (ensure average scounters)
    (if (<= (get-median-tt scounters) average)
        (better-serve done-clients (cdr requests) fast-counters scounters)
        (ensure average (append scounters (list (empty-counter (get-next-index scounters)))))))

  ; functiile necesare pentru a implementa requestul remove
  (define (apply-time-passing-to-counters counters time)
    (map (pass-time-through-counter time) counters))

  (define (get-most-advanced-pair fast-counters slow-counters)
    (min-et (filter (lambda (C) (not (queue-empty? (counter-queue C)))) (append fast-counters slow-counters))))
  
  (define (get-most-advanced-index fast-counters slow-counters)
    (car (get-most-advanced-pair fast-counters slow-counters)))

  (define (get-most-advanced-value fast-counters slow-counters)
    (cdr (get-most-advanced-pair fast-counters slow-counters)))

  (define (get-counter-with-index counters index)
    (car (filter (lambda (c) (= index (counter-index c))) counters)))

  (define (get-pair index fast-counters slow-counters)
    (cons index (car (top (counter-queue (get-counter-with-index (append fast-counters slow-counters) index))))))

  (define (get-remaining-time fast-counters slow-counters time)
    (- time (get-most-advanced-value (append fast-counters slow-counters))))
  
  (define (pass-time done-clients fast-counters slow-counters time)
    (if (< time (get-most-advanced-value fast-counters slow-counters))
        (better-serve done-clients (cdr requests) (apply-time-passing-to-counters fast-counters time) (apply-time-passing-to-counters slow-counters time))
        (pass-time
         (append done-clients (list (get-pair (get-most-advanced-index fast-counters slow-counters) fast-counters slow-counters)))
         (update remove-first-from-counter (apply-time-passing-to-counters fast-counters (get-most-advanced-value fast-counters slow-counters)) (get-most-advanced-index fast-counters slow-counters))
         (update remove-first-from-counter (apply-time-passing-to-counters slow-counters (get-most-advanced-value fast-counters slow-counters)) (get-most-advanced-index fast-counters slow-counters))
         (- time (get-most-advanced-value fast-counters slow-counters)))))

  ; functiile necesare pentru a implementa requestul close
  (define close-one-counter
    (lambda (C)
      (struct-copy counter C [closed #t])))
  
  (define (close-counter index)
    (better-serve done-clients (cdr requests) (update close-one-counter fast-counters index) (update close-one-counter slow-counters index)))
  
  (if (null? requests)
      (cons done-clients (map (lambda (c) (cons (counter-index c) (counter-queue c))) (filter (lambda (c) (not (queue-empty? (counter-queue c)))) (append fast-counters slow-counters))))
      (match (car requests)
        [(list 'close index) (close-counter index)]
        [(list 'ensure average) (ensure average slow-counters)]
        [(list name n-items) (add-person-to-queue name n-items)]
        [(list 'delay index minutes) (add-delay index minutes)]
        [x (pass-time done-clients fast-counters slow-counters x)])))