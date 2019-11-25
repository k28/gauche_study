;
;
;

(use util.match)

(define (make-player . args)
  (define (loop lis)
    (match lis
        [() '()]
        [(attr value . rest) (cons (cons attr value) (loop rest))]))
  (loop args))

(define *player*
    (make-player 'hp 320 'mp 66 'position #f 'inventory '(potion potion dagger cookie dagger)))


(define (get-player-attr player attr)
  (cdr (assoc attr player)))

(define (update-player-attr! player attr updater)
  (let ((p (assoc attr player)))
    (set! (cdr p) (updater (cdr p)))))

(define (get-inventory player)
  (get-player-attr player 'inventory))

(define (has-item? player item)
  (member item (get-inventory player)))

(define (delete-item! player item)
  (update-player-attr! player 'inventory (cut delete item <>)))

(define (add-item! item)
  (update-player-attr! player 'inventory (cut cons item <>)))

(define (get-hp player) (get-player-attr player 'hp))

(define (add-hp! player n) (update-player-attr! player 'hp (cut + n <>)))

(define (get-mp player) (get-player-attr player 'mp))

(define (add-mp! player n) (update-player-attr! player 'mp (cut + n <>)))

(define (get-position player) (get-player-attr player 'position))

(define (set-position! player pos)
  (update-player-attr! player 'position (lambda (_) pos)))

(define *item-database*
  `((potion  (drink . ,(cut add-hp! <> 50))
             (throw . ,(cut add-hp! <> -3)))
    (elixir  (drink . ,(cut add-mp! <> 50))
             (throw . ,(cut add-mp! <> -3)))
    (pancake (eat   . ,(cut add-hp! <> 30))
             (throw . ,(cut add-hp! <> -2)))
    (cookie  (eat   . ,(lambda (player)
                         (sys-system "fortune -s")
                         (add-hp! player 7)))
             (throw . ,(cut add-hp! <> -1)))
    (dagger  (throw . ,(lambda (_) #f)))))

(define (item-propertys item)
  (cond [(assoc item *item-database*) => cdr]
        [else '()]))

(define (item-property-get item prop)
  (cond [(assoc prop (item-propertys item)) => cdr]
        [else #f]))

(define (use-item! what item)
  (cond [(not (has-item? *player* item))
         (print item "を持っていません")]
        [(item-property-get item what)
        => (lambda (action)
             (delete-item! *player* item)
             (action *player*))]
        [else (print item "を" what "することはできません")])
  #f)

(define *dungen*
  '(["あなたは森の北側にいる。道は南に続いている"
     (s . 1)]
    ["あなたは鬱蒼とした森の中にいる。道は南北に伸びている。東に降りてゆく小道がある"
     (n . 0)
     (s . 2)
     (e . 3)]
    ["足元がぬかるんでいる。道は直角に折れ、北と西に伸びている。西に続く道の先が明るくなっている"
     (n . 1)
     (w . 4)]
    ["あなたは沼のほとりにいる。空気の動きが止まり、暑さを感じる。西に登ってゆく小道がある"
     (w . 1)]
    ["突然目の前が開けた、あなたは森の中の広場にいる。丈の短い柔らかそうな草が一面に茂っている。道が東に伸びている"
     (e . 2)]))

(define (describe)
  (print (car (get-position *player*)))
  #t)

(define (move! direction)
  (let ((position (get-position *player*)))
    (cond [(assoc direction (cdr position))
           => (lambda (p)
                (set-position! *player* (list-ref *dungen* (cdr p)))
                (describe))]
          [else
            (print "そちらには移動できません")]))
  #t)

(define (status)
  (print "hp :" (get-hp *player*))
  (print "mp :" (get-mp *player*))
  (print "inventory :" (get-inventory *player*))
  #t)

(define (reset!)
  (set! *player*
    (make-player 'hp 320 'mp 66 'position (car *dungen*)
                 'inventory '(potion, potion, dagger, cookie, dagger)))
  #t)

; (define (has-item? player item)
;   (member item (cdr (assoc 'inventory player))))
; 
; (define (add-item! player item)
;   (let ((p (assoc 'inventory player)))
;     (push! (cdr p) item)))
; 
; (define (delete-item! player item)
;   (let ((p (assoc 'inventory player)))
;     (set (cdr p) (delete-1 item (cdr p)))))

(define (main args)
    (print "Hello, World")
    ;(add-hp! *player* 5)
    (print "player hp " (get-hp *player*))
    (use-item! 'eat 'cookie)
    (print "player hp " (get-hp *player*))
    (use-item! 'eat 'cookie)
    ;(print (cdr (assoc 'inventory *player*)))
    ;(print *player*)
    0)

