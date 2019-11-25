;;;
;;; プログラミングGauche P61練習問題
;;; 手続きを返す手続き
;;;

;; filter をcondを使って定義
(define (filter pred lis)
  (cond ((null? lis) ())
        ((pred (car lis)) (cons (car lis) (filter pred (cdr lis))))
        (else (filter pred (cdr lis)))))

;; pairの場合はcar, cdr両方にfilter-for-treeを適応する
(define (filter-for-tree pred lis)
  (cond ((null? lis) ())
        ((pair? (car lis)) (cons (filter-for-tree pred (car lis)) (filter-for-tree pred (cdr lis))))
        ((pred  (car lis)) (cons (car lis) (filter-for-tree pred (cdr lis))))
        (else (filter-for-tree pred (cdr lis)))))

;; treeに対してfilterを適応するfilter-for-tree これでも動くけど, filter単体でうまく動作しない
;(define (filter-for-tree pred tree)
;  (filter (lambda (x) (or (pred x) (pair? x))) tree))

;; 下の物に変更してもOK
;(define (filter-for-tree pred lis)
;  (cond ((null? lis) lis)
;        ((pair? (car lis)) (cons (filter-for-tree pred (car lis)) (filter-for-tree pred (cdr lis))))
;        ((pred (car lis)) (cons (car lis) (filter-for-tree pred (cdr lis))))
;        (else (filter-for-tree pred (cdr lis)))))


;; filter-for-treeを使って数値のみに適応するnumbers-only-tree-walk
;; (numbers-onlyのfilterをfilter-for-treeに変更しただけ)
(define (numbers-only-tree-walk walker)
  (lambda (proc lis)
    (walker proc (filter-for-tree number? lis))))

(define (tree-walk walker proc tree)
  (walker (lambda (elt)
            (if (list? elt)
                (tree-walk walker proc elt)
                (proc elt)))
          tree))

(define (main args)
    (print (filter-for-tree number?
                      '((#f #t (#f #f)) (1 2 3 #f) 4 5 #t (#f 6 (7 8)))))
                            
    (print (tree-walk (numbers-only-tree-walk map) (lambda (x) (* x 1))
                      ;'(() 1 2 3 #f 4 5 6 7 8 9 #t)))
                      ;'((#f #t) (1 2 3 #f) 4 5 #t (#f 6 (7 8)))))
                      '((#f #t (#f #f)) (1 2 3 #f) 4 5 #t (#f 6 (7 8)))))
    0)


gosh>((()) (1 2 3) 4 5 (6 (7 8)))
gosh>(() (1 2 3) 4 5 (6 (7 8)))
