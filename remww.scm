(define remove-duplicates
         (lambda (l)
           (cond ((null? l)
                  '())
                 ((member (car l) (cdr l))
                  (remove-duplicates (cdr l)))
                 (else
                  (cons (car l) (remove-duplicates (cdr l)))))))

(define remove-from-alive
  (lambda (lst dead)
    (if (null? dead) lst
        (if (member (car dead) lst)
            (remove-from-alive (remove (car dead) lst) (cdr dead))
            (remove-from-alive lst (cdr dead))))))
           
       

(define opti
  (lambda (start end dead alive)

    (if (null? start) end
        (let (
              (line (car start))
              (read (cadr (car start)))
              (write (caddr (car start))))
          
          (if (andmap (lambda (number) (member number dead)) write)
              (if (not (ormap (lambda (number) (member number alive)) write ))
                  (opti (cdr start) end dead alive)
                  (opti (cdr start) (cons line end) (remove-from-alive (remove-duplicates (append! dead write)) read) (remove-from-alive (remove-duplicates (append! alive read)) read) )
                         
                    
                  )
              (opti (cdr start) (cons line end) (remove-from-alive (remove-duplicates (append! dead write)) read) (remove-from-alive (remove-duplicates (append! alive read)) read) ))               
          ))))

;;(opti (cdr start) (cons end (car start)) (cons alive (cadar start)) (cons alive (caddar start))))
        
        
(define remww
  (lambda (lst)
      (opti (reverse lst) (list) (list) (list))
      ))

  (define x '((g46494 (5 1) (6)) (g46494 (6 1 2) (6)) (g46494 (1 1) (2)) (g46494 (1 1) (6)) (g46494 (6 2) (2))))
  (define y '((g46494 (6 1) (6)) (g46494 (6 1) (6)) (g46494 (6 1) (6)) (g46494 (6 1) (6)) (g46494 (6 1) (6)) (g46494 (7) (6))))
  (define x1 '((inst1 () (1)) (inst2 () (1))))
