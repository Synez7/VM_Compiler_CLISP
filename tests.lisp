(load "Compiler.lisp")
(load "VM.lisp")



(setf traceTests T)
(setf affichageCompilation NIL)


(setf mv-instructions NIL)           
(setf mv-registres NIL)            
(setf mv-memoire NIL)              





(setf testPrgm
(list
   
    ;; CONSTANTES
    (cons 'constante1 '(1))
    (cons 'constante2 '(2))
    (cons 'constante3 '(3))

    ;; EXPRESSIONS ARITHMETIQUES
    (cons 'exprArith1 '((+ 3 5)))
    (cons 'exprArith2 '((+ 1 (+ 1 (- (* (- 2 1) 3) (/ 8 2))))))
    (cons 'exprArith3 '((+ 5 (- 2 (* (- 2 1) 3)))))

    ;; INSTRUCTIONS IF
    (cons 'if1 '((if T 0)))
    (cons 'if2 '((if (< 1 9) (if NIL T (if T 1 2)))))
    (cons 'if3 '((when T 1 2)))
    (cons 'if4 '((setf a NIL) (cond (a 2) ((not a) 3) ('b 4) (t 1))))
    (cons 'if5 '((setf a T) (case a (a 2) ((T) 4) (t 3))))

    ;; EXPRESSION LOGIQUE
    (cons 'exprLogique '((not T)))

    ;; QUOTE
    (cons 'quote1 '('0))
    (cons 'quote2 '('((a b) 1 NIL (func T))))

    ;; LISTES
    (cons 'liste1 '((cons '(1 2 3) '(4 5 6))))
    (cons 'liste2 '((car '(2 3 4))))
    (cons 'liste3 '((cdr '(2 3 4))))

    ;; AFFECTATIONS
    (cons 'setf1 '((setf a 0)))
    (cons 'setf2 '((setf x 0) (setf y 2) (setf x (+ x 1))))
    (cons 'setf3 '((setf b (setf c 2))))

    (cons 'let1 '((let () 0)))
    (cons 'let2 '((let ((a 0) b) (if b a 1))))
    (cons 'let3 '((let ((u 3) (_ 99) (v 1)) (+ (- u _) (- _ v)))))
    (cons 'let4 '((let ((x (not NIL)) y (z (+ 2 1))) (setf y (1- z)) (if (not x) y z))))
    (cons 'let5 '((let ((a 1)) (let ((i 2) j) (let ((k (+ a i))) (setf j (- k a)))))))

 

    ;; FONCTIONS
    (cons 'lambda1 '(((lambda ()))))
    (cons 'lambda2 '(((lambda (id) id) 1)))
    (cons 'lambda3 '(((lambda (x) (setf x (+ x 1)) (- x 1)) 2)))

    (cons 'defun1 '((defun f ())))
    (cons 'defun2 '((defun const () 1) (const)))
    (cons 'defun3 '((defun id (x) x) (id 2)))
    (cons 'defun4 '((defun fa (k) (+ k 1)) (defun fb () (fa 2)) (fb)))
    (cons 'defun5 '((defun sum (i) (if (<= i 0) 0 (+ i (sum (- i 1))))) (* (sum 2) (sum 2))))

    (cons 'fonction1 '((let ((f (lambda ()))) (funcall f))))
    (cons 'fonction2 '((defun id (x) x) (id 1)))

    ;; FONCTIONS RECURSIVES
    (cons 'fact '((defun fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 10)))
    (cons 'recursifTest '((defun rec(n) (if (<= n 0) 0 (rec (- n 1)))) (rec 10)))
    (cons 'puissance '((defun puissance(x n) (if (<= n 0) 1 (* x (puissance x (- n 1))))) (puissance 2 8)))
    (cons 'fibo '((defun fibo (n) (if (<= n 1) n (+ (fibo (- n 1)) (fibo (- n 2))))) (fibo 10)))


    ;; LABELS
    (cons 'label1 '((labels ((const () (setf const 0))) (const))))
    (cons 'label2 '((labels ((pair (n) (if (<= n 0) T (impair (- n 1)))) (impair (n) (if (<= n 0) NIL (pair (- n 1))))) (pair 4))))
    (cons 'label3 '((defun sum-list (l) (labels ((acc (l res) (if l (acc (cdr l) (+ (car l) res)) res))) (acc l 0))) (sum-list '(1 5 10))))

    NIL))



;; Lancement de tests sur la VM a partir d'une liste de programmes
(defun test-vm (mv prgm)
    (let (asm mv-resultat eval-resultat)
        (format traceTests "% Programme = ~a~%" prgm)
        ;; compilation du programme
        (format traceTests "=== Compilation en cours...~%")
        (setf asm (compilation-prgm prgm))
        (format traceTests "% Instructions = ~a~%" asm)
        ;; chargement des instructions dans la machine virtuelle
        (format traceTests "=== Chargement du code de la VM...~%")
        (mv-loadASM mv asm)
        (format mv-memoire "Mémoire = ~a~%" (get mv 'memoire))
        ;; exécution de la machine virtuelle
        (format traceTests "=== Execution du code chargé dans la VM...~%")
        (mv-exec mv)
        ;; récupération du résultat obtenu
        (setf mv-resultat (get-registre mv 'R0))
        (format traceTests "% Résultat = ~a~%~%" mv-resultat)
        ;; évaluation du programme par lisp directement
        (setf eval-resultat (eval (cons 'progn prgm)))
        ;; comparaison des résultats :
        ;; la seule différence autorisée est lors de l'évaluation
        ;; d'un nom de fonction, où lisp retourne son nom
        ;; tandis que la compilation le remplace par son adresse
        ;; on vérifie alors qu'elle est bien présente dans
        ;; la table de références de la machine virtuelle
        (if (and (not (equal mv-resultat eval-resultat))
                 (not (fboundp eval-resultat))
                 (not (get-referenceNR mv mv-resultat)))
            (error "résultat trouvé = ~a ; attendu = ~a" mv-resultat eval-resultat))))


;;; teste séquentiellement tous les programmes de TESTPROGS
(defun test-all (prgm)
    (let ((debug-flags (or mv-instructions mv-registres mv-memoire)))
        (make-vm 'mv)
        (loop :for test :in prgm :while test :do
            (format traceTests "====================~%%%% Jeu de test: ~a~%" (car test))
            (test-vm 'mv (cdr test))
            ;; réinitialisation
            (if debug-flags
                (make-vm 'mv)
                (mv-reset 'mv)))))



(test-all testPrgm)
