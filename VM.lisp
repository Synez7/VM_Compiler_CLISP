;; Creation et initialisation de notre machine virtuelle
(defun make-vm (&optional (nom 'maVM) (taille 10000))
	
    ; Allocation memoire de notre VM
    (setf (get nom 'memoire) (make-array taille :initial-element ()))

    ; Registres generaux 
    (setf (get nom 'R0) 0)
	(setf (get nom 'R1) 0)
	(setf (get nom 'R2) 0)

    ; Registres de pile
    (setf (get nom 'BP) 0)
	(setf (get nom 'SP) -1)
	(setf (get nom 'FP) 0)

    ; Registre concernant le compteur ordinal / compteur de programme 
    (setf (get nom 'PC) 0)

    ; Registres booleens pour les comparaisons 
    (setf (get nom 'FLT) 0)  
	(setf (get nom 'FEQ) 0)  
	(setf (get nom 'FGT) 0)  
	
    ; Table des references non resolues
	(setf (get nom 'tableReferences) (make-hash-table)) 

	

	"VM initialisée avec succès !"
)




;; Descriptif des etats de la VM ainsi initialisee
(defun mv-etats(mv)
  
	(format t "~%Machine virtuelle : ~%--- Nom : ~S ~%--- Taille mémoire : ~D" mv (get-tailleMemoire mv))
    (format t "~%- Registres généraux : ~%--- R0 : ~D ~%--- R1 : ~D ~%--- R2 : ~D" (get-registre mv 'R0) (get-registre mv 'R1) (get-registre mv 'R2))
	(format t "~%- Registres de pile : ~%--- BP : ~D ~%--- SP : ~D ~%--- FP : ~D" (get-registre mv 'BP) (get-registre mv 'SP) (get-registre mv 'FP))
	(format t "~%- Registre lié au compteur ordinal : ~%--- PC : ~D" (get-registre mv 'PC))
    (format t "~%- Drapeaux : ~%--- FLT : ~D ~%--- FEQ : ~D ~%--- FGT : ~D" (get-registre mv 'FLT) (get-registre mv 'FEQ) (get-registre mv 'FGT))
	"Fin du desctiptif des états de la VM"
  )


;; Reinitialisation de la VM
(defun mv-reset (&optional (nom 'maVM) (taille 10000))
     (setf (get nom 'PC) 0)
     (setf (get nom 'FP) 0)
     (setf (get nom 'SP) -1)
	"VM réinitialisée !"
    )


;; On assisgne la valeur T au drapeau 'lancementVM pour indiquer que la VM est executee
(defun mv-start (mv)
    (setf (get mv 'lancementVM) T))


;; (HALT) indication de l'arret de la VM en assignant la valeur NIL au drapeau 'lancementVM
(defun mv-halt(mv)
	(setf (get mv 'lancementVM) NIL))


;; Etat courant de la VM (Connaissance de la valeur assignee au drapeau 'lancementVM)
(defun mv-etatCourant(mv)
    (get mv 'lancementVM))



;; Accesseurs en lecture et en ecriture de registres
(defun get-registre (mv registre)
	(get mv registre))


(defun set-registre (mv registre valeur)
	(setf (get mv registre) valeur))
	


;; Accesseurs en lecture et ecriture pour la memoire allouee à la VM
(defun get-tailleMemoire (mv)
	(length (get mv 'memoire)))


(defun set-tailleMemoire (mv taille)
   (if (> taille 0)
	(setf (get mv 'memoire) (make-array taille))
))

(defun get-memoire (mv adresse)
	(aref (get mv 'memoire) adresse))

(defun set-memoire (mv adresse valeur)
	(setf (aref (get mv 'memoire) adresse) valeur))



;; Accesseurs en lecture et en ecriture pour les references non resolues
(defun get-referenceNR (mv reference)
	(gethash reference (get mv 'tableReferences)))


(defun set-referenceNR (mv reference adresse) 
	(setf (gethash reference (get mv 'tableReferences)) adresse))


;; Empilement d'une valeur sur la pile d'instructions
(defun mv-pushMemoire(mv valeur)
    
    (mv-instruction-un mv '1+ 'SP) ;; Incrementation du sommet de pile SP (Stack Pointer)
    (set-memoire mv (get-registre mv 'SP) valeur))




;; Chargeur de la VM qui charge un contenu d'instructions assembleur au sein de la memoire de la VM
(defun mv-loadASM (mv codeASM)
    (loop :for instruction :in codeASM :do
        (if (eql (car instruction) 'LABEL)   
			(set-referenceNR mv (cadr instruction) (+ (get mv 'SP) 1))
            (progn
                (if (null instruction)       
					(mv-instruction-un mv '1+ 'PC)) ; incrementation du compteur ordinal apres la detection d'une instruction unaire
					(mv-pushMemoire mv instruction)))))


;; Execution d'un code assembleur une fois chargé dans la mémoire de la VM
(defun mv-exec (mv)
    (mv-start mv) ; VM allumee 
    (loop :while (mv-etatCourant mv) :do
        (mv-evalInstr mv)))



;; Chargement et execution des instructions assembleur dans la mémoire de la VM
(defun mv-loadExec (mv codeASM)
    (mv-loadASM mv asm)
    (mv-exec mv))


;; Recuperation d'une instruction de VM
(defun mv-getInstr (mv)
    (let ((v (get-registre mv 'PC)))
        (mv-instruction-un mv '1+ 'PC)  ; Incrementation du compteur ordinal
        (get-memoire mv v)))  ; Recuperation de l'emplacement memoire du compteur ordinal (PC)



;; Evaluation d'une instruction courante d'assembleur
(defun mv-evalInstr (mv)
	(let ((instruction (mv-getInstr mv)))
	; Traitement de chaque instruction assembleur
	(case (car instruction)               
		
		('LOAD  (mv-load mv (cadr instruction) (caddr instruction)))
		('STORE (mv-store mv (cadr instruction) (caddr instruction)))

		('MOVE	(mv-move mv (cadr instruction) (caddr instruction)))
		; Evaluation d'une instruction binaire ( + | - | * | / )
		('ADD	(mv-instruction-bin mv '+ (cadr instruction) (caddr instruction)))
		('SUB	(mv-instruction-bin mv '- (cadr instruction) (caddr instruction)))
		('MUL	(mv-instruction-bin mv '* (cadr instruction) (caddr instruction)))
		('DIV	(mv-instruction-bin mv '/ (cadr instruction) (caddr instruction)))
		; Evaluation d'une instruction unaire ( -1 (decrementer) | +1 (incrementer))
		('INCR	(mv-instruction-un mv '1+ (cadr instruction)))
		('DECR	(mv-instruction-un mv '1- (cadr instruction)))
		; Evaluation d'une instruction de pile ( POP | PUSH )
		('PUSH	(mv-push mv (cadr instruction)))
		('POP	(mv-pop mv (cadr instruction)))
		; Saut inconditionnel
		('JMP	(mv-jmp mv (cadr instruction)))

		('JSR	(mv-jsr mv (cadr instruction)))
		('RTN	(mv-rtn mv))

		('CMP	(mv-cmp mv (cadr instruction) (caddr instruction)))
		; Saut conditionnel
		('JGT	(mv-jgt mv (cadr instruction)))
		('JGE	(mv-jge mv (cadr instruction)))
		('JLT	(mv-jlt mv (cadr instruction)))
		('JLE	(mv-jle mv (cadr instruction)))
		('JEQ	(mv-jeq mv (cadr instruction)))
		('JNE	(mv-jne mv (cadr instruction)))

		('NOP	(mv-nop mv))
		('HALT	(mv-halt mv))
		; Instructions supplementaires 
		(NOT 	(mv-instruction-un mv 'not (cadr instruction)))
		(CAR 	(mv-instruction-un mv 'car (cadr instruction)))
		(CDR 	(mv-instruction-un mv 'cdr (cadr instruction)))
		(CONS 	(mv-instruction-bin mv 'cons (cadr instruction) (caddr instruction)))

		(T (error "mv-evalInstr: Instruction non connue ~s " (car instruction)))
	)
))



;;;;;;;;;;;;;;;;;;;   Operations de la machine virtuelle a registres   ;;;;;;;;;;;;;;;;;;;   

;; (MOVE <src> <dest>) mouvement de registre a registre
(defun mv-move (mv src dest)
	(set-registre mv dest (mv-adressageMode mv src)))

;; (LOAD <src> <dest>) chargement de memoire a registre
(defun mv-load (mv src dest)
	(set-registre mv dest (get-memoire mv (mv-adressageMode mv src))))

;; (STORE <src> <dest> chargement de registre a memoire
(defun mv-store (mv src dest)
	(set-memoire mv (mv-adressageMode mv dest) (get-registre mv src)))



;; Modes d'adressage (mode direct (on charge une constante) et le mode normal)
;; Exemple : (MOVE (:CONST 50) R0), en mode direct, chargement de la constante 50 dans R0. 
;; (MOVE R0 R1) recopie le contenu de R0 dans R1
(defun mv-adressageMode (mv couple)
    (case (car couple)  ; Couple (Mode,Registre)
        (:CONST (cdr couple))
        (:DIRECT (get-registre mv (cdr couple)))
        (:INDEX (+ (cadr couple) (get-registre mv (cddr couple))))
        (:LABEL (get-referenceNR mv (cdr couple)))
        (T (error "Mode d'adressage incorrect : ~a" (car couple)))))



;; Operations arithmetiques unaires et binaires
;; Operations unaires : {INCR/DECR/NOT/CAR/CDR} | Operations binaires : {ADD/SUB/MUL/DIV/CONS}
;; operateur: fonction d'arité 1 qui va être ainsi executee avec l'appel à la primitive funcall
(defun mv-instruction-un (mv operateur dest)
    (set-registre mv dest (funcall operateur (get-registre mv dest))))


;; operateur: fonction d'arité 2 qui va être ainsi executee avec l'appel à la primitive funcall
(defun mv-instruction-bin (mv operateur src dest)
    (set-registre mv dest (funcall operateur (get-registre mv dest) 
								(mv-adressageMode mv src))))


;; (PUSH <src>) pousse sur la pile le contenu de <src>
(defun mv-push(mv src)
    (mv-instruction-un mv '1+ 'SP) ; Operation unaire d'incrementation du SP
    (mv-store mv src (cons :DIRECT 'SP))) ; Mode d'adressage direct
  

;; (POP <dest>) depile le sommet de pile, et met l information dans <dest>
(defun mv-pop(mv dest)
	(mv-load mv (cons :DIRECT 'SP) dest)
    (mv-instruction-un mv '1- 'SP)) ; Operation unaire de decrementation du SP



;; (JMP <label>) saut inconditionnel a une etiquette
(defun mv-jmp (mv label)
	(set-registre mv 'PC (mv-adressageMode mv label)))


;; (JSR <label>) saut avec retour
(defun mv-jsr (mv label)
	(mv-push mv 'PC)
    (mv-jmp mv (cons :CONST (mv-adressageMode mv label))))

;; (RTN) retour
(defun mv-rtn (mv)
	(mv-pop mv 'R1)
    (mv-jmp mv (cons :DIRECT 'R1)))


;; (CMP <src1> <src2>) comparaison
(defun mv-cmp (mv src1 src2)
	
	; Initialisation des drapeaux booléens à 0
	(set-registre mv 'FLT 0)
    (set-registre mv 'FEQ 0)
	(set-registre mv 'FGT 0)

	; Stockage de contenu des registres respectifs (src1 et src2)
	(let ((valeurSrc1 (get-registre mv src1))
          (valeurSrc2 (get-registre mv src2)))

		(if (symbolp valeurSrc2)  ; Test si le type est un symbole

	       (if (equal valeurSrc1 valeurSrc2)
			   (set-registre mv 'FEQ 1)  ; Contenu identique de registres (Assignation de 1 au drapeau 'FEQ)
			   nil)
		(if (> valeurSrc1 valeurSrc2) 
			(set-registre mv 'FGT 1) ; Contenu du registre src1 >  Contenu du registre src2 (Assignation de 1 au drapeau 'FGT)
			(set-registre mv 'FLT 1) ; (Assignation de 1 au drapeau 'FLT) sinon
		)
	)
))

;; (JGT <label>) saut si plus grand
(defun mv-jgt (mv label)
	(if (= (get-registre mv 'FGT) 1)
		(mv-jmp mv label)
	)
)

;; (JGE <label>) saut si plus grand ou egal
(defun mv-jge (mv label)
	(if (or (= (get-registre mv 'FEQ) 1) (= (get-registre mv 'FGT) 1))
		(mv-jmp mv label)
	)
)

; (JLT <label>) saut si plus petit
(defun mv-jlt (mv label)
	(if (= (get-registre mv 'FLT) 1)
		(mv-jmp mv label)
	)
)

; (JLE <label>) saut si plus petit ou egal
(defun mv-jle (mv label)
	(if (or (= (get-registre mv 'FLT) 1) (= (get-registre mv 'FEQ) 1))
		(mv-jmp mv label)
	)
)

; (JEQ <label> saut si egal
(defun mv-jeq (mv label)
	(if (= (get-registre mv 'FEQ) 1)
		(mv-jmp mv label)
	)
)

; (JNE <label> saut si different
(defun mv-jne (mv label)
	(if (or (= (get-registre mv 'FLT) 1) (= (get-registre mv 'FGT) 1))
		(mv-jmp mv label)
	)
)

; (NOP) rien
(defun mv-nop (mv))




(setf traceTests T)
(setf mv-memoire NIL)   

(defun file-exec(file)
    (make-vm 'maVM)
    (let (asm mv-res eval-res)
        (format traceTests "% programme = ~a~%" file)
        ;; compilation du programme
        (format traceTests "=== compilation...~%")
        (setf asm (compilation-prgm-fichier file))
        (format traceTests "% instructions = ~a~%" asm)
        ;; chargement des instructions dans la machine virtuelle
        (format traceTests "=== chargement...~%")
        (mv-loadASM 'maVM asm)
        (format mv-memoire "mémoire = ~a~%" (get 'maVM 'memoire))
        ;; exécution de la machine virtuelle
        (format traceTests "=== exécution...~%")
        (mv-exec 'maVM)
        ;; récupération du résultat obtenu
        (setf mv-res (get-registre 'maVM 'R0))
        (format traceTests "% résultat = ~a~%~%" mv-res)))




