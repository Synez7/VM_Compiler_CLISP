;; Fonctions prinicpales pour la compilation d'un programme en Lisp
(defun compilation-prgm(prgm)
    (setf label -1)
    (setf listeGlobals NIL)
    (setf fonctions NIL)
    ; On compile le programme avec un environnement vide au depart
    (let ((codeASM (map-analyseCas prgm '(NIL))))
        (append
            (compilation-globalsVar)
            codeAsm
            (list '(HALT)))))



;; Compilation d'un programme Lisp a partir d'un chemin d'acces vers un fichier contenant le programme
(defun compilation-prgm-fichier(fichier)
    (compilation-prgm
        (with-open-file (fstream fichier)
            (loop :for forme := (read fstream nil)
                :while forme
                :collect forme))))


;; Possibilité de lire une trace de la compilation d'un prgm LISP (NIL : Non | T : Oui)
(setf affichageCompilation NIL)



;; Creation des etiquettes avec auto-incrementation
(defun creationLabel()
    (setf label (1+ label)))



;; Gestion des variables globales 
;; ajout de variables globales, association entre var globale et label via un couple (listeGlobals)
(defun addGlobalVar(variable)
    (affichageCas "Nouvelle variable globale" variable)
    (setf listeGlobals
        ; Lien d'une variable a une nouvelle etiquette
        (cons (cons variable (creationLabel))
              listeGlobals)))


;; Obtention d'une etiquette pour une variable
(defun getGlobalVar(variable)
    (cdr (assoc variable listeGlobals)))



;; Code assembleur produit a partir de variables globales lues, chacune ayant un label
(defun compilation-globalsVar()
    (loop :for g :in listeGlobals :nconc
        (list (list 'LABEL (cdr g))
              'NIL)))





;; Gestion des variables locales
(defun addLocalVar (locales env)
    (let ((nbreLocales (length locales)))
        (cons
            ; Nouvel environnement, qui est une liste
            ; de couples(nomVariable,offset par rapport au FP)
            (loop :for l :in locales :and offset :from 0 :collect
                (cons l (- offset nbreLocales)))
            env)))




(defun find-localVar (var env)
    (let (index profondeur)
        ;; recherche de la variable dans ENVS
        (setf profondeur
            (loop :for e :in env :and i :from 0 :do
                (if (setf index (assoc var e))
                    (return i))))
        (if index
            (cons
                (if (> profondeur 0)
                    ; Descente au sein de la pile niveau frame
                    (append
                        (list '(MOVE (:direct . FP) R1))
                        (loop :repeat profondeur :collect
                            '(LOAD (:index 1 . R1) R1))))
                (cdr index)))))




;; Gestion des fonctions
;; fct: nom de fonction
;; nbParam: nombre de paramètres de la fonction fct
(defun addFonction (fct nbParam)
    (format affichageCompilation "Nouvelle fonction: ~a(~a)~%" fct nbParam)
    (let (d)
        (setf fonctions
            (cons
                (cons
                    (cons fct
                        (cons nbParam
                            (if (setf d (assoc fct (car fonctions)))
                                (cddr d)
                                (creationLabel))))
                    (car fonctions))
                (cdr fonctions)))))




;; fct: fonction a trouver dans l'environnement
(defun findFonction(fct)
    (let (r)
    ;Parcours sur l'environnement
        (loop :for env :in fonctions :do
            (if (setf r (assoc fct env))
                (return (cdr r))))))




(defun pushFonctions()
    (setf fonctions (cons NIL fonctions)))



(defun popFonctions()
    (setf fonctions (cdr fonctions)))





;; Analyse des cas
(defun affichageCas(forme valeur)
    (format affichageCompilation "~a: ~a~%" forme valeur))



;;; EXPR est une liste de formes
;;; fonction aussi utilisée pour les progn implicites
(defun map-analyseCas(expr env)
    (if (null expr)
        ;; une S-expression vide vaut nil
        (list '(MOVE (:CONST . NIL) R0))
        ;; analyse de cas de chaque forme
        (loop :for forme :in expr :nconc
            (analyseCas forme env))))



;; Analyse de cas suivant la forme de l'expression expr
(defun analyseCas(expr env)
    (if (atom expr)
        (if (constantp expr)
          
            (compilation-constante expr)
        
            (compilation-variable expr env))
       
        (if (consp (car expr))
            (if (eql 'lambda (caar expr))
             
                (compilation-lambdaExpr expr env)
                (error "Expression incorrecte syntaxiquement : ~a" expr))
            (if (not (symbolp (car expr)))
                (error "~a n'est pas un symbole" (car expr))
                (if (not (fboundp (car expr)))
                    ; expr: appel de fonction utilisateur
                    (compilation-caller expr env)
                    (if (macro-function (car expr))
                        (compilation-macro expr env)
                        (if (special-form-p (car expr))
                            ; expr: forme syntaxique du langage
                            (compilation-forme expr env)
                            ; expr: appel de fonction
                            (compilation-primitive-call expr env))))))))



;; Compilation dans le cas où une expression est une macro
(defun compilation-macro(expr env)
    (case (car expr)
        (lambda (compilation-lambda (cdr expr) env))
        (defun (compilation-defun (cdr expr) env))

        (t (let ((forme (macroexpand-1 expr)))
            (format affichageCompilation "Macro: ~a => ~a~%" expr forme)
            (analyseCas forme env)))))


;; Compilation dans le cas où une expression a une forme syntaxique particuliere 
(defun compilation-forme(expr env)
    (case (car expr)
        (if       (compilation-if       (cdr expr)  env))
        (let      (compilation-let      (cdr expr)  env))
        (let*     (compilation-let      (cdr expr)  env))
        (progn    (compilation-progn    (cdr expr)  env))
        (quote    (compilation-quote    (cadr expr) env))
        (setq     (compilation-setq     (cdr expr)  env))
        (function (compilation-fonction (cadr expr) env))
        (labels   (compilation-label   (cdr expr)  env))
        
        (T (error "Forme non trouvée : ~a" (car expr)))))




;; Compilation dans le cas d'un appel de fonction primitive
(defun compilation-primitive-call(expr env)
    (affichageCas "Appel d'une fonction primitive" expr)
    (case (car expr)
        ; Operations d'arithmetique
        (1+ (compilation-un 'INCR (cadr expr) env))
        (1- (compilation-un 'DECR (cadr expr) env))
        (+  (compilation-bin 'ADD  (caddr expr) (cadr expr) env))
        (*  (compilation-bin 'MUL (caddr expr) (cadr expr) env))
        (-  (compilation-bin 'SUB  (caddr expr) (cadr expr) env))
        (/  (compilation-bin 'DIV  (caddr expr) (cadr expr) env))
        ; Operation logique
        (not (compilation-un 'NOT (cadr expr) env))
        ; Operation sur listes
        (car  (compilation-un  'CAR  (cadr expr) env))
        (cdr  (compilation-un  'CDR  (cadr expr) env))
        (cons (compilation-bin 'CONS (caddr expr) (cadr expr) env))
        ; Operations de comparaison
        (=  (compilation-comp 'JEQ (cadr expr) (caddr expr) env))
        (/= (compilation-comp 'JNE (cadr expr) (caddr expr) env))
        (<  (compilation-comp 'JLT  (cadr expr) (caddr expr) env))
        (<= (compilation-comp 'JLE (cadr expr) (caddr expr) env))
        (>  (compilation-comp 'JGT  (cadr expr) (caddr expr) env))
        (>= (compilation-comp 'JGE (cadr expr) (caddr expr) env))
       
        (eq    (compilation-comp 'JEQ (cadr expr) (caddr expr) env))
        (eql   (compilation-comp 'JEQ (cadr expr) (caddr expr) env))
        (equal (compilation-comp 'JEQ (cadr expr) (caddr expr) env))
        (funcall (compilation-funcall (cdr expr) env))
        (T (compilation-caller expr env))))




;; Gestion du cadre de pile
;; args: code a compiler sous format de liste
;; corpsASM: instructions d'un nouveau frame 
;; Empilement d'arguments > Creation d'un nouveau frame > Corps du frame > Frame effacé
(defun compilation-framePointer(args corpsASM env)
    (append
        (loop :for a :in args :and nbreLocales :from 0 :nconc
        (append (analyseCas a env)
                    (list '(PUSH R0)))
            :into localesASM
            :finally
                (return
                    (append localesASM
                        (list (list 'MOVE (cons :CONST nbreLocales) 'R1)))))
        '((PUSH R1)
        ;Creation d'un nouveau frame
          (MOVE (:DIRECT . FP) R1)     ; R1 = FP
          (MOVE (:DIRECT . SP) FP)     ; FP = SP
          (PUSH R1))                 
        corpsASM
        '((POP FP)                     
          (POP R1)                     
          (SUB (:DIRECT . R1) SP))     
        NIL))


;; Compilation dans le cas où l'expression est une constante
(defun compilation-constante (expr)
    (affichageCas "Constante" expr)
    (list (list 'MOVE (cons :CONST expr) 'R0)))



;; Compilation dans le cas où l'expression est une variable soit locale soit globale
(defun compilation-variable (expr env)
    (affichageCas "Variable" expr)
    (let (varInd)
        (if (setf varInd (assoc expr listeGlobals))
            (list (list 'LOAD (cons :LABEL (cdr varInd)) 'R0))
            (if (setf varInd (find-localVar expr env))
                (if (null (car varInd))
                    (list (list 'LOAD
                                (cons :INDEX (cons (cdr varInd) 'FP))
                                'R0))
                 
                    (append
                        (car varInd)  
                        (list (list 'LOAD
                                    (cons :INDEX (cons (cdr varInd) 'R1))
                                    'R0))))
                (error "Variable incorrecte : ~a" expr)))))


;; Compilation dans le cas d'une lambda expression
(defun compilation-lambdaExpr (expr env)
    (affichageCas "Une lambda-expression" expr)
    (let ((parametres (cadar expr))
          (corps (cddar expr))
          (args (cdr expr)))
        ;; le nombre d'arguments doit être égal au nombre de paramètres
        (if (/= (length parametres) (length args))
            (error "Nombre de paramètres invalide pour la lambda-expression"))
        ;; compilation des arguments et création d'un cadre de pile
        (compilation-framePointer
            args
            (map-analyseCas corps (addLocalVar parametres env))
            env)))



;; Compilation dans le cas où l'expression correspond au corps d'une fonction
(defun compilation-corpsFonction (expr fonctionLabel env)
    (let ((end (- label (length listeGlobals)))
          corpsASM)
        (setf corpsASM (map-analyseCas expr env))
        (setf end (+ (length corpsASM) end (- (length listeGlobals) label) 1))
        (append
           (list (list 'MOVE (cons :LABEL fonctionLabel) 'R0)
                  (list 'JMP  (cons :INDEX (cons end 'PC)))
                  (list 'LABEL fonctionLabel))
            corpsASM
            (list '(RTN)))))



;; Compilation dans le cas où l'expression est une fonction lambda
(defun compilation-lambda (expr env)
    (affichageCas "Une fonction quelconque" expr)
    (let ((parametres (car expr))
          (corps (cdr expr)))
        (compilation-corpsFonction
            corps                       ; corps de la lambda
            (creationLabel)               
            (addLocalVar parametres env)))) 


;; Compilation dans le cas où l'expression est une définition de fonction
(defun compilation-defun (expr env)
    (affichageCas "Defun" expr)
    (let ((fct (car expr))
          (parametres (cadr expr))
          (corps (cddr expr)))
        (if (or (< (length expr) 2) (not (listp parametres)))
            (error "DEFUN : définition de fonction incorrecte : ~a" expr))
        (addFonction fct (length parametres))
        (compilation-corpsFonction
            corps
            (cdr (findFonction fct))  
            (addLocalVar parametres env)))) 
	



;; Compilation dans le cas où l'expression est un appel de fonction indiquee par l'utilisateur
(defun compilation-caller (expr env)
    (affichageCas "Appel de fonction de la part du caller" expr)
    (let ((fct (findFonction (car expr))))
        (if (null fct)
            (error "Fonction inconnue : ~a" (car expr)))
        (if (/= (length (cdr expr)) (car fct))
            (error "Fonction ~a : ~a arguments donnés, ~a attendus"
                (car expr) (length (cdr expr)) (car fct)))
        (compilation-framePointer
            (cdr expr)
            (list (list 'JSR (cons :LABEL (cdr fct))))
            env)))



;; Compilation dans le cas où l'expression est un appel de fonction explicite
(defun compilation-funcall (expr env)
    (affichageCas "Appel fonction explicite" expr)
    (let ((fct (car expr))
          (args (cdr expr)))
        (append
            (analyseCas fct env)
            (list '(MOVE (:DIRECT . R0) R2))
            (compilation-framePointer
                args
                (list '(JSR (:DIRECT . R2)))
                env))))





;; Compilation  dans le cas où l'expression est une expr. unaire
(defun compilation-un (operateur expr env)
    (append
        (analyseCas expr env)
        (list (list operateur 'R0))))


;; Compilation  dans le cas où l'expression est une expr. binaire
(defun compilation-bin (operateur left right env)
    (append
        (analyseCas left env)
        (list '(PUSH R0))
        (analyseCas right env)
        (list '(POP R1)
              (list operateur '(:DIRECT . R1) 'R0))))



;; Compilation par cas de comparaison
(defun compilation-comp (operateur left right env)
    (append
        (analyseCas left env)
        (list '(PUSH R0))
        (analyseCas right env)
      
        (list '(POP R1)
              '(CMP R1 R0)
              '(MOVE (:CONST . t) R0)
              (list operateur '(:INDEX 1 . PC))
              '(MOVE (:CONST . nil) R0))))


;; Compilation d'une expression de type instruction conditionnelle "if"
(defun compilation-if (expr env) 
    (affichageCas "Instruction conditionnelle if" expr)
    (let ((else (creationLabel))
          (endif (creationLabel)))
        (append
         
            (analyseCas (car expr) env)
            (list '(MOVE (:CONST . nil) R1)
                  '(CMP R0 R1))
            (list (list 'JEQ (cons :LABEL else)))
            ; Traitement du then
            (analyseCas (cadr expr) env)
            (list (list 'JMP (cons :LABEL endif))
                  (list 'LABEL else))
            ; Traitement du else
            (analyseCas (caddr expr) env)
            (list (list 'LABEL endif)))))


;; Compilation d'une expression de type "let"
(defun compilation-let (expr env)
    (affichageCas "Let" expr)
    (loop :for var :in (car expr)
        :if (consp var)
            :collect (car var) :into vars
            :and :collect (cadr var) :into args
        :else
            :collect var :into vars
            :and :collect nil :into args
        :finally
            (return
                (compilation-framePointer
                    args
                    (map-analyseCas (cdr expr) (addLocalVar vars env))
                    env))))



;; Compilation d'une expression de type "progn"
(defun compilation-progn (expr env)
    (affichageCas "Progn" expr)
    (map-analyseCas expr env))


;; Expression en quote a compiler
(defun compilation-quote (expr env)
    (affichageCas "Quote" expr)
    (list (list 'MOVE (cons :CONST expr) 'R0)))



;; Compilation d'une expression etant une affectation d'une valeur à une variable
(defun compilation-setq (expr env)
    (affichageCas "Setq" expr)
    (let ((var (car expr))
          (body (cadr expr))
          var-pos)
        (append
            (analyseCas body env)  
            (if (setf var-pos (find-localVar var env))
               
                (if (null (car var-pos))
                    (list (list 'STORE
                                'R0
                                (cons :INDEX (cons (cdr var-pos) 'FP))))
                   
                    (append
                        (car var-pos)  
                        (list (list 'STORE
                                    'R0
                                    (cons :INDEX (cons (cdr var-pos) 'R1))))))
                ; VAR est une variable globale
                (progn
                    (if (not (assoc var listeGlobals))
                        ;; VAR est une nouvelle variable
                        (addGlobalVar var))
                    (list (list 'STORE 'R0 (cons :LABEL (getGlobalVar var)))))))))



;; Compilation d'une expression de type "fonction"
(defun compilation-fonction (expr env)
    (affichageCas "Fonction" expr)
    (let (fonctionLabel)
        (if (atom expr)
            (setf fonctionLabel (cdr (findFonction expr)))
            ;; EXPR est une lambda
            (if (eql 'lambda (car expr))
                (setf fonctionLabel (creationLabel))))
        (if (not fonctionLabel)
            (error "FONCTION: ~a n'est pas une fonction" expr))
        ;; chargement de l'étiquette dans R0
        (list (list 'MOVE (cons :LABEL fonctionLabel) 'R0))))



;; Compilation d'une expression type "label"
(defun compilation-label (expr env)
    (affichageCas "Label" expr)
    (let ((decls (car expr))
          (body (cdr expr))
          asm)
        (pushFonctions)
        (loop :for decl :in decls :do
            (addFonction (car decl) (length (cadr decl))))
        (setf asm
            (loop :for decl :in decls :nconc
                (compilation-defun decl env)))
        (setf asm
            (append asm
                (map-analyseCas body env)))
        (popFonctions)
        asm))
