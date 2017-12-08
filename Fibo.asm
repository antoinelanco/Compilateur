# Programme principal
	.text
	# Création d'un objet Fib, qu'on stockera dans le registre $s0
	# Allocation d'un objet Fib
	li $a0, 8
	li $v0, 9
	syscall
	move $s0, $v0
	# Init premier champ : pointeur vers le descripteur de classe
	la $t0, Fib_descr
	sw $t0, 0($s0)
	# Ensuite on appelle le constructeur
	move $a0, $s0
	jal Fib_new
	
	# Création d'un objet FibMem, qu'on stockera dans le registre $s1
	# Allocation d'un objet FibMem
	li $a0, 12
	li $v0, 9
	syscall
	move $s1, $v0
	# Init premier champ : pointeur vers le descripteur de classe
	la $t0, FibMem_descr
	sw $t0, 0($s1)
	# Ensuite on appelle le constructeur
	move $a0, $s1
	li   $a1, 1024
	jal FibMem_new
	
	# Appel o.fib(5)
	# Préparation des paramètres
	move $a0, $s0
	li   $a1, 8
	# Recherche de l'adresse du code à appeler
	lw   $t0, 0($s0)  # t0 <- adresse du descripteur de classe de s0
	lw   $t0, 12($t0)  # t0 <- deuxième champ de contenu du descripteur
	# Appel
	jalr $t0          # Appel à une adresse calculée
	
	# Appel o.print()
	# Préparation des paramètres
	move $a0, $s0
	# Recherche de l'adresse du code à appeler
	lw   $t0, 0($s0)
	lw   $t0, 8($t0)
	# Appel
	jalr $t0

	# Appel om.fib(5)
	# Préparation des paramètres
	move $a0, $s1
	li   $a1, 8
	# Recherche de l'adresse du code à appeler
	lw   $t0, 0($s1)  # t0 <- adresse du descripteur de classe de s1
	lw   $t0, 12($t0)  # t0 <- deuxième champ de contenu du descripteur
	# Appel
	jalr $t0          # Appel à une adresse calculée
	
	# Appel om.print()
	# Préparation des paramètres
	move $a0, $s1
	# Recherche de l'adresse du code à appeler
	lw   $t0, 0($s1)
	lw   $t0, 8($t0)
	# Appel
	jalr $t0

	# Terminaison
	li $v0, 10
	syscall
	
# Codes des différentes méthodes

# Constructeur pour Fib
Fib_new:
	# Init deuxième champ : c=0
	li $t0, 0
	sw $t0, 4($a0)
	# Retour
	jr $ra

# Constructeur pour FibMem
FibMem_new:
	# Appel de super()
	# Sauvegarde de $ra
	sw $ra, 0($sp)
	sub $sp, $sp, 4
	# Calcul de l'adresse du constructeur super=
	lw $t0, 0($a0)
	lw $t0, 0($t0)
	lw $t0, 4($t0)
	# Appel
	jalr $t0

	# Init troisième champ : un tableau de $a1 cellules
	# On préserve d'abord $a0
	move $t0, $a0
	# Allocation
	mul $a0, $a1, 4
	li $v0, 9
	syscall
	sw $v0, 8($t0)

	add $sp, $sp, 4
	lw $ra, 0($sp)
	# Retour
	jr $ra
	

# Fib_print sera utilisée par les objets des deux classes
Fib_print:
	# Convention d'appel :
	# - le paramètre implicite est donné par son adresse dans $a0
	# - les paramètres explicites sont passés par $a1, $a2, $a3
	# - le résultat est renvoyé dans $v0
	# Remarque : ici on se passe des sauvegardes de fp et ra, qui ne
	# seront pas modifiés par Fib_print.
	lw $a0, 4($a0)  # Charge la valeur du compteur dans $a0
	li $v0, 1
	syscall         # Affichage
	jr $ra
	
# Pour fib on a une version original Fib_fib et une redéfinie FibMem_fib
Fib_fib:
	# La séquence de sauvegarde de fp et ra ne sera utile que dans le
	# cas de l'itération, on peut placer en dehors l'incrément de c
	# et le cas de base

	# Incrément de c
	lw  $t0, 4($a0)  # c est dans le premier champ utile de l'objet
	add $t0, $t0, 1  # incrément
	sw  $t0, 4($a0)  # écriture
	
	# Test et cas de base
	bgt $a1, 1, Fib_fib_main_case
	move $v0, $a1
	b Fib_fib_end

Fib_fib_main_case:
	# Sauvegarde des pointeurs fp et ra et allocation de deux mots
	sw $fp, 0($sp)
	sw $ra, -4($sp)
	sub $fp, $sp, 4
	sub $sp, $sp, 16
	# On sauvegarde la valeur de n ($a1) dans notre premier mot de pile,
	# car on en aura encore besoin pour le deuxième appel récursif
	# En revanche, on manipulera toujours le même objet o, donc pas
	# besoin de faire de même avec $a0
	sw $a1, -4($fp)

	# Appel fib(n-2)
	# Préparation des paramètres
	# Calcul et passage de n-2
	sub $a1, $a1, 2
	# (rien à faire pour [this], qui est déjà dans $a0)
	# Recherche de l'adresse du code à appeler
	lw $t0, 0($a0)
	lw $t0, 12($t0)
	# Appel
	jalr $t0
	# Le résultat est dans $v0, on l'enregistre dans le deuxième mot
	# de pile
	sw $v0, -8($fp)
	
	# Appel fib(n-1)
	# Préparation des paramètres
	# Calcul et passage de n-1
	lw $t0, -4($fp)
	sub $a1, $t0, 1
	# Recherche de l'adresse du code
	lw $t0, 0($a0)
	lw $t0, 12($t0)
	# Appel
	jalr $t0
	# Le résultat est dans $v0

	# Calcul de la somme et préparation du retour
	lw $t0, -8($fp)    # Récupération de fib(n-2)
	add $v0, $t0, $v0  # Récup de fib(n-1), somme et passage du résultat
	
	# Désallocation du tableau d'activation et restauration de ra et fp
	add $sp, $fp, 4
	lw  $ra, 0($fp)
	lw  $fp, 4($fp)
	# Le résultat est déjà dans $v0

Fib_fib_end:
	# Retour
	jr $ra
	
FibMem_fib:
	# On interprète la non-appartenance comme une case de valeur 0
	lw  $t0, 8($a0)    # Chargement de l'adresse de la table
	mul $t1, $a1, 4    # Calcul du décalage
	add $t0, $t0, $t1  # Adresse à tester
	lw  $v0, 0($t0)    # Lecture de la valeur
	# La valeur est placée dans $v0 pour uniformiser avec l'autre cas
	
	# Si non nul, retour
	bnez $v0, FibMem_fib_end
	
	# Sinon, il faut calculer
	# Pour commencer, sauvegarde propre de fp, ra, et l'adresse où
	# enregistrer fib(n)
	sw $fp, 0($sp)
	sw $ra, -4($sp)
	sub $fp, $sp, 4
	sub $sp, $sp, 12
	sw $t0, -4($fp)

	# Appel super.fib(n)
	# Préparation des paramètres : $a1 contient déjà n
	# Recherche de l'adresse du code à appeler
	lw $t0, 0($a0)  # Chargement de l'adresse du descripteur (FibMem_descr)
	lw $t0, 0($t0)  # Chargement du descripteur super (Fib_descr)
	lw $t0, 12($t0)  # Récupération de l'adresse de la méthode
	# Appel
	jalr $t0
	# Le résultat est dans $v0

	# Reste : stockage du résultat et fin de l'appel
	lw $t0, -4($fp)
	sw $v0, 0($t0)
	# Désallocation du tableau d'activation et restauration de ra et fp
	add $sp, $fp, 4
	lw  $ra, 0($fp)
	lw  $fp, 4($fp)
	# Le résultat est déjà dans $v0
	
FibMem_fib_end:
	# Retour
	jr $ra

	
# Partie données
# On y place les descripteurs de classes
	.data
Fib_descr:
	# Pas de super-classe
	.word 0
	# Pointeur vers le constructeur
	.word Fib_new
	# Pointeurs vers les bonnes versions de print et fib
	.word Fib_print
	.word Fib_fib
FibMem_descr:
	# Pointeur vers le descripteur de la super-classe
	.word Fib_descr
	# Pointeur vers le constructeur
	.word FibMem_new
	# Pointeurs vers les bonnes versions de print et fib
	.word Fib_print
	.word FibMem_fib
