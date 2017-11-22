# Compilateur A6000 Projet M1 Antoine Lanco

 ## Fait et fonctionnel :
 ### Acte I
  #### TP1)
  - 1.Traduction des structures de contrôle (UntypedtoGoto.ml)
  - 2.Traduction des expressions (GototoIr.ml)
  - 3.Allocation des variables (IrtoAllocated.ml)
  - 4.Génération de code assembleur (AllocatedtoMips.ml)

  #### TP2)
  - 1.Sucre syntaxique : une boucle for
  - 2.Aider son prochain : messages d'erreur (RECOMMANDÉ)
     - 2.1.Erreurs de syntaxe

  #### TP3)
  - 1.Analyse de vivacité
  - 2.Élimination de code mort

  #### TP4)
  - 1.Allocation sans frais
  - 1'.Allocation de registres, pour de vrai
     - 1'.1. Construction du graphe d'interférence
     - 1'.2. Algorithme de coloration
     - 1'.3. Allocation
        - 2.2.1. Méthode ad hoc

 ### Acte II
  #### TP5)
  - 1 Description
    - 1.1 Extension du langage source
      - 1.1.1 Lexique
      - 1.1.2 Grammaire
      - 1.1.3 Semantique
    - 1.2 Extention du compilateur
      - 1.2.1 Syntaxes abstraites
      - 1.2.2 Convertions d'appel
  - 3 Extensions
    - 3.3 Conventions d'appel, suite
      - 3.3.1 Sauvegarder les registres (RECOMMANDÉ)
      - 3.3.2 Paramètres et résultat

  #### TP6)
  - 1 Description
    - 1.1 Extension du langage source
      - 1.1.1 Lexique
      - 1.1.2 Grammaire
      - 1.1.3 Sémantique
      - 1.1.4 Sémantique : précision supplémentaire
    - 1.2 Extension du compilateur
      - 1.2.1 Syntaxes abstraites
      - 1.2.2 Représentation
      - 1.2.3 Gestion du tas
  - 3 Extensions
    - 3.2 Sucre syntaxique : boucles inconditionnelles
    - 3.3 Tableaux initialisés

  #### TP7)
  - Pas fais

  #### TP8)
  - Travail attendu


 ## Info sup :

 https://github.com/antoinelanco/Compilateur
 https://www.lri.fr/~blsk/Compilation/

 Dans certain cas en mode full stack on a une erreur stack overflow mais avec le parametre -O ca marche toujours

  - Incrementation (++)
  - Decrementation (--)
  - Divition (/)
  - More than (>)
  - More Equal (>=)
