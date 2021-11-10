# Rapport

- Je choisi Ocaml pour implémenter APS car il est facile de créer les types puis les appliquer.
- Dans ce projet, j'ai fait les implémentations de APS jusqu'à APS2 pour prologTerm, évaluateur et typage.
- J'ai également essayé de travailler sur APS3, mais je n'ai pas trouvé le bon typage et l'évaluation.
- Il y a encore des petits problèmes sur mon APS2 (lexing: empty token pour prog207.aps). Du coup je vais aussi livrer mon APS1a et APS3.
- Pour la partie du typage, la règle est présente dans le fichier check.pl.
- Pour la partie d'évaluation, elle est présente dans le fichier evaluateur.ml.
- J'ai écris un fichier check.sh pour vérifier le typage et l'évaluation des fichier .aps.
- La partie plus difficile pour moi est d'implémenter de la mémoire dan APS2. Je dois penser quelles fonctions vont traiter la mémoire, quels pour l'environnement et quelles pour la sortie.Malgré les notes de cours, j'ai pris beaucoup de temps mais pas encore réussi cette partie.

--------------

Chaque implémentations de APS compose les fichiers suivants:

- ast.ml
- lexer.mll
- paser.mly
- prologTerm.ml
- typrog
- evaluateur.ml
- check.pl
- makefile
- check.sh
