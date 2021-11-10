
TME4 - PAF - Tutoriel Quickcheck
================================

Pour ce TME nous ajoutons à notre infrastructure l'un des outils les
plus importants de l'éco-système Haskell : QuickCheck.  Il s'agit d'un
outil de *property-based testing* qui permet de tester des propriétés
par génération aléatoire (plutôt «arbitraire») des données manipulées dans les programmes.

Pour répondre à ce TME on suivra les explications ci-dessous, divisées
en trois exercices indépendants.

Les documents à consulter pour pouvoir suivre ce TME sont les suivants :

 - La page de documentation de référence pour QuickCheck (version 2.14.2)

cf. https://www.stackage.org/haddock/lts-17.2/QuickCheck-2.14.2/Test-QuickCheck.html

 - le manuel de QuickCheck (relativement incomplet)

cf. http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

  - l'intégration de QuickCheck dans HSpec

cf. http://hspec.github.io/quickcheck.html


**Remarque** : dans ce TME, il s'agit de se familiariser avec QuickCheck et de commencer à comprendre son mode d'emploi à travers une sélection d'exemples. Bien sûr, la lecture du "véritable" mode d'emploi de l'outil est vivement recommandée. Lors du TME8 nous reviendrons sur QuickCheck en étudiant la conception et l'implémentation de *générateurs arbitraires*.

----

Exercice 1 : `reverse`, ça renverse !
----------

Dans le module `Revrev`  (fichier `src/Revrev.hs`) on trouvera une propriété
portant sur la fonction `reverse` du prélude de Haskell.

Dans le module de test `RevrevSpec` (fichier `test/RevrevSpec.hs`) cette
propriété est vérifiée avec QuickCheck sur une sélection des listes en
entrée.

Pour lancer ces tests, il suffit d'invoquer la commande :

```
$ stack test
...

reverse
  when used with ints
    is idempotent
      +++ OK, passed 100 tests.
reverse
  when used with ints
    commutes with append FAILED [1]
...

```

La seconde propriété de `reverse` n'est donc pas vérifiée, voyez-vous le problème ? Si oui corrigez et relancez les tests.

Exercice 2 : machin ou `unsplit` `split` machin, c'est pareil !
----------

Dans le module `Split` (fichier `src/Split.hs`) on a déclaré deux fonctions `split` et `unsplit`
 ainsi qu'une propriété que ces fonctions doivent respecter. Dans le module de test `SplitSpec`
 (fichier `test/SplitSpec.hs`) un certain nombre de vérifications sont effectuées : un test
 unitaire et quelques tests QuickCheck.

Les spécifications du module de test ne sont pas commentées. Dans cet exercice une des tâches
sera d'expliquer en commentaire ce que font précisément les spécifications de test de propriété,
 en s'inspirant du la spécification commentées de l'exercice 1 (cf. fichier `test/RevrevSpec.hs`).

Dans un deuxième temps, on définira les fonctions `split` et `unsplit` pour qu'elles puissent
passer les tests.

Exercice 3 : modélisation et tests de propriétés
----------

Dans le module `Bridge` (fichier `src/Bridge.hs`) on a modélisé un système simple de circulation
sur un pont entre un contient et une presqu'île. On a de plus spécifié un certain nombre
de tests de propriétés dans le module `BridgeSpec`  (fichier `test/BridgeSpec.hs`).

Dans une première étape, on essaiera de comprendre la modélisation à partir de son code fonctionnel
et (surtout) des propriétés et des tests associés.
Pour valider cette compréhension informelle, on ajoutera des commentaires (qui ne devraient pas être
de la paraphrase du code source)

Question 3.1
------------

On souhaite ajouter trois opérations permettant de compléter la modélisation du système.

 - une opération `leaveToIsland` qui permet à un véhicule de quitter le pont vers l'île
 - une opération `enterFromIsland` qui permet à un véhicule d'entrer sur le pont depuis l'île
 - une opération `leaveFromIsland` qui permet à un véhicule de quitter le pont vers le continent

On s'inspirera des opérations `initBridge` (initialisation) et  `enterToIsland` (entrée depuis le continent)
pour définir ces fonctions (ainsi que leur préconditions éventuelles), 
et surtout vérifier qu'elles préservent bien l'invariant du système (dans le module de test).

**Remarques** : 

 - pour que `stack test` fonctionne, il faut ajouter les nouveaux tests au
module `Spec` (cf. fichier `test/Spec.hs`).

Question 3.2. Défi
-------------

On souhaite proposer une variante du système en n'autorisant qu'un sens de circulation sur le pont.
Définir cette variante dans des module `Bridge2` et `Bridge2Spec` séparés).
Ajouter une propriété permettant de tester que tout `Bridge2` correct est aussi un `Bridge` ("tout court") correct.

