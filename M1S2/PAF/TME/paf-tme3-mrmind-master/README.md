# TME 3 : MrMind

Dans ce TME nous adoptons une approche "Test-Driven" : il faut lire le code
source (en particulier les types) et les tests associés pour comprendre ce qu'il faut faire.

C'est une occasion, également, de comprendre comment mettre en oeuvre les
tests unitaires en Haskell.

## Projet MrMind à compléter.

 - dans `src/MindEngine.hs` se trouve le coeur du programme, certaines fonctions ne sont pas définies 
(cherchez les `undefined`)

 - dans `test/MindEngineSpec.hs` se trouve les tests unitaire (HSpec) du coeur. Les tests pour les fonctions
à compléter sont écrits. La fonction `verify` n'est pas correctement testée (tests à compléter)


- toutes les fonctions du noyau doivent être testées
- `stack test` doit valider tous les tests

## Défi : Un vrai MrMind

Compléter le projet en ajoutant les modes de jeu proposés dans le point d'entrée du programme.

(rappel : les défis sont optionels, pour "aller plus loin")

## Références

Voici quelques pointeurs utiles pour compléter/comprendre le projet :

 - guide du jeu de Mastermind : <https://fr.wikipedia.org/wiki/Mastermind>

 - les tests unitaires avec HSpec : <https://hspec.github.io/>

 - entrées/sorties en Haskell : <http://learnyouahaskell.com/input-and-output>
   (notamment la partie *Randomness* pour le défi)
