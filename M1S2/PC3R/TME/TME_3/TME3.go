package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

const n_tv int = 5

type paquet struct {
	arrivee string
	depart  string
	arret   int
}

func lire_fichier(lecteur chan string, filepath string) {
	//l'ouverture du fichier
	file, err := os.Open(filepath)
	if err != nil {
		fmt.Println("Error_READ")
		return
	}
	defer file.Close()

	//lire fichier ligne par ligne
	scanner := bufio.NewScanner(file)
	scanner.Scan()
	for scanner.Scan() {
		lecteur <- scanner.Text()
	}
}

func travailleur(lecteur chan string, travailleurs chan chan paquet, reducteur chan paquet) {
	for {
		//recoire une ligne de donne
		ligne := <-lecteur
		//convertir en un paquet
		var temps []string = strings.Split(ligne, ",")
		var p paquet
		p.arrivee = temps[1]
		p.depart = temps[2]
		p.arret = 0
		paq := make(chan paquet)
		//envoyer au serveur
		travailleurs <- paq
		//recoire le paquet apres le calcul
		paq <- p
		res := <-paq
		//envoyer au reducteur
		reducteur <- res
	}
}

func serveur(travailleurs chan chan paquet) {
	for {
		//recoire le paquet du travailleur

		travailleur := <-travailleurs
		p := <-travailleur
		if p.arret == 0 {
			//calculer le paquet
			arr, _ := time.Parse("15:04:05", p.arrivee)
			dep, _ := time.Parse("15:04:05", p.depart)
			sub := dep.Sub(arr)
			//if p.arret != 0 {
			p.arret = int(sub.Minutes()) % 525600
			//renvoyer au travailleur
			travailleur <- p
		}
	}
}

func reducteur(prin chan int, paq_tv chan paquet) {
	var somme int
	var cpt int
	for {
		select {
		//recoire le paquet apres le calcul
		case paq := <-paq_tv:
			somme = somme + paq.arret
			cpt = cpt + 1
		//apres traiter les paquets
		case <-prin:
			//calculer
			fmt.Println(" Temps total               : ", somme, " minutes \n Compteur                  : ", cpt, " trains")
			prin <- somme / cpt
			return
			//default:
			//	fmt.Println("default!.")
		}
	}
}

func main() {
	//creer les canaux
	lecteur := make(chan string)
	travailleurs := make(chan chan paquet)
	prin := make(chan int)
	paquets := make(chan paquet)

	//lancer les processus
	go func(filepath string) { lire_fichier(lecteur, filepath) }("./stop_times.txt")

	for i := 0; i < n_tv; i++ {
		go func() { travailleur(lecteur, travailleurs, paquets) }()
	}
	go func() { serveur(travailleurs) }()
	go func() { reducteur(prin, paquets) }()

	//attendre 1 second
	time.Sleep(1 * (time.Second))
	prin <- 0
	res := <-prin
	fmt.Printf("le temps moyens des trains :  %d minutes\n", res)
}

// ------------------Resultat------------------
//  Temps total               :  52594  minutes
//  Compteur                  :  16761  trains
// le temps moyens des trains :  159 minutes
