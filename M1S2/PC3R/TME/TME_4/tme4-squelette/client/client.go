package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"net"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
	tr "./travaux"
	st "./structures" // contient la structure Personne
	// contient les fonctions de travail sur les Personnes
)

var ADRESSE string = "localhost"                           // adresse de base pour la Partie 2
var FICHIER_SOURCE string = "./1-rne-cm.txt"               // fichier dans lequel piocher des personnes
var TAILLE_SOURCE int = 7                                  // inferieure au nombre de lignes du fichier, pour prendre une ligne au hasard
var TAILLE_G int = 5                                       // taille du tampon des gestionnaires
var NB_G int = 2                                           // nombre de gestionnaires
var NB_P int = 2                                           // nombre de producteurs
var NB_O int = 4                                           // nombre d'ouvriers
var NB_PD int = 2                                          // nombre de producteurs distants pour la Partie 2

var pers_vide = st.Personne{Nom: "", Prenom: "", Age: 0, Sexe: "M"} // une personne vide

// paquet de personne, sur lequel on peut travailler, implemente l'interface personne_int
type personne_emp struct {
	personne st.Personne
	ligne    int
	afaire   []func(st.Personne) st.Personne
	statut   string
	lecture  chan string
}

type message_proxy struct{
	id int
	reponse chan string
	method string
}

// paquet de personne distante, pour la Partie 2, implemente l'interface personne_int
type personne_dist struct {
	id int
	proxy chan message_proxy
}

// interface des personnes manipulees par les ouvriers, les
type personne_int interface {
	initialise()          // appelle sur une personne vide de statut V, remplit les champs de la personne et passe son statut à R
	travaille()           // appelle sur une personne de statut R, travaille une fois sur la personne et passe son statut à C s'il n'y a plus de travail a faire
	vers_string() string  // convertit la personne en string
	donne_statut() string // renvoie V, R ou C
}

// fabrique une personne à partir d'une ligne du fichier des conseillers municipaux
// à changer si un autre fichier est utilisé
func personne_de_ligne(l string) st.Personne {
	separateur := regexp.MustCompile("\u0009") // oui, les donnees sont separees par des tabulations ... merci la Republique Francaise
	separation := separateur.Split(l, -1)
	naiss, _ := time.Parse("2/1/2006", separation[7])
	a1, _, _ := time.Now().Date()
	a2, _, _ := naiss.Date()
	agec := a1 - a2
	return st.Personne{Nom: separation[4], Prenom: separation[5], Sexe: separation[6], Age: agec}
}

// *** METHODES DE L'INTERFACE personne_int POUR LES PAQUETS DE PERSONNES ***

func (p *personne_emp) initialise() {
	//fmt.Println("start reading")
	go func(){lecteur(p.ligne,p.lecture)}()
	//fmt.Println("End reading")
	p.personne = personne_de_ligne(<-p.lecture)
	//fmt.Println("Person loaded from source code")
	// insere dans le tableau afaire un nombre al ́eatoire (1-5) de fonctions de travail
	for i := 0; i <= rand.Intn(6); i++{
		p.afaire = append(p.afaire, tr.UnTravail())
	}
	p.statut = "R"
	//fmt.Println("La ligne du personne",p.personne.Nom,"est",p.ligne,"statut",p.statut)
}

func (p *personne_emp) travaille() {
	p.personne = p.afaire[0](p.personne)
	p.afaire = p.afaire[1:]
	if len(p.afaire) == 0 {
		p.statut = "C"
	}
	//fmt.Println("Test tasks :",len(p.afaire))

}

func (p *personne_emp) vers_string() string {
	str := ""
	str += " Nom :" + p.personne.Nom + "\n Preonm :" + p.personne.Prenom + "\n Age :" + strconv.Itoa(p.personne.Age) + "\n Sexe :" + p.personne.Sexe
	return str
}

func (p *personne_emp) donne_statut() string {
	return p.statut
}

// *** METHODES DE L'INTERFACE personne_int POUR LES PAQUETS DE PERSONNES DISTANTES (PARTIE 2) ***
// ces méthodes doivent appeler le proxy (aucun calcul direct)

func (p personne_dist) initialise() {
	paq := make(chan string)
	message := message_proxy{id: p.id, method: "initialise", reponse: paq}
	p.proxy <- message
	<- paq

}

func (p personne_dist) travaille() {
	paq := make(chan string)
	message := message_proxy{id: p.id, method: "travaille", reponse: paq}
	p.proxy <- message
	<- paq
}

func (p personne_dist) vers_string() string {
	paq := make(chan string)
	message := message_proxy{id: p.id, method: "ver_string", reponse: paq}
	p.proxy <- message
	return <- paq
}

func (p personne_dist) donne_statut() string {
	paq := make(chan string)
	message := message_proxy{id: p.id, method: "donne_statut", reponse: paq}
	p.proxy <- message
	return <- paq
}

// *** CODE DES GOROUTINES DU SYSTEME ***

// Partie 2: contacté par les méthodes de personne_dist, le proxy appelle la méthode à travers le réseau et récupère le résultat
// il doit utiliser une connection TCP sur le port donné en ligne de commande
func proxy(port string, mess chan message_proxy) {
	address := ADRESSE + ":" + port
	conn, _ := net.Dial("tcp", address)
	for {
		message := <- mess
		requete := strconv.Itoa(message.id) + "," + message.method + "\n"
		fmt.Fprintf(conn, fmt.Sprintf(requete))
		recu, _ := bufio.NewReader(conn).ReadString('\n')
		reponse := strings.TrimSuffix(recu, "\n")
		fmt.Println("Reponse receive of server: " + reponse)
		message.reponse <- reponse
	}
	conn.Close()

}

// Partie 1 : contacté par la méthode initialise() de personne_emp, récupère une ligne donnée dans le fichier source
func lecteur(ligne int, lec chan string) {
	// ouvrire le fichier
	file, err := os.Open(FICHIER_SOURCE)
	if err != nil {
		fmt.Println("Error_READ")
	}
	file, err = os.Open(FICHIER_SOURCE)
	//fmt.Println("fichier open with line ",ligne)
	defer file.Close()
	fileScanner := bufio.NewScanner(file)
	// lire le contenu de la ligne
	lineCount := 1
	for fileScanner.Scan() {
		//fmt.Println(lineCount)
		if lineCount == ligne {
			lec <- fileScanner.Text()
		}
		lineCount++
	}
}

// Partie 1: récupèrent des personne_int depuis les gestionnaires, font une opération dépendant de donne_statut()
// Si le statut est V, ils initialise le paquet de personne puis le repasse aux gestionnaires
// Si le statut est R, ils travaille une fois sur le paquet puis le repasse aux gestionnaires
// Si le statut est C, ils passent le paquet au collecteur
func ouvrier(ouv chan personne_int, col chan personne_int , paq chan personne_int) {
	//fmt.Println("----Begin work!----")
	for {
		personne := <- ouv
		//fmt.Println("Ouv receive paq from gest:",personne.donne_statut())
		if personne.donne_statut() == "V" {
			personne.initialise()
			//fmt.Println("Ouv send paq to gest V :",personne.donne_statut())
			paq <- personne
		} else {
			if personne.donne_statut() == "R" {
				personne.travaille()
				//fmt.Println("Ouv send paq to gest R :",personne.donne_statut())
				paq <- personne
			} else {
				if personne.donne_statut() == "C"{
					//fmt.Println("Ouv send paq to col C :",personne.vers_string())
					col <- personne
				}
			}
		}
	}
}

// Partie 1: les producteurs cree des personne_int implementees par des personne_emp initialement vides,
// de statut V mais contenant un numéro de ligne (pour etre initialisee depuis le fichier texte)
// la personne est passée aux gestionnaires
func producteur(lec chan string, prod chan personne_int) {
	//fmt.Println("----Begin production!----")
	for {
		personne := personne_emp{
			ligne:    rand.Intn(TAILLE_SOURCE-2) + 3,
			personne: pers_vide,
			afaire:   make([]func(st.Personne) st.Personne, 0),
			statut:   "V",
			lecture:  lec,
		}
		prod <- personne_int(&personne)
		//fmt.Println("Prod send paq to gest :",personne.statut)
	}
}

// Partie 2: les producteurs distants cree des personne_int implementees par des personne_dist qui contiennent un identifiant unique
// utilisé pour retrouver l'object sur le serveur
// la creation sur le client d'une personne_dist doit declencher la creation sur le serveur d'une "vraie" personne, initialement vide, de statut V
func producteur_distant(gest chan personne_int, mess chan message_proxy, id chan int) {
	for{
		ide := <- id
		personne := personne_dist{id: ide, proxy: mess}
		retour := make(chan string)
		mess <- message_proxy{id: ide, method: "creer", reponse: retour}
		<- retour
		// send paq to gest
		gest <- personne
	}

}

// Partie 1: les gestionnaires recoivent des personne_int des producteurs et des ouvriers et maintiennent chacun une file de personne_int
// ils les passent aux ouvriers quand ils sont disponibles
// ATTENTION: la famine des ouvriers doit être évitée: si les producteurs inondent les gestionnaires de paquets, les ouvrier ne pourront
// plus rendre les paquets surlesquels ils travaillent pour en prendre des autres
func gestionnaire(paq_vide chan personne_int,paq chan personne_int, ouv chan personne_int) {
	//fmt.Println("----Begin gestion!----")
	tab := make([]personne_int, 0)
	for{
		if len(tab) == TAILLE_G{
			//full
			ouv <- tab[0]
			tab = tab[1:]
		}else if len(tab) == 0{
			//recieve paq only when taille = 0
			select {
			case personne := <- paq_vide:
				tab = append(tab, personne)
				//fmt.Println("000000 Gest recieve paq_vide from prod",personne.donne_statut())
			case personne := <- paq:
				tab = append(tab, personne)
				//fmt.Println("000000 Gest recieve paq from ouv",personne.donne_statut())
			}
		}else if len(tab) < TAILLE_G - 1{
			//2 places pour paquet de ouvrier
			select {
			case personne := <- paq_vide:
				tab = append(tab, personne)
				//fmt.Println("Gest receive paq_vide from prod :",tab[0].donne_statut())
			case personne := <- paq:
				tab = append(tab, personne)
				//fmt.Println("Gest receive paq from ouv :",tab[0].donne_statut())
			case ouv <- tab[0]:
				//fmt.Println("Gest send paq_vide to ouv :",tab[0].donne_statut())
				tab = tab[1:]
			}
		}else {
			select {
			case personne := <- paq:
				tab = append(tab, personne)
				//fmt.Println("Gest recieve paq from ouv :",tab[0].donne_statut())
			case ouv <- tab[0]:
				tab = tab[1:]
				//fmt.Println("Gest send paq to ouv :",tab[0].donne_statut())
			}
		}
	}
}

// Partie 1: le collecteur recoit des personne_int dont le statut est c, il les collecte dans un journal
// quand il recoit un signal de fin du temps, il imprime son journal.
func collecteur(ouv chan personne_int, fin chan int) {
	//fmt.Println("----Begin collection!----")
	var journal string
	for {
		per := <-ouv
		//fmt.Println("Col receive paq from ouv :",per.donne_statut())
		journal = journal + per.vers_string()
		select {
		case paq := <-ouv:
			journal += paq.vers_string() + "\n"
		case <-fin:
			fmt.Println("Journal:\n" + journal)
			fin <- 0
			return
		}
	}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano()) // graine pour l'aleatoire
	if len(os.Args) < 3 {
		fmt.Println("Format: client <port> <millisecondes d'attente>")
		return
	}
	port := os.Args[1]   // utile pour la partie 2
	millis, _ := strconv.Atoi(os.Args[2]) // duree du timeout
	fintemps := make(chan int)
	// creer les canaux de partie 1
	lect := make(chan string)
	paq_vide := make(chan personne_int)
	paq := make(chan personne_int)
	ouv := make(chan personne_int)
	col := make(chan personne_int)
	// creer les canaux de partie 2
	mess := make(chan message_proxy)
	id := make(chan int)
	//fmt.Println("Les canaux sont crees")
	// lancer les goroutines (parties 1 et 2): 1 lecteur, 1 collecteur, des producteurs, des gestionnaires, des ouvriers
	for i := 0; i < NB_P; i++ {
		go func() { producteur(lect, paq_vide) }()
	}
	for i := 0; i < NB_G; i++ {
		go func() { gestionnaire(paq_vide,paq, ouv) }()
	}
	for i := 0; i < NB_O; i++ {
		go func() { ouvrier(ouv, col, paq) }()
	}
	go func() { collecteur(col,fintemps) }()
	//lancer les goroutines (partie 2): des producteurs distants, un proxy
	// lancer les goroutines (partie 2): des producteurs distants, un generator d'identifiant, un proxy
	go func(){
		proxy(port, mess)
	}()
	go func(){
		compteur := 0
		for{
			id <- compteur
			compteur++
		}
	}()
	for i:= 0; i < NB_PD; i++{
		go func() {
			producteur_distant(paq, mess, id)
		}()
	}

	time.Sleep(time.Duration(millis) * time.Millisecond)
	fintemps <- 0
	<-fintemps
}
