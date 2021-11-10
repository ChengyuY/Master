package travaux

import (
	"math/rand"

	st "../../client/structures"
)

// *** LISTES DE FONCTION DE TRAVAIL DE Personne DANS Personne DU SERVEUR ***
// Essayer de trouver des fonctions *différentes* de celles du client


func f1(p st.Personne) st.Personne {
	return st.Personne{Nom: "Minase", Prenom:  "Inori", Sexe: "F", Age: 26}
}

func f2(p st.Personne) st.Personne {
	return st.Personne{Nom: "Sakula", Prenom: "Ayaneru", Sexe: "F", Age: 27}
}

func f3(p st.Personne) st.Personne {
	return st.Personne{Nom: "Onishi", Prenom: "Saori", Sexe: "F", Age: 29}
}

func f4(p st.Personne) st.Personne {
	return st.Personne{Nom: "Hidaka", Prenom: "Rina", Sexe: "F", Age: 27}
}

func UnTravail() func(st.Personne) st.Personne {
	tableau := make([]func(st.Personne) st.Personne, 0)
	tableau = append(tableau, func(p st.Personne) st.Personne { return f1(p) })
	tableau = append(tableau, func(p st.Personne) st.Personne { return f2(p) })
	tableau = append(tableau, func(p st.Personne) st.Personne { return f3(p) })
	tableau = append(tableau, func(p st.Personne) st.Personne { return f4(p) })
	i := rand.Intn(len(tableau))
	return tableau[i]
}
