\name{llinear0}
\alias{llinear0}

\title{Modele lineaire hetero-statistique}
\description{
Realise une modelisation lineaire adaptee au type de la variable dependante, si numerique un modele de regression multiple, si binaire un modele de regression logistique a erreurs binomiales, si ordonnee un modele odds-ratio et si categorielle un modele multinomial logistique.
}
\usage{
llinear0()
}

\value{
Un objet du type modele lineaire correspondant a la classification precedente. L'objet obtenu peut etre analyse par toutes les options du menu Modeles du R-Commander : graphes, tests, criteres d'information, intervalles de confiance... 
Le resultat affiche est une table d'ANOVA ordonnee en fonction du critere de somme des carres ou chi2 divise par le nombre de degres de liberte
}

\author{
Stephane Champely
}
\note{
Cette fonction utilise les packages nnet, MASS et car.
}

