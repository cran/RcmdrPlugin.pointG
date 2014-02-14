\name{plotLikert0}
\alias{plotLikert0}
\title{Analyse graphique d'echelles de Likert similaires}
\description{
Realise une analyse graphique d'une batterie d'echelles de Likert similaires, eventuellement differenciees selon
une variable categorielle.}

\usage{
plotLikert0(X,tri=0,adaptation=0,...)
}

\arguments{
  \item{X}{
Jeu de donnees contenant la batterie d'echelles
}
  \item{tri}{
0= pas de tri
}
  \item{adaptation}{
adaptation=0 : echelle des abscisses correspondant a min et max sur les donnes alors que adaptation=1 : echelle des abscisses correspondant a min et max des moyennes (meilleure visualisation des differences fines).
}
\item{...}{
Divers renseignements graphiques
}
}

\value{
Pas de valeurs renvoyees, seulement des graphiques
}

\author{
Stephane Champely
}

