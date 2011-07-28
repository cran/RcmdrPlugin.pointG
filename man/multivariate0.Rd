\name{multivariate0}
\alias{multivariate0}

\title{Analyse factorielle exploratoire}
\description{
Realise une analyse factorielle exploratoire pour tout type de variable (Hill & Smith) ainsi qu'une eventuelle
visualisation de variables supplementaires
}
\usage{
multivariate0(XX, YY = NULL)
}

\arguments{
  \item{XX}{
Le jeu de donnees contenant les variables actives
}
  \item{YY}{
Le jeu de donnees contenant les variables supplementaires}
}


\value{
Un objet du type dudi.hillsmith (et des graphiques)
}
\references{
Merci a Daniel Chessel (pour tout un tas de raisons !)
}
\author{
Stephane Champely
}
\note{
Cette fonction utilise grandement des elements du package ade4.
}




\examples{
data(survey)
s1<-survey[,1:5]
s2<-survey[,6:12]
#multivariate(s1,s2)
}

