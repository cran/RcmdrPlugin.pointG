\name{Summaryvariable0}
\alias{Summaryvariable0}
\title{Analyse statistique univariee}

\description{Analyse statistique univariee d'une variable : pour une variable numerique, les statistiques usuelles sont presentees (M, SD, Q1, Q2...), pour une variable categorielle ou ordinale, par défaut ce sont les pourcentages mais il existe une option presentant les effectifs. L'ordre des categories est conservee pour les variables ordinales mais pour les categorielles nominales, elles sont presentees par effectifs decroissants.  Chaque resume se termine par le nombre de donnees manquantes (et pas le pourcentage !)
}

\usage{
Summaryvariable0(x, maxsum = 7, digits = max(3, getOption("digits") - 
    3),pourcent=1) 
}


\arguments{
  \item{x}{
A vector of class numeric or factor.
}
  \item{maxsum}{
integer, indicating how many levels should be shown for factors.
}
  \item{digits}{
Number of significant digits for the result.
}
  \item{pourcent}{
Si pourcent=1 alors les pourcentages sont calcules sinon ce sont les effectifs pour les variables categorielles.
}

}


\value{
Une valeur avec une etiquette est renvoyee. Cette fonction est generalement employee sur un tableau (data.frame) a l'aide de la fonction lapply.
}


\author{
Stephane Champely
}




\seealso{
whatis, summary
}

