\name{bref.variable0}
\alias{bref.variable0}
\title{Analyse statistique univariee express}

\description{Analyse statistique univariee express d'une variable : pour une variable numérique, la moyenne est renvoyee et pour une variable categorielle (factor) la categorie de plus grand effectif, ainsi que le pourcentage associe.
}
\usage{
bref.variable0(x,digits=max(3, getOption("digits") - 
    3))}


\arguments{
  \item{x}{
A vector of class numeric or factor
}
  \item{digits}{
Number of significant digits for the result
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

