\name{bisummarize0}
\alias{bisummarize0}
\title{Analyse bivariee numerique par tranche}
\description{
Realise pour un ensemble de variables les croisements avec une autre variable (explicative). 
Selon la nature de ces variables des calculs differents sont effectues.

}
\usage{
bisummarize0(X,y,tri=FALSE)
}

\arguments{
  \item{X}{
Le nom du jeu de donnees contenant les variables a expliquer}
\item{y}{La variable explicative
}
\item{tri}{Valeur logique pour (ne pas) trier les variables en fonction de la p-value
}
}

\value{
Un tableau dont les lignes sont les variables à expliquer et les colonnes les éléments
\item{CR}{Carres des correlations entre les sous-espaces vectoriels engendres par les deux variables (ou les indicatrices 
pour une variable categorielle)}
\item{ES}{Tailles d'effet au sens de Cohen : w, f ou r}
\item{Size}{Valeurs conventionnelles correspondant aux tailles d'effet : XS, S, M, L, XL}
\item{p.value}{Les p-values des tests correspondants}
}
\references{
Cohen
}
\author{
Stephane Champely
}


