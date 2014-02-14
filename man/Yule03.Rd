\name{Yule03}
\alias{Yule03}
\title{Profil de modalites en utilisant la statistique Q de Yule}

\description{A partir d'une variable explicative categorielle, l'une de ses categories est choisie et croisee avec une tranche de variables.
En particulier c'est la liaison entre cette modalite particuliere et toutes les modalites de la tranche de variables qui est calculee.
Pour ce faire, primo, les variables numeriques de la tranche sont d'abord transformees en categorielle binaire en employant comme seuil de coupure la moyenne de la variable.
Deuxio, pour le calcul de la liaison entre les deux modalites, un tableau 2*2 est cree en mettant d'une part la modalite choisie et d'autre part en regroupant les autres.
Tertio, la statistique Q, son écart-type et la p-value du test du chi-carre correspondant sont calcules.
}

\usage{
Yule03(X,YY,levX,nameYY,tri=0)
}


\arguments{
  \item{X}{
La variable categorielle pivot.
}
  \item{YY}{
La tranche de variables.
}
  \item{levX}{
La modalite concernee de la variable X.
}
  \item{nameYY}{
Les noms des variables de la tranche.
}
  \item{tri}{
Variable 0/1 permettant de trier ou non les lignes du tableau en fonction de la valeur de la statistique Q.
}


}


\value{
Un tableau est retourne avec en lignes chaque modalite pour les variables de la tranche, en particulier deux pour chaque variable numerique : plus (au dessus de la moyenne), moins (au dessous de la moyenne).
En colonnes, l'effectif dans la case croisant les deux modalites visees, puis la valeur de Q de la table 2*2 correspondante, son ecart-tyoe, en enfin la p-value.
}


\author{
Stephane Champely
}