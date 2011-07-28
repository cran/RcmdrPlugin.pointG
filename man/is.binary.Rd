\name{is.binary}
\alias{is.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Identification d'une variable binaire}
\description{
Definit si une variable est binaire, qu'elle soit categorielle, numerique, logique ou chaines de caracteres.
}

\usage{
is.binary(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
La variable a tester
}
}


\value{
Une valeur logique
}


\author{
Stephane Champely
}




\seealso{
is.numeric, is.factor, is.ordered
}
\examples{
w<-sample(c(1,2),size=10,replace=TRUE)
is.binary(w)
x<-factor(w)
is.binary(x)
y<-sample(c(1,2,3),size=10,replace=TRUE)
is.binary(y)
z<-factor(y)
is.binary(z)

}

