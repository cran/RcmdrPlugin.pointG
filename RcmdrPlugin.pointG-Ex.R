pkgname <- "RcmdrPlugin.pointG"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('RcmdrPlugin.pointG')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("is.binary")
### * is.binary

flush(stderr()); flush(stdout())

### Name: is.binary
### Title: Identification d'une variable binaire
### Aliases: is.binary

### ** Examples

w<-sample(c(1,2),size=10,replace=TRUE)
is.binary(w)
x<-factor(w)
is.binary(x)
y<-sample(c(1,2,3),size=10,replace=TRUE)
is.binary(y)
z<-factor(y)
is.binary(z)




cleanEx()
nameEx("multivariate0")
### * multivariate0

flush(stderr()); flush(stdout())

### Name: multivariate0
### Title: Analyse factorielle exploratoire
### Aliases: multivariate0

### ** Examples

data(survey)
s1<-survey[,1:5]
s2<-survey[,6:12]
#multivariate(s1,s2)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
