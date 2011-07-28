bivariate0<-function (XX) 
{
    XXX <- get(XX)
noms <- colnames(XXX)
X<-XXX[,-1]
nomsX<-noms[-1]
p <- ncol(X)

y <- XXX[,1]
nomsy<-noms[1]
    
    noms <- colnames(X)
yy<-names(y)
    windows()
    if(p==3){par(mfrow = c(2,2))}else{par(mfrow = n2mfrow(p))}
    for (j in 1:p) {
        if (is.factor(X[, j]) & is.factor(y)) {
            mosaicplot(table(y, X[, j]), main = noms[j], xlab = nomsy, 
                las = 1,col=rev(brewer.pal(3,name="PuRd")))
        }
        if (is.numeric(X[, j]) & is.numeric(y)) {
            plot(y, X[, j], main = noms[j], xlab = nomsy, ylab = "", 
                las = 1,col=rev(brewer.pal(3,name="PuRd"))[1])
        }
        if (is.numeric(X[, j]) & is.factor(y)) {
            boxplot(X[, j] ~ y, main = noms[j], xlab = nomsy, ylab = "",col=rev(brewer.pal(3,name="PuRd"))[1])
        }
        if (is.numeric(y) & is.factor(X[, j])) {
            boxplot(y ~ X[, j], main = noms[j], xlab = nomsy, ylab = "", 
                horizontal = TRUE, las = 1,col=rev(brewer.pal(3,name="PuRd"))[1])
        }
    }
}
