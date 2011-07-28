plotLikert0<- function (XX, yy = FALSE, ...) {
XXX <- get(XX)

if(!yy){
X<-XXX
X <- as.matrix(X)
        Mean <- apply(X, 2, mean, na.rm = TRUE)
        Min <- min(X,na.rm = TRUE)
        Max <- max(X,na.rm = TRUE)
        dotchart(sort(Mean), xlim = c(Min, Max),pch=16,col=rev(brewer.pal(3,name="PuRd"))[1], ...)
}
else{
X<-XXX[,-1]
p <- ncol(X)
y <- XXX[,1]

X <- as.matrix(X)
 Min <- min(X,na.rm = TRUE)
        Max <- max(X,na.rm = TRUE)

           
        facto <- factor(y)
        k <- length(levels(facto))
        XXX <- matrix(0, nrow = k, ncol = p)
        for (j in 1:p) {
            XXX[, j] <- tapply(X[, j], facto, mean, na.rm = TRUE)
        }
        XXXX <- as.vector(XXX)
        f1 <- factor(rep(levels(facto), p))
        f2 <- factor(rep(colnames(X), rep(k, p)))
        f2 <- reorder(f2, XXXX, mean)
        dotplot(f2 ~ XXXX | f1, xlab = "", , xlim = c(Min, Max),pch=16,col=rev(brewer.pal(3,name="PuRd"))[1],...)
}
}
