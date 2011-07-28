bisummarize0<-function (XX) 
{

 XXX <- get(XX)
noms <- colnames(XXX)[-1]
X<-XXX[,-1]
p <- ncol(X)

y <- XXX[,1]
    
 
    RC <- numeric(p)
    ES <- numeric(p)
    size <- numeric(p)
    p.value <- numeric(p)
    for (j in 1:p) {
        if (is.factor(X[, j]) & is.factor(y)) {
            ch <- chisq.test(table(y, X[, j]))
            k <- length(levels(y))
            RC[j] <- dudi.coa(data.frame(matrix(table(y, X[, 
                j]), nrow = k)), scannf = FALSE, nf = 1)$eig[1]
            ES[j] <- sqrt(ch$stat/sum(table(y, X[, j])))
            if (ES[j] <= 0.05) {
                size[j] <- "XS"
            }
            if ((ES[j] > 0.05) & (ES[j] <= 0.2)) {
                size[j] <- "S"
            }
            if ((ES[j] > 0.2) & (ES[j] <= 0.4)) {
                size[j] <- "M"
            }
            if ((ES[j] > 0.4) & (ES[j] <= 0.75)) {
                size[j] <- "L"
            }
            if (ES[j] > 0.75) {
                size[j] <- "XL"
            }
            p.value[j] <- ch$p.value
        }
        if (is.numeric(X[, j]) & is.numeric(y)) {
            RC[j] <- summary(lm(X[, j] ~ y))$r.squared
            ES[j] <- sqrt(RC[j])
            if (ES[j] <= 0.05) {
                size[j] <- "XS"
            }
            if ((ES[j] > 0.05) & (ES[j] <= 0.2)) {
                size[j] <- "S"
            }
            if ((ES[j] > 0.2) & (ES[j] <= 0.4)) {
                size[j] <- "M"
            }
            if ((ES[j] > 0.4) & (ES[j] <= 0.75)) {
                size[j] <- "L"
            }
            if (ES[j] > 0.75) {
                size[j] <- "XL"
            }
            fff <- summary(lm(X[, j] ~ y))$fstatistic
            p.value[j] <- pf(q = fff[1], df1 = fff[2], df2 = fff[3], 
                lower.tail = FALSE)
        }
        if (is.numeric(X[, j]) & is.factor(y)) {
            RC[j] <- summary(lm(X[, j] ~ y))$r.squared
            ES[j] <- sqrt(RC[j]/(1 - RC[j]))
            if (ES[j] <= 0.05) {
                size[j] <- "XS"
            }
            if ((ES[j] > 0.05) & (ES[j] <= 0.175)) {
                size[j] <- "S"
            }
            if ((ES[j] > 0.175) & (ES[j] <= 0.325)) {
                size[j] <- "M"
            }
            if ((ES[j] > 0.325) & (ES[j] <= 0.7)) {
                size[j] <- "L"
            }
            if (ES[j] > 0.7) {
                size[j] <- "XL"
            }
            fff <- summary(lm(X[, j] ~ y))$fstatistic
            p.value[j] <- pf(q = fff[1], df1 = fff[2], df2 = fff[3], 
                lower.tail = FALSE)
        }
        if (is.numeric(y) & is.factor(X[, j])) {
            RC[j] <- summary(lm(y ~ X[, j]))$r.squared
            ES[j] <- sqrt(RC[j]/(1 - RC[j]))
            if (ES[j] <= 0.05) {
                size[j] <- "XS"
            }
            if ((ES[j] > 0.05) & (ES[j] <= 0.175)) {
                size[j] <- "S"
            }
            if ((ES[j] > 0.175) & (ES[j] <= 0.325)) {
                size[j] <- "M"
            }
            if ((ES[j] > 0.325) & (ES[j] <= 0.7)) {
                size[j] <- "L"
            }
            if (ES[j] > 0.7) {
                size[j] <- "XL"
            }
            fff <- summary(lm(y ~ X[, j]))$fstatistic
            p.value[j] <- pf(q = fff[1], df1 = fff[2], df2 = fff[3], 
                lower.tail = FALSE)
        }
    }
    return(list(names = noms, RC = round(RC, 2), ES = round(ES, 
        2), size = size, p.value = round(p.value, 3)))
}

