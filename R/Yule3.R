Yule3<-
function (df, levX, varX, varYYY) 
{
    L <- length(varYYY)
    if (is.factor(df[, varYYY[1]])) {
        T1 <- Yule1(df, levX, varX, varYYY[1])
    }
    else {
        T1 <- Yule2(df, levX, varX, varYYY[1])
    }
    if (L > 1) {
        for (l in 2:L) {
            if (is.factor(df[, varYYY[l]])) {
                T2 <- Yule1(df, levX, varX, varYYY[l])
            }
            else {
                T2 <- Yule2(df, levX, varX, varYYY[l])
            }
            T1 <- rbind(T1, T2)
        }
    }
    T1
}