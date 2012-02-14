Yule4<-
function (df, levX, varX, varYYY, alpha = 0.1) 
{
    T1 <- Yule3(df, levX, varX, varYYY)
    T2 <- T1[T1[, 2] > 0, ]
    T3 <- T2[order(T2[, 2]), ]
    T4 <- T3[T3[, 4] < alpha, ]
    L <- nrow(T4)
    MIN <- min(0, T4[, 2] - 1.96 * T4[, 3])
    MAX <- max(1, T4[, 2] + 1.96 * T4[, 3])
    titre <- paste(varX, levX, sep = "=")
    dotchart(T4[, 2], labels = rownames(T4), xlim = c(MIN, MAX), 
        main = titre)
    abline(v = 0, lty = 2)
    segments(T4[, 2] - 1.96 * T4[, 3], 1:L, T4[, 2] + 1.96 * 
        T4[, 3], 1:L)
}
