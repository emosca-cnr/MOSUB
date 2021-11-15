mn.page <- function(x = NULL, exprs = NULL) {
    
    mu <- mean(exprs)
    std <- sd(exprs)
    sm <- mean(exprs[names(exprs) %in% x])
    f <- (sm - mu) * sqrt(length(x)) / std
    f <- pnorm(-abs(f))
    
    return(f)
}