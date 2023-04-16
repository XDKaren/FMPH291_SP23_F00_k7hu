

# dparetoR <- function(x, alpha, beta) {
#   
#   if (x < alpha) {
#     return(0)
#   } else {
#     return( beta * alpha^beta / x^(beta + 1))
#   }
# }

paretodens <- function(x, alpha, beta) {
  if (x < alpha) {
    return (0)
  } else {
    return(beta * alpha^beta / x^(beta + 1))
  }
}

dparetoR <- function(x, alpha, beta, logd = FALSE) {
  xn <- length(x)
  alphan <- length(alpha)
  betan <- length(beta)
  
  if (xn == 0L || alphan == 0L || betan == 0L) {
    cat(sprintf("Zero length detected: xn = %d, alphan = %d, betan = %d\n", xn, alphan, betan))
  }
  
  n <- max(xn, alphan, betan)
  f <- numeric(n)
  
  ix <- 1
  ialpha <- 1
  ibeta <- 1
  for (i in 1:n) {
    if (logd == FALSE) {
      f[i] <- paretodens(x[ix], alpha[ialpha], beta[ibeta])
    } else {
      f[i] <- log(paretodens(x[ix], alpha[ialpha], beta[ibeta]))
    }
    
    ix <- ix + 1
    if (ix > xn) ix <- 1
    
    ialpha <- ialpha + 1
    if (ialpha > alphan) ialpha <- 1
    
    ibeta <- ibeta + 1
    if (ibeta > betan) ibeta <- 1
  }
  
  return(f)
}
