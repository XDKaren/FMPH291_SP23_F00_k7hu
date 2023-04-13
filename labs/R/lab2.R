## File: lab2.R
## Author: Keren Hu
## Date: 4/13/2023
## Purpose: FMPH 291 Statistical Computing using R - lab 2 exercises

# Hypotenuse Length Calculation

hypot1 <- function(x,y) sqrt(x ^ 2 + y ^ 2)

hypot1(0, 0) # output is zero
hypot1(6, 8) # output is 10
hypot1(1:10, 20) 

hypot1(3e-, 20)

hypot1(3e300, 4e300) # output is Inf (infinite)

hypot1(3e-163, 4e-163) # output is zero

hypot2 <- function(x,y){
  
  minv=pmin(abs(x),abs(y))
  init_result <- minv * (hypot1(x/minv, y/minv))
  
  final_result <- ifelse(pmax(abs(x),abs(y))==0,0, init_result)
  return (final_result)
}


# hypot2 <- function(x,y){
#   init_result <- pmax(abs(x), abs(y))*sqrt(1+(pmin(abs(x), abs(y))))
#   
#   final_result <- ifelse(pmax(abs(x),abs(y))==0, 0, init_result)
#   return (final_result) 
# }

hypot2(3e300, 4e300)
hypot2(3e-161, 4e-161)

hypot2( 3 * (0:10), -4 * (0:10))

all.equal(hypot1(3 * (0:10), -4 * (0:10)),hypot2( 3 * (0:10), -4 * (0:10)))

# Logspace Computations
logSumExp1 <- function(x,y) log( exp( x ) + exp( y ))

logSumExp1(1, 1)

logSumExp1(100, 100)

bigButFinite <- log(.Machine$double.xmax) # .Machine$double.xmax=1.797693e+308

logSumExp1(bigButFinite-1, bigButFinite-1)

logSumExp1(bigButFinite, bigButFinite) # output is Inf, because this is the biggest number R can handle.

logSumExp2 <- function (x,y) {
  
  result <- pmax(x,y)+log(1+exp(pmin(x,y)-pmax(x,y)))
  return (result)
  
}

dat <- data.frame(x=c(1,1,1000,10,100, 2e50),
                  y=c(1,1000,1,20,200, 3e50))

do.call(logSumExp2, dat)
