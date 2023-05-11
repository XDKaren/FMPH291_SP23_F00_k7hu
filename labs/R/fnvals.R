# The open.account example
fnvals <- function(values = list()){
  list(
    fn = function(x){
      values[[length(values)+1]] <<- x
      sum( ( x - 1:length(x) ) ^ 2 )
    },
    values = function() do.call (rbind, values)
  )
}

myfun = fnvals()

myfun$fn( c( 10, 12 ) ) # returns 181
myfun$fn( c( 2, 3 ) )   # returns 2
myfun$fn( c( 4, 5 ) )   # returns 18 


myfun$values()


# A self tracing function

optfun  <- fnvals()
optfun$fn(c(2,4)) # 5

optim(c(10,4), optfun$fn)
optim( c(2, 1), optfun$fn )

plot(optfun$values())

optfun$fn(c(1,2)) # 0


optfun2 <- fnvals()
optim( c(2, 1), optfun2$fn, method = "BFGS" )
optfun2$values() 
sweep( optfun2$values(), 2, 1:2, "-")
plot(optfun2$values() , type="b")

