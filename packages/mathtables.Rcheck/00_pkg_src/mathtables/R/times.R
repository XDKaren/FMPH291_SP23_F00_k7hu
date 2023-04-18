## calculate x times y for integers
times <-
  function( x, y ) {
    res <- outer( as.integer( x ), as.integer( y ),
                  function( xx , yy )
                    as.integer( xx * yy ))
    dimnames(res) <-
      list( formatC( x, digits = 0, width = max( nchar( x ))),
            formatC( y, digits = 0, width = max( nchar( y ))))
    res
  }
# x,y are two integer. 
# outer() = outer product of arrays, x %*% t(y). By default, the outer funcion apply * on x and y.
