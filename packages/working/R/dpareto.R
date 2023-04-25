dpareto = function(x, alpha, beta, log=FALSE){
  result =  .dpareto(x, alpha, beta, log)
  
  if (anyNA(result)){warning("NaN")}
  
  return (result)
}

