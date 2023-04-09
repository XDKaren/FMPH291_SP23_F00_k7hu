

dparetoR<-function (x, alpha, beta, log = FALSE) 
{
  if (log == FALSE){
    return (beta*alpha^beta/x^(beta+1))
  } 
  else{
    (log(beta*alpha^beta/x^(beta+1)))
  }
  
}

