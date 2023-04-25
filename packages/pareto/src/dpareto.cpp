#include <Rcpp.h>
using namespace Rcpp;

double paretodens(double x, double alpha, double beta) {
  if (x < alpha) {
    return 0.0;
  } else {
    return beta * pow(alpha/x, beta) / x;
  }
}
// [[Rcpp::export(name = ".dpareto")]] 
NumericVector dpareto(NumericVector x, NumericVector alpha, NumericVector beta, bool logd = false) {
  int xn = x.size();
  int alphan = alpha.size();
  int betan = beta.size();
  
    
    
  if (xn == 0L || alphan == 0L || betan == 0L) {
    Rprintf("Zero length detected: xn = %d, alphan = %d, betan = %d\n", xn, alphan, betan);
  }
    
    
  int n = std::max(std::max(xn, alphan), betan);
  NumericVector f(n);
    
  int ix = 0, ialpha = 0, ibeta = 0;
  for (int i = 0; i < n; ++i) {
    
    if (logd == false) {
      f[i]=paretodens(x[ix], alpha[ialpha], beta[ibeta]);
    }
    

    else {
      f[i]=log(paretodens(x[ix], alpha[ialpha], beta[ibeta]));
    }
      
    if (++ix == xn) ix = 0;
    if (++ialpha == alphan) ialpha = 0;
    if (++ibeta == betan) ibeta = 0;
  }
    
  return f;
  }