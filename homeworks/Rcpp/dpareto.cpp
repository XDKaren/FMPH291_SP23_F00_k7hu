#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double paretodens(double x, double alpha, double beta) {
  if (x < beta) {
    return 0.0;
  } else {
    return alpha * pow(beta, alpha) / pow(x, alpha + 1.0);
  }
}
  

// [[Rcpp::export]]
NumericVector dpareto(NumericVector x, NumericVector alpha, NumericVector beta, bool logd = false) {
  int n = x.size();
  NumericVector f(n);
    
  for (int i = 0; i < n; ++i) {
    if (x[i] < beta[i]) {
      f[i] = 0.0;
    } else {
      if (logd == true) {
        f[i] = log(paretodens(x[i], alpha[i], beta[i]));
      } else {
        f[i] = paretodens(x[i], alpha[i], beta[i]);
      }
    }
  }
    
  return f;
}
  