#include <math.h>
// [[Rcpp::export]]
double paretodens(double x, double alpha, double beta, bool use_log=false){
  double res = (beta*pow(alpha,beta)/pow(x,beta+1)); 
  if (use_log == false){
    return res;
  } else {
    return log(res);
  }
}
  
  
  