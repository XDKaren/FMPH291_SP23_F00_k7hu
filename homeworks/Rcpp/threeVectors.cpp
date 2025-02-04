#include <Rcpp.h>
using namespace Rcpp;
// Demonstrate how to `Vectorize' 2 arguments.
// Print the indexes and the element indexed.
// `void' type needs no `return' statement
// Rprintf is described in Writing R Extensions Section 6.5 Printing
void print2Values( int i, int ix, int iy, double xelt, double yelt) {
  Rprintf("i = %d ix = %d x element = %f iy = %d y element = %f\n",
          i, ix, xelt, iy, yelt);
}
// [[Rcpp::export]]
CharacterVector twoVectors(
    NumericVector x, NumericVector y) {
  int xn = x.size();
  int yn = y.size();
  // if (xn == 0 || yn == 0) special handling is needed such as
  // returning a zero length vector
  if (xn==0L || yn == 0L)
    Rprintf(" Zero Length Detected: xn == %d, yn ==%d\n", xn, yn);
  int n = xn > yn ? xn : yn;

  int ix=0, iy=0;
  for (int i = 0; i<n; ++i) {
    // Do something here:
    print2Values( i, ix, iy, x[ix], y[iy] );
    // Update the indexes for the next pass:
    if (++ix == xn ) ix = 0 ;
    if (++iy == yn ) iy = 0;
  }
  return "Done";
}
// /*** R
// twoVectors(1:2,3:7)
// */



#include <Rcpp.h>
using namespace Rcpp;

// A function to print 3 input values
void print3Values(int i, int ix, int iy, int iz,
                  double xelt, double yelt, double zelt) {
  Rprintf("i = %d ix = %d x element = %f iy = %d y element = %f iz = %d z element = %f\n",
          i, ix, xelt, iy, yelt, iz, zelt);
}

// A function to recycle 3 input vectors and vectorize 3 arguments
// [[Rcpp::export]]
CharacterVector threeVectors(NumericVector x, NumericVector y, NumericVector z) {
  int xn = x.size();
  int yn = y.size();
  int zn = z.size();
  
  
  if (xn == 0L || yn == 0L || zn == 0L) {
    Rprintf("Zero length detected: xn = %d, yn = %d, zn = %d\n", xn, yn, zn);
  }
  

  int n = std::max(std::max(xn, yn), zn);
  
  int ix = 0, iy = 0, iz = 0;
  for (int i = 0; i < n; ++i) {
    

    print3Values(i, ix, iy, iz, x[ix], y[iy], z[iz]);
    
    
    if (++ix == xn) ix = 0;
    if (++iy == yn) iy = 0;
    if (++iz == zn) iz = 0;
  }
  
  return "Done";
}


