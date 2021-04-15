#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector estimateGammaBC3(NumericVector y) {

  double sumy = 0;

  double aux, alpha, beta;
  int n = y.length();

  sumy = sum(y);
  aux = n*sum(y*log(y)) - sumy*sum(log(y));
  alpha = n*sumy/aux;
  alpha = alpha - ( 3*alpha - (2*alpha)/( 3*(1+alpha) ) -
    (4*alpha)/( 5*std::pow(1+alpha, 2) ) )/n;

  beta = n*alpha/sumy;

  return NumericVector::create(Named("shape", alpha), Named("rate", beta));
}
