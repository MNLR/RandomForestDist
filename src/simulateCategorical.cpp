#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateCategorical(int n, NumericMatrix prediction) {
  NumericMatrix simulations(prediction.nrow(), n);

  NumericVector valnames(prediction.ncol());
  for (int i=0; i < valnames.length(); i++) valnames(i) = i;

  for (int i = 0; i < prediction.nrow(); i++){
     simulations.row(i) = sample(valnames, n, true, (NumericVector)prediction.row(i));
  }

  return simulations;
}
