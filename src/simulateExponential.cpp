#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateExponential(int n, NumericMatrix prediction) {
  NumericMatrix simulations(prediction.nrow(), n);

  for (int i = 0; i < prediction.nrow(); i++){
    simulations.row(i) = rexp(n,  prediction(i, 0));
  }

  return simulations;
}
