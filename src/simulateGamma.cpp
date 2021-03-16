#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateGamma(int n, NumericMatrix prediction) {
  NumericMatrix simulations(prediction.nrow(), n);

  for (int i = 0; i < prediction.nrow(); i++){
    simulations.row(i) = rgamma(n,  prediction(i, 0),  1/prediction(i, 1));
  }

  return simulations;
}
