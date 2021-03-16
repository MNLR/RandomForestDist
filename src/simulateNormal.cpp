#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateNormal(int n, NumericMatrix prediction) {
  NumericMatrix simulations(prediction.nrow(), n);

  for (int i = 0; i < prediction.nrow(); i++){
    simulations.row(i) = rnorm(n,  prediction(i, 0),  prediction(i, 1));
  }

  return simulations;
}
