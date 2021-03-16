#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateBernoulli(int n, NumericMatrix prediction) {
  NumericMatrix simulations(prediction.nrow(), n);

  for (int i = 0; i < prediction.nrow(); i++){
    simulations.row(i) = sample(NumericVector::create(0,1),
                                n,
                                true,
                                NumericVector::create(1-prediction(i, 0),
                                                      prediction(i, 0))
                                );
  }

  return simulations;
}
