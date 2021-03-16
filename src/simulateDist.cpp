#include <Rcpp.h>
#include "simulate.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix simulateDist(int n, NumericMatrix prediction, String distribution) {

  int code = checkConsistencyAndGetCode(prediction, distribution);
  NumericMatrix simulations(prediction.nrow(), n);

  switch(code) {
    case 0:
      simulations = simulateBernoulli(n, prediction);
      break;

    case 1:
      simulations = simulateNormal(n, prediction);
      break;

    case 2:
      simulations = simulateGamma(n, prediction);
      break;

    case 3:
      simulations = simulateExponential(n, prediction);
      break;

    default:
      stop("Internal error: distribution code not found");
      break;
  }

  return simulations;
}
