#include <Rcpp.h>
#include "simulate.h"
using namespace Rcpp;

void checkConsistency(NumericMatrix prediction, String distribution){
  int nparameters = distribution_parameters[distribution];
  if (nparameters < 0) return;
    if (prediction.ncol() != nparameters)
      stop("Number of parameters inconsistent with distribution code.");
}
