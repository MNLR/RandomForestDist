#include <Rcpp.h>
#include "simulate.h"
using namespace Rcpp;

int checkConsistencyAndGetCode(NumericMatrix prediction, String distribution){

  if (prediction.ncol() != distribution_parameters[distribution])
    stop("Number of parameters inconsistent with distribution code.");

  return distribution_codes[distribution];
}
