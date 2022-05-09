#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix simulateOneConditionalBernoulli(NumericMatrix prediction) {

  int nresponses, aux, idxcond, idxbasecond;

  nresponses = 0;
  aux = 0;
  while (aux < prediction.ncol()){
    aux = aux + pow(2, nresponses);
    nresponses++;
  }
  NumericMatrix simulations(prediction.nrow(), nresponses);

  NumericVector valnames = NumericVector::create(0, 1);


   for (int i = 0; i < prediction.nrow(); i++){
    // for nresp = 0:
    simulations(i,0) = sample(valnames, 1, true,
                              NumericVector::create(1-prediction(i, 0),
                                                     prediction(i, 0)))(0);
     // for nresp = rest:
    idxbasecond = 0;
    for (int nresp = 1; nresp < nresponses; nresp++){
      idxbasecond += ( pow(2, nresp - 1) );
      idxcond = 0;

      for (int ncond = 0; ncond < nresp; ncond++){
        idxcond += (int)(simulations(i,ncond)*( pow(2, ncond)) );
      }

      simulations(i,nresp) = sample(valnames, 1, true,
                          NumericVector::create(1 - prediction(i, idxbasecond + idxcond),
                                                prediction(i, idxbasecond + idxcond)))(0);
    }
  }

  return simulations;
}
