#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector estimateBernoulliGammabc3(NumericVector y, double non_informative_beta){

  double aux, alpha, beta;
  int p = 0;
  int n = y.length();
  double sumy = 0;
  double sumlogy = 0;
  double sumylogy = 0;
  int where_last_positive = 0;
  NumericVector positive_values;

  for (int i=0; i < y.length(); i++){
    if (y[i] > 0){
      p++;
      sumy += y[i];
      sumlogy += log(y[i]);
      sumylogy += y[i]*log(y[i]);

      y[p-1] = y[i];
    }
  }


  if (p == 0){
    return NumericVector::create(Named("p", 0), Named("shape", 0), Named("rate", 0));
  } else if (p == 1){
    return NumericVector::create(Named( "p", p/( (double)n) ),
                                 Named("shape", y[0]),
                                 Named("rate", non_informative_beta));
  } else {
    y = y[Range(0, p-1)];
  }

  if (p < 3 || unique(y).length() < 2 ){
    return NumericVector::create(Named( "p", p/( (double)n) ),
                                 Named("shape", sumy/p),
                                 Named("rate", non_informative_beta));

  } else {
    aux = p*sumylogy - sumy*sumlogy;
    alpha = p*sumy/aux;
    alpha =
      alpha - ( 3*alpha - (2*alpha)/( 3*(1+alpha) ) - (4*alpha)/( 5*std::pow(1+alpha, 2) ) )/p;

    beta = p*alpha/sumy;

    return NumericVector::create(Named("p", p/( (double)n) ),
                                 Named("shape", alpha),
                                 Named("rate", beta));

  }

}

