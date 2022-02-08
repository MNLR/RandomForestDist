#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector computeBinaryMultiEntropyConditional(NumericMatrix appr,
                                                   int non_informative_threshold = 0){

  int n = appr.nrow();
  int number_of_ys = appr.ncol();
  int i, varindex, idx, pos0;

  int number_of_responses = 0;
  for (int i = 0; i < number_of_ys; i++)
    number_of_responses += (int) pow((double)2, (double)i);

  NumericVector informed_frequencies(number_of_responses);
  NumericVector cond_informed_frequencies(number_of_responses);
  for (i = 0; i < number_of_responses; i++){
    informed_frequencies[i] = 0;
    cond_informed_frequencies[i] = 0;
  }

  for (i = 0; i < n; i++) {
    for (varindex = 0; varindex < number_of_ys; varindex++){
          if (varindex == 0){
            informed_frequencies[varindex] += appr(i, varindex); //
            cond_informed_frequencies[varindex] ++;
          } else {
            idx = 0;
            pos0 = 0;
            for (int k = 0; k < varindex; k++){
              pos0 += (int)pow((double)2, (double)k);
              idx += (int)pow((double)2, (double)k)*appr(i,k);
            }

            informed_frequencies[pos0 + idx] += appr(i, varindex);
            cond_informed_frequencies[pos0 + idx] ++;
          }
    }
  }

  for (i = 0; i < number_of_responses; i++){
    if (cond_informed_frequencies[i] <= non_informative_threshold)
      informed_frequencies[i] = -1;
    else informed_frequencies[i] = informed_frequencies[i]/cond_informed_frequencies[i];
  }

  return informed_frequencies;
}
