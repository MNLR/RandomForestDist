#include <Rcpp.h>
using namespace Rcpp;

// Consistency:
int checkConsistencyAndGetCode(NumericMatrix prediction, String distribution);

// Distributions:
NumericMatrix simulateBernoulli(int n, NumericMatrix prediction);
NumericMatrix simulateNormal(int n, NumericMatrix prediction);
NumericMatrix simulateGamma(int n, NumericMatrix prediction);
NumericMatrix simulateExponential(int n, NumericMatrix prediction);

// Distributions Configuration:
static std::map<String,int> distribution_parameters = {{"bernoulli",1},
                                                       {"norm",     2},
                                                       {"gamma",    2},
                                                       {"exp",      1}};
static std::map<String,int> distribution_codes = {{"bernoulli",0},
                                                  {"norm",     1},
                                                  {"gamma",    2},
                                                  {"exp",      3}};

// (beware of the order, code is the index)
