#include <Rcpp.h>
using namespace Rcpp;

// Consistency:
void checkConsistency(NumericMatrix prediction, String distribution);

// Distributions:
NumericMatrix simulateCategorical(int n, NumericMatrix prediction);
NumericMatrix simulateNormal(int n, NumericMatrix prediction);
NumericMatrix simulateGamma(int n, NumericMatrix prediction);
NumericMatrix simulateExponential(int n, NumericMatrix prediction);
NumericMatrix simulateOneConditionalBernoulli(NumericMatrix prediction);

// Distributions Configuration:
static std::map<String,int> distribution_parameters = {{"categorical", -1},  //use -1 to do not check
                                                       {"norm",     2},
                                                       {"gamma",    2},
                                                       {"exp",      1},
                                                       {"conditionalBernoulli", -1}};
static std::map<String,int> distribution_codes = {{"categorical", 0},
                                                  {"norm",        1},
                                                  {"gamma",       2},
                                                  {"exp",         3},
                                                  {"conditionalBernoulli", 4}};

// (beware of the order, code is the index)
