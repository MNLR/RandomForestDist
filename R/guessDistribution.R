guessDistribution <- function(split.function){
  if (split.function == "gammaLLMME" ||
      split.function == "gammaLLmean" ||
      split.function == "gammaDeviation" ||
      split.function == "gammaLLBC3") distr <- "gamma"
  else if (split.function == "anova") distr <- "norm"
  else stop("Method not found and cannot infer distribution")

  return(distr)
}
