guessDistribution <- function(split.function, prediction){

  distr <-
    switch(split.function,
              gammaLLBC3 = {"gamma"},
              gammaLLMME = {"gamma"},
              gammaLLmean = {"gamma"},
              gammaDeviation = {"gamma"},
              anova = {"norm"},
              class = {"categorical"},
              binaryMultiEntropyCond = {"conditionalBernoulli"},
              binaryMargEntropyCond = {"conditionalBernoulli"},
              {stop("Method not found and cannot infer distribution")}
          )

  return(distr)
}
