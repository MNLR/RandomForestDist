#' @importFrom qmap startberngamma
#' @importFrom fitdistrplus fitdist

aposterioriEstimation <- function(model, newdata, method, split.function, distr = NULL,
                                  simplify.estimation = TRUE,
                                  non.informative.threshold = 0,
                                  non.informative.p = 0.5,
                                  marginal.imaginary.samplesize = 1
                                  ){

  if (method == "random.sample"){
    prl <- predictLeaves(model, newdata)
    if (method == "binaryMultiEntropyCond" ||
        method == "binaryMargEntropyCond"){
      tbr <- do.call(rbind,
                     lapply(prl, FUN = function(pp){
                       pp[sample(1:nrow(pp), size = 1), ]
                     })
      )
    } else {
      tbr <- sapply(prl, FUN = sample, size = 1)
    }
  } else {
    idxS <- 1:nrow(newdata)

    if (split.function == "binaryMultiEntropyCond" ||
        split.function == "binaryMargEntropyCond"){
      if (method == "aposteriori") method <- "mme"

      prl <- predictLeaves(model, newdata)
      tbr <- aposterioriBinaryMultiEntropiCond(prl, method,
                                non.informative.threshold = non.informative.threshold,
                                non.informative.p = non.informative.p,
                      marginal.imaginary.samplesize = marginal.imaginary.samplesize)

    } else if (split.function == "bernoulliGammaLLMME" ||
               split.function == "binaryCrossEntropyGammaDeviation"){

      if (method == "aposteriori") method <- "mme"
      # generally speaking this needs to be done sample-by-sample
      # since predictive sample sizes tend to be big

      if (method == "mme" || method == "bc3"){ ## mme is currently deactivated
        tbr <-
          do.call(rbind,
                  lapply(1:nrow(newdata), function(indl){
                    return(
                      .Call(`_RandomForestDist_estimateBernoulliGammabc3`,
                            randomForestPredict(model,
                                                newdata = newdata[indl, , drop = F],
                                                method = "leaves")[[1]],
                            1)
                    )
                  })
          )
      } else {stop("Invalid method")}
    } else if (split.function == "MSEbinaryEntropyGammaDeviance"){
      if (method == "aposteriori"){ method <- "mme.bc3" }

      aux <- randomForestPredict(model,
                                 newdata = newdata,
                                 method = "leaves")

      method <- strsplit(x = method, split = ".", fixed = T)[[1]]

      tbr <-
        do.call(rbind,
                lapply(aux, FUN = function(ll) fitdistrplus::fitdist(data = ll[,1],
                                                                     distr = "norm",
                                                                     method = method[1])$estimate)
        )

      if (method[2] == "mme" || method[2] == "bc3"){ ## mme is currently deactivated
        tbr <-
          cbind(tbr,
                do.call(rbind,
                        lapply(aux, function(ll){
                          return(
                            .Call(`_RandomForestDist_estimateBernoulliGammabc3`,
                                  ll[,2],
                                  1)
                          )
                        })
                )
          )
      } else { stop("Invalid method") }
    } else if (split.function == "class"){
      if (method == "aposteriori") method <- "mme"

      tbr <- t(
              apply(predictLeaves(model, newdata),
                    MARGIN = 1,
                    FUN = function(ll) ll/sum(ll))
              )
    } else if (split.function == "bernoulliLL"){
      if (method == "aposteriori") method <- "mme"

      tbr <- predictLeaves(model, newdata)
    } else {
      if (method == "aposteriori") method <- "mme"

      if (is.null(distr)) distr <- guessDistribution(split.function)

      prl <- predictLeaves(model, newdata)

      if (method != "bc3"){
        tbr <-
          lapply(prl, function(ls){
            fitdistrplus::fitdist( data = ls, distr = distr, method = method )
          })
      } else {
        if (distr != "gamma") stop("bc3 estimators are for the gamma distribution")
        tbr <-
          estimateLeavesBC3(
            leaves.info = randomForestPredict(model,
                                              method = "leaves",
                                              newdata = newdata),
            noninformative.beta = 1
          )
        simplify.estimation <- FALSE
      }
      if (simplify.estimation){
        tbr <- lapply(tbr, function(ls) ls$estimate)
        if (length(tbr[[1]]) == 1) tbr <- do.call(c, tbr)
        else tbr <- do.call(rbind, tbr)
      }
    }
  }

  return(tbr)
}
