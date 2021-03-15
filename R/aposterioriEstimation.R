#' @importFrom qmap startberngamma
#' @importFrom fitdistrplus fitdist

aposterioriEstimation <- function(model, newdata, method, split.function, distr = NULL,
                                  ... = ...){

  if (method == "random.sample"){
    prl <- predictLeaves(model, newdata)
    tbr <- sapply(prl, FUN = sample, size = 1)
  } else {

    if (method == "aposteriori") method <- "mme"
    idxS <- 1:nrow(newdata)
    if (split.function == "bernoulliGammaLLMME"){

      prl <- predictLeaves(model, newdata)
      tbr <- t(sapply(prl, FUN = function(ll) unlist(startberngamma(ll))))

    } else if (split.function == "class"){
      tbr <- t(
              apply(predictLeaves(model, newdata),
                    MARGIN = 1,
                    FUN = function(ll) ll/sum(ll))
              )
    } else if (split.function == "bernoulliLL"){
      tbr <- predictLeaves(model, newdata)
    } else {
      if (is.null(distr)){
        if (split.function == "gammaLLMME" ||
            split.function == "gammaLLmean" ||
            split.function == "gammaDeviation" ||
            split.function == "gammaLLBC3") distr <- "gamma"
        else if (split.function == "anova") distr <- "norm"
        else stop("Method not found and cannot infer distribution")
      }

      prl <- predictLeaves(model, newdata)
      tbr <-
        lapply(prl, function(ls){
          fitdist( data = ls, distr = distr, method = method )
        })
    }
  }

  return(tbr)
}
