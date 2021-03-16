#' @importFrom qmap startberngamma
#' @importFrom fitdistrplus fitdist

aposterioriEstimation <- function(model, newdata, method, split.function, distr = NULL,
                                  simplify.estimation = TRUE,
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
      if (is.null(distr)) distr <- guessDistribution(split.function)

      prl <- predictLeaves(model, newdata)
      tbr <-
        lapply(prl, function(ls){
          fitdist( data = ls, distr = distr, method = method )
        })
      if (simplify.estimation){
        tbr <- lapply(tbr, function(ls) ls$estimate)
        if (length(tbr[[1]]) == 1) tbr <- do.call(c, tbr)
        else tbr <- do.call(rbind, tbr)
      }
    }
  }

  return(tbr)
}
