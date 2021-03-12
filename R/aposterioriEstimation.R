aposterioriEstimation <- function(model, newdata, method, split.function,
                                  ... = ...){

  if (method == "random.sample"){
    tbr <-
      sapply(1:nrow(newdata), function(isamp){
        sample(size = 1,
               x = predictLeaves(model, matrix(newdata[isamp, ], nrow = 1))[[1]]
               )
      })
  } else {

    if (method == "aposteriori") method <- "mme"
    idxS <- 1:nrow(newdata)
    if (split.function == "bernoulliGammaLLMME"){
      with_progress({
        p <- progressor(along = idxS)
        tbr <-  future_lapply(X = idxS, FUN = function(isamp) {
          aux <- unlist(startberngamma(
            predictLeaves( model, matrix(newdata[isamp, ], nrow = 1) )[[1]]
          ))
          p()
          return(
            c(aux[c("prob", "shape")], rate = 1/unname(aux["scale"]))
          )
        })
      })
    } else if (split.function == "class"){
      tbr <- t(
              apply(predictLeaves(model, newdata),
                    MARGIN = 1, FUN = function(ll) ll/sum(ll))
              )
    } else if (split.function == "bernoulliLL"){
      tbr <- predictLeaves(model, newdata)
    } else {
      if (split.function == "gammaLLMME" ||
          split.function == "gammaLLmean" ||
          split.function == "gammaDeviation" ||
          split.function == "gammaLLBC3") distr <- "gamma"
      else if (split.function == "anova") distr <- "normal"
      else stop("Method not found, cannot infer distribution")
      with_progress({
        p <- progressor(along = idxS)
        tbr <-
          future_lapply(X = idxS, FUN = function(isamp) {
            fitdist( predictLeaves(model,
                                   newdata = data.frame(
                                     matrix(newdata[isamp, ], nrow = 1)
                                   )
                                   )[[1]],
                     distr = distr,
                     method = method
            )
            p()
          })
      })
    }
  }

  return(tbr)
}
