#' @export
#' @title Simulate stochastic series with the Random Forest model predictions.
#' @description Use the predictions output by \code{randomForestPredict()} to simulate
#' \code{n} stochastic series.
#' @param prediction A prediction object, as output by \code{.randomForestPredict()}.
#' It needs the parameter \code{simplify.estimation} to be set as \code{TRUE}
#' \code{randomForestPredict()}.
#' @param n How many series to simulate?
#' @param distr The probability distribution. Leave empty or set to \code{NULL} for
#'  automatically selecting the distribution. Currently supported distributions are the
#'  bernoulli distribution, \code{"bernoulli"}; the normal distribution, \code{"norm"};
#'  the gamma distribution, \code{"gamma"}; and the exponential distribution, \code{"exp"}.


randomForestSimulate <- function(prediction, n = 1, distr, simplify.multivariable = TRUE){
  if (class(prediction) != "RandomForestDist.prediction.simulable")
    stop("Expects an object of type RandomForestDist.prediction.simulable")

  if (missing(distr) || is.null(distr))
    distr <- guessDistribution(attr(prediction, "split.function"))

  if (distr == "conditionalBernoulli"){
    simulations <- lapply(seq(from = 1, to = n, by = 1),
                          function(nsim){
                            .Call(`_RandomForestDist_simulateDist`, -1, prediction, distr)
                          } )

    if (simplify.multivariable) simulations <- simplify2array(simulations)
  } else{
    simulations <- .Call(`_RandomForestDist_simulateDist`, n, prediction, distr)
  }

  if (distr == "categorical") simulations <- renameClasses(simulations,
                                                           colnames(prediction))

  return(simulations)
}


renameClasses <- function(simulations, class.names){
  for (i in 1:length(class.names)){
    simulations[which( simulations == (i-1) )] <- class.names[i]
  }
  return(simulations)
}
