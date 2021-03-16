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


randomForestSimulate <- function(prediction, n = 1, distr){
  if (class(prediction) != "RandomForest2.prediction")
    stop("prediction must be the output of randomForestPredict()")

  if (missing(distr) || is.null(distr))
    distr <- guessDistribution(attr(prediction, "split.function"))

  return( .Call(`_RandomForest2_simulateDist`, n, prediction, distr) )
}
