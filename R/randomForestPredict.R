#' @export
#' @title Predict with the Random Forest model
#' @description Use the Random Forest model trained with \code{randomForestTrain()} to
#' predict \code{newdata}.
#' @param model A Random Forest, as output from \code{randomForestTrain()}.
#' @param newdata The predictors for which to predict \code{newdata}.
#' @param method Controls how the prediction is performed. Set this parameter
#' as one of the following:
#' \itemize{
#' \item \code{NULL} (default) performs bagging using the parameter \code{bagging.function}.
#' \item \code{NA} or \code{"leaves"} returns all the training observations' values falling
#' on the leaves for each predictor.
#' \item \code{"random.sample"} draws a non generative random sample from the observations
#' falling on the leaves.
#' \item \code{"aposteriori"} uses a posteriori prediction with Moments Matching
#' Estimation (MME). This is the safest method for \code{fitdistrplus::fitdist()}.
#' \item The estimation method for \code{fitdistrplus::fitdist()}, i.e. one of the
#' following: \code{"mle"} for Maximum Likelihood Estimation; \code{"mme"} for
#' Moments Matching Estimation; \code{"qme"} for Quantile Matching Estimation;
#' \code{mge} for Maximum Goodness-of-fit Estimation; \code{mse} for
#' Maximum Spacing Estimation.
#' }
#'
#' @param bagging.function The bagging (aggregation) function if bagging is used,
#'  using \code{method = NULL}. Otherwise ignored.
#'  Use \code{NA} to return the whole set of predictions for all the trees in the forest.
#' @param distr Only used if using aposteriori estimation
#' (\code{method = "aposteriori", "mle", ...}. The probability distribution to be estimated,
#' parameter passed to \code{fitdistrplus::fitdist()}. Will be automatically infered if set
#' to \code{NULL}.
#' @param simplify.estimation Only used if using aposteriori estimation. Set to \code{FALSE}
#' to return the whole output from \code{fitdistrplus::fitdist()}.
#' @param ... Either optional arguments for the bagging function or additional arguments
#' to \code{fitdistrplus::fitdist()}


randomForestPredict <- function(model, newdata,
                                method = NULL,
                                bagging.function = mean,
                                distr = NULL,
                                simplify.estimation = TRUE,
                                ...){

  if (model[[1]]$method == "class") prediction.type = "prob"
  else prediction.type <- "matrix"

  if (is.null(dim(newdata))){
    newdata <- matrix(newdata, nrow = length(newdata))
    newdata <- data.frame(newdata)
    names(newdata) <- "x"
  } else{
    if (!is.data.frame(newdata)) newdata <- data.frame(newdata)
  }

  split.function <- model[[1]]$method

  if (is.null(method)){
    tbr <- simplify2array(lapply(model,
                                 FUN = function(md) predict(md,
                                                            newdata = newdata,
                                                            type = prediction.type))
    )
    if (!is.function(bagging.function) && is.na(bagging.function)) {}
    else {
      tbr <- apply(X = tbr,
                   MARGIN = seq(1, length(dim(tbr))-1),
                   FUN = bagging.function, ... = ...)
    }
  } else {
    if (is.na(method) || method == "leaves") tbr <- predictLeaves(model, newdata)
    else {
      tbr <- aposterioriEstimation(model = model,
                                   newdata = newdata,
                                   method = method,
                                   split.function = split.function,
                                   distr = distr,
                                   simplify.estimation = simplify.estimation,
                                   ... = ...)
    }
  }

  class(tbr) <- "RandomForest2.prediction"
  attr(tbr, "split.function") <- split.function

  return(tbr)
}
