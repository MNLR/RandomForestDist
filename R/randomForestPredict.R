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
#' @param ... Either optional arguments for the bagging function or additional arguments
#' to \code{fitdistrplus::fitdist()}


randomForestPredict <- function(model, newdata,
                                method = NULL,
                                bagging.function = mean,
                                parallel.plan = NULL,
                                workers = NULL,
                                ...){

  # ... optional arguments to bagging function
  # bagging function = NA -> return all trees' predictions
  # method = "leaves": Return all leaves' observed values
  # else argument to fitdistrplus::fitdist()
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
    if (!is.na(bagging.function)){
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
                                   ... = ...)
    }
  }

  return(tbr)
}
