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
#' \item If \code{distr = "gamma"}, \code{"bc3"} uses bc3 estimators, as defined in
#' Louzada, F. et al: A Note on Bias of Closed-Form Estimators for the Gamma Distribution
#' Derived From Likelihood Equations, doi: 10.1080/00031305.2018.1513376
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


randomForestPredict <- function(model,
                                newdata,
                                method = NULL,
                                bagging.function = mean,
                                distr = NULL,
                                simplify.estimation = TRUE,
                                non.informative.threshold = 0,
                                non.informative.p = 0.5,
                                marginal.imaginary.samplesize = 1,
                                also.return.x = FALSE,
                                parallel.plan = NA,
                                workers,
                                ...){


  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)
  ) {
    lapply.opt <- "future_lapply"
    if (missing(parallel.plan) || is.null(parallel.plan)){
      parallel.plan <- future::plan()
    } else if (is.character(parallel.plan) && parallel.plan == "auto") {
      parallel.plan <- future::plan(future::multisession)
    } else {
      if (!is.list(parallel.plan) &&
          !is.function(parallel.plan) &&
          is.na(parallel.plan)) lapply.opt <- "lapply"
      else{
        o.plan <- future::plan()
        if (missing(workers)) future::plan(parallel.plan)
        else future::plan(parallel.plan,
                          workers = if (is.null(workers) || workers == 0) (availableCores()) else workers
        )
        on.exit(future::plan(o.plan), add = TRUE)
      }
    }
  } else { # package future or future.apply not available
    lapply.opt <- "lapply"
  }




    if (lapply.opt == "future_lapply"){
      intervals <- splitIntervals(length.indices = nrow(newdata),
              chunks = if (missing(workers) || is.null(workers) || workers == 0) (availableCores()) else workers
              )
      tbr <-
        future.apply::future_lapply(future.packages = "RandomForestDist",
                                    future.seed = TRUE,
                                    future.stdout = FALSE,
                                    X = intervals,
                                    FUN = function(int){
                                      if (is.null(dim(newdata))){
                                        nd <- newdata[int]
                                      } else {
                                        nd <- newdata[int, , drop = F]
                                      }
                                      return(
                                      randomForestPredict(model,
                                                          newdata = nd,
                                                          method = method,
                                                          bagging.function = bagging.function,
                                                          distr = distr,
                                                          simplify.estimation = simplify.estimation,
                                                          non.informative.threshold = non.informative.threshold,
                                                          non.informative.p = non.informative.p,
                                                  marginal.imaginary.samplesize = marginal.imaginary.samplesize,
                                                          also.return.x = also.return.x,
                                                          parallel.plan = NA
                                                          )
                                      )
                      })
      if (!is.null(dim(tbr[[1]]))){
        return( do.call(rbind, tbr) )
      } else {
        return( do.call(c, tbr) )
      }

    } else {

      if (is.null(dim(newdata))){
        newdata <- data.frame(x = as.vector(newdata))
      } else{
        if (!is.data.frame(newdata)) newdata <- data.frame(x = I(unname(newdata)))
      }


      split.function <- model[[1]]$method

      if (split.function == "class") prediction.type = "prob"
      else prediction.type <- "matrix"




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

          if (split.function == "binaryMultiEntropyCond" ||
              split.function == "binaryMargEntropyCond"){
            colnames(tbr) <- getBinaryMultiEntropyCondColnames(model[[1]]$parms)
          }
        }
      } else {
        if (is.na(method) || method == "leaves") tbr <- predictLeaves(model, newdata, also.return.x)
        else {
          tbr <- aposterioriEstimation(model = model,
                                       newdata = newdata,
                                       method = method,
                                       split.function = split.function,
                                       distr = distr,
                                       simplify.estimation = simplify.estimation,
                                    non.informative.threshold = non.informative.threshold,
                                    non.informative.p = non.informative.p,
                          marginal.imaginary.samplesize = marginal.imaginary.samplesize)
        }
      }

      attr(tbr, "split.function") <- split.function

      class(tbr) <- "RandomForestDist.prediction"
      if ( isSimulable(method, split.function) ){
        class(tbr) <- "RandomForestDist.prediction.simulable"
      }
    }

    return(tbr)
}




splitIntervals <- function(length.indices, chunks = 1){
  if (chunks > 2){
    cut_size <- floor(length.indices/chunks)

    intervals <-
      c(
        list( 1:cut_size ),
        lapply(2:(chunks-1), function(icut){
          return(
            (cut_size*(icut-1) + 1):(cut_size*icut)
          )
        }),
        list( (cut_size*(chunks-1) + 1):(length.indices) )
      )
  } else {
    intervals <- list(1:length.indices)
  }

  return(intervals)
}




isSimulable <- function(method, split.function){
  if (split.function == "binaryMultiEntropyCond" ||
      split.function == "binaryMargEntropyCond"){
    condition_ <- is.null(method) || (
      !is.na(method) && (method != "leaves" && method != "random.sample")
    )
  }
  else if (split.function == "class"){
    condition_ <- is.null(method) || (
                    !is.na(method) && (method != "leaves" && method != "random.sample")
                  )
  }
  else{
    condition_ <- ( !is.null(method) && !is.na(method) && (method != "leaves") &&
                      (method != "random.sample") )
  }

  return(condition_)
}
