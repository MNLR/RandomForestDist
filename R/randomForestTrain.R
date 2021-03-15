#' @export
#' @title Train Random Forest model
#' @description Train a random forest for predictors \code{x} and predictands \code{y}.
#' For each tree a bootstrap sample is drawn from \code{x}. By default these samples are
#' drawn with replacement (\code{replace = TRUE}), and have size \code{.632*nrow(x)}.
#' @param x The predictors, in matrix form.
#' @param y The predictands, a vector.
#' @param ntree Number of trees.
#' @param mtry Number of predictors randomly used as candidate split.
#' Default is \code{sqrt(ncol(x))} for classification and \code{ncol(x)/3} for
#' regression
#' @param minsplit Minimum number of elements to attempt a split.
#' @param minbucket Minimum terminal leaf size, i.e. the minimum number of observations
#' that must be present on a terminal leaf.
#' @param maxdepth Maximum depth of the trees.
#' @param method The split function.
#' @param replace Either \code{TRUE} or \code{FALSE}. Should samples of \code{x} be
#' drawn with replacement?
#' @param sampsize Size of the samples of \code{x}
#' @param parallel.plan Controls parallel execution, which is handled by and requires the
#' package \code{future.apply}. If this package is not installed parallel execution will
#'  not be used. If left missing or set to \code{NULL} (default), the function uses the
#' current \code{future::plan()} of the session. Set this parameter to \code{"auto"} for
#' automatic parallelization, which avoids dealing with futures outside the function.
#' In any other case this parameter corresponds to
#' \code{futute::plan(strategy = parallel.plan)}, check their corresponding help pages.
#' \code{NA} avoids the use of \code{future.apply::future_lapply()}.
#' @param workers The number of workers. By default uses the maximum available cores.
#' @param weights \code{weights} as pased to \code{rpart::rpart()}.
#' @param parms \code{parms} as pased to \code{rpart::rpart()}.
#' @param remove.leaf.info Reduces the memory usage of the trees. This should not be
#' set to \code{TRUE} unless there are memory issues. NOTE: Setting this to \code{TRUE}
#' makes a posteriori estimation not available.

randomForestTrain <- function(x, y = NULL,
                              ntree = 100,
                              mtry = if (!is.null(y) && !is.factor(y))
                                        max(floor(ncol(x)/3), 1)
                                     else floor(sqrt(ncol(x))),
                              minsplit = if (!is.null(y) && !is.factor(y)) 5 else 1,
                              minbucket = minsplit/3,
                              maxdepth = 30,
                              method,
                              replace = TRUE,
                              sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                              parallel.plan,
                              workers,
                              weights,
                              parms,
                              remove.leaf.info = FALSE){



  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.apply", quietly = TRUE)
      ) {
    lapply.opt <- "future_lapply"
    if (missing(parallel.plan) || is.null(parallel.plan)){
      parallel.plan <- future::plan()
    }
    else if (is.character(parallel.plan) && parallel.plan == "auto") {
      parallel.plan <- future::plan(future::multisession)
    }
    else {
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


  stopifnot(sum(is.na(y)) == 0)
  stopifnot(sum(is.na(x)) == 0)

  mc <- match.call()

  # mandatory, otherwise passed to rpart:
  mc$ntree <- NULL
  mc$workers <- NULL
  mc$replace <- NULL
  mc$sampsize <- NULL
  mc$parallel.plan <- NULL
  mc$remove.leaf.info <- NULL


  mc[[1]] <- quote(rpart)
  mc$formula <- y ~ .
  mc$x <- NULL
  mc$y <- NULL
  mc$mtry <- mtry
  mc$minsplit <- minsplit
  mc$minbucket <- minbucket
  mc$cp <- -Inf  # RFs do not regularize - Ensures negative nll values don't conflict
  mc$maxdepth <- maxdepth
  mc$xval <- 0

  if (!missing(method)) mc$method <- method
  if (!missing(weights)) mc$weights <- weights
  if (!missing(parms)) mc$parms <- parms

  if (is.null(dim(x))) x <- matrix(x, nrow = length(x))
  nrx <- nrow(x)


  idxS <- 1:ntree
  with_progress({
    p <- progressor(along = idxS)
    if (lapply.opt == "future_lapply") {
      rf <- future.apply::future_lapply(future.packages = "rpart",
                                        future.seed = T, future.stdout = NA,
                                        X = idxS, FUN = function(idxt){

                            sid <- sample(1:nrx, size = sampsize, replace = replace)
                            x <- x[sid, ]
                            if (!is.null(dim(y))) y[sid, ] else y <- y[sid]
                            mc$data <- data.frame(y, x)

                            tree <- eval(mc)
                            mc <- NULL
                            if (remove.leaf.info){
                              tree$where <- NULL
                              tree$y <- NULL
                            }

                            p(message = sprintf("Tree %g/%g", idxt, ntree))

                            return(tree)
                          })
    } else if (lapply.opt == "lapply"){
      rf <- lapply(X = idxS, FUN = function(idxt){

        sid <- sample(1:nrx, size = sampsize, replace = replace)
        x <- x[sid, ]
        if (!is.null(dim(y))) y[sid, ] else y <- y[sid]
          mc$data <- data.frame(y, x)

          tree <- eval(mc)
          mc <- NULL
          if (remove.leaf.info){
            tree$where <- NULL
            tree$y <- NULL
          }

          p(message = sprintf("Tree %g/%g", idxt, ntree))

          return(tree)
        })
    } else {stop("Internal Error: lapply.opt not set")}

  })

  class(rf) <- "RandomForest"

  return(rf)
}
