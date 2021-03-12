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
#' @param parallel.plan Controls parallel execution, which is handled by package
#' \code{future.apply}. The default value, \code{NA}, does not use parallel execution. If
#' set to \code{NULL}, retains the \code{plan()} of the session. Otherwise this parameter
#' corresponds to \code{plan(strategy = parallel.plan)}. Set it to \code{multisession} for
#' parallel execution and to \code{sequential} to override plan to sequential.
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

  lapply.opt <- "future_lapply"

  if (missing(parallel.plan) || is.null(parallel.plan)) parallel.plan <- plan()
  else {
    if (is.na(parallel.plan)) lapply.opt <- "lapply"
    else{
      o.plan <- plan()
      if (missing(workers)) plan(parallel.plan)
      else plan(parallel.plan,
                workers = if (is.null(workers) || workers == 0) (availableCores())
                          else workers
                )
      on.exit(plan(o.plan), add = TRUE)
    }
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
      rf <- future_lapply(future.packages = "rpart",
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
    } else {
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
    }

  })

  return(rf)
}
