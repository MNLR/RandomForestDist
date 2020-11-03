#' @export

randomForestTrain <- function(x, y = NULL,
                             ntree = 500,
                             mtry = if (!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
                             minsplit = if (!is.null(y) && !is.factor(y)) 5 else 1,
                             minbucket = minsplit/3,
                             maxdepth = 30,
                             method,
                             replace = TRUE,
                             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                             parallel.plan,
                             workers,
                             weights,
                             parms){

  lapply.opt <- "future_lapply"

  if (missing(parallel.plan) || is.null(parallel.plan)) parallel.plan <- plan()
  else {
    if (is.na(parallel.plan)) lapply.opt <- "lapply"
    else{
      o.plan <- plan()
      if (missing(workers)) plan(parallel.plan)
      else plan(parallel.plan, workers = if (is.null(workers) || workers == 0) (availableCores()) else workers)
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


  mc[[1]] <- quote(rpart)
  mc$formula <- y ~ .
  mc$x <- NULL
  mc$y <- NULL
  mc$mtry <- mtry
  mc$minsplit <- minsplit
  mc$minbucket <- minbucket
  mc$cp <- -Inf  # Ensures negative nll values are handled correctly
  mc$maxdepth <- maxdepth
  mc$xval <- 0
  if (!missing(method)) {
    mc$method <- method
  }
  if (!missing(weights)) mc$weights <- weights
  if (!missing(parms)) mc$parms <- parms

  if (is.null(dim(x))) x <- matrix(x)
  nrx <- nrow(x)


  idxS <- 1:ntree
  with_progress({
    p <- progressor(along = idxS)
    if (lapply.opt == "future_lapply") {
      rf <- future_lapply(future.packages = "rpart", future.seed = T,
                          X = idxS, FUN = function(idxt){
                            p()

                            sid <- sample(1:nrx, size = sampsize, replace = replace)
                            x <- x[sid, ]
                            if (!is.null(dim(y))) y[sid, ] else y <- y[sid]
                            mc$data <- data.frame(y, x)

                            tree <- eval(mc)
                            mc <- NULL
                            tree$where <- NULL
                            tree$y <- NULL

                            return(tree)
                          })
    } else {
      rf <- lapply(X = idxS, FUN = function(idxt){
        p()

        sid <- sample(1:nrx, size = sampsize, replace = replace)
        x <- x[sid, ]
        if (!is.null(dim(y))) y[sid, ] else y <- y[sid]
          mc$data <- data.frame(y, x)

          tree <- eval(mc)
          mc <- NULL
          tree$where <- NULL
          tree$y <- NULL

          return(tree)
        })
    }

  })

  # if (!missing(method))  attr(rf, "split.method") <- method else attr(rf, "anova")

  return(rf)
}
