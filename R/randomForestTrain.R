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
                             parms,
                             remove.leaf.info = FALSE){
  # Note that remove.leaf.info does not permit a posteriori estimation

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
  mc$remove.leaf.info <- NULL


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

  # if (!missing(method))  attr(rf, "split.method") <- method else attr(rf, "anova")

  return(rf)
}
