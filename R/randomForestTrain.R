#' @export


randomForestTrain <- function(x, y = NULL,
                             ntree = 500,
                             mtry = if (!is.null(y) && !is.factor(y)) max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
                             nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
                             maxdepth = 30,
                             cp = 0.01,
                             method,
                             replace = TRUE,
                             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
                             parallel.plan = multisession,
                             workers = NULL,
                             weights,
                             parms,
                             control
){

  stopifnot(sum(is.na(y)) == 0)
  stopifnot(sum(is.na(x)) == 0)

  mc <- match.call()

  # mandatory, otherwise passed to rpart:
  mc$ntree <- NULL
  mc$workers <- NULL
  mc$nodesize <- NULL
  mc$replace <- NULL
  mc$sampsize <- NULL

  mc[[1]] <- quote(rpart)
  mc$formula <- y ~ .
  mc$x <- NULL
  mc$y <- NULL
  mc$method <- method
  mc$minbucket <- nodesize
  mc$mtry <- mtry

  plan(parallel.plan, workers = if (is.null(workers)) (future::availableCores()-1) else workers)
  handlers("progress")

  idxS <- 1:ntree
  with_progress({
    p <- progressor(along = idxS)
    rf <- future_lapply(future.packages = "rpart", future.seed = T,
                        X = idxS, FUN = function(idxt){
                          p()

                          if (is.null(dim(x))) x <- matrix(x)
                          sid <- sample(1:nrow(x), size = sampsize, replace = replace)
                          x <- x[sid, ]
                          if (!is.null(dim(y))) y[sid, ] else y <- y[sid]
                          mc$data <- data.frame(y, x)

                          return( eval(mc) )
                        })
  })

  return(rf)
}
