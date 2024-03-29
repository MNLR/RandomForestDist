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
#' @param method The split function. Currently supported split functions are:
#' \code{"anova"} (default) for root mean squared error; \code{"poisson"} for poisson
#' distributed \code{y} (expect two columns); \code{"class"} for classification of
#' factors (default if \code{y} is factor); \code{"exp"} for survival objects;
#' \code{"gammaLLMME"}, \code{"gammaLLmean"}, \code{"gammaLLBC3"} for the 2-parameter
#' gamma distribution log-likelihood using moments-matching-estimation, the mean, and
#' the BC3 estimators, respectively; \code{"bernoulliGammaLLMME"} for the 3-parameter
#' bernoulli-gamma log likelihood; \code{"gammaDeviation"} for the deviation of the
#' 2-parameter gamma distribution; \code{"bernoulliLL"} for the bernoulli
#' log-likelihood (expects class \code{numeric} with \code{0,1}).
#' @param resample Either \code{TRUE} or \code{FALSE}. Should resampling of \code{x, y} be
#' done?
#' @param replace Either \code{TRUE} or \code{FALSE}. Should samples of \code{x,y} be
#' drawn with replacement?
#' @param sampsize Size of the samples of \code{x,y}
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
#' @param keep.x Keeps a copy of the training set x for each tree. Default to FALSE, reduces
#' the memory usage.
#' @param keep.y Keeps a copy of the training set y for each tree. Default to TRUE.
#' NOTE: Setting this to \code{FALSE} disables a posteriori estimation.
#' @param ignore.y.indices A vector, used to ignore certain columns during the training
#' stage. Usefull to use it a posteriori. Default to NULL: nothing will be ignored.


randomForestTrain <- function(x, y = NULL,
                              ntree = 100,
                              mtry = if (!is.null(y) && !is.factor(y))
                                        max(floor(ncol(x)/3), 1)
                                     else floor(sqrt(ncol(x))),
                              minsplit = if (!is.null(y) && !is.factor(y)) 5 else 1,
                              minbucket = minsplit/3,
                              maxdepth = 30,
                              method = "anova",
                              resample = TRUE,
                              replace = TRUE,
                              sampsize = NULL,
                              oversample.binary = FALSE,
                              undersample.binary = FALSE,
                              oob.prunning = FALSE,
                              oob.prunning.function = NULL,
                              parallel.plan = NULL,
                              workers = NULL,
                              weights = NULL,
                              parms = NULL,
                              remove.leaf.info = FALSE,
                              keep.x = FALSE,
                              keep.y = TRUE,
                              ignore.y.indices = NULL,
                              progress.bar = TRUE,
                              checkpoints.every = NULL,
                              checkpoints.label = "grove",
                              delete.checkpoints.afterMerging = TRUE){

  if (is.null(checkpoints.every)){

    if (
        (!is.null(parallel.plan) && !is.function(parallel.plan) && is.na(parallel.plan)) ||   ## No parallel selected or packages not available
        !requireNamespace("parallelly", quietly = TRUE) ||
        !requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("future.apply", quietly = TRUE)
      ){
      lapply.opt <- "lapply"
    } else {
      lapply.opt <- "future_lapply"
      if (is.null(parallel.plan)){
        # and do nothing, use plan set outside
      } else {
        o.plan <- future::plan()
        if (is.null(workers)) workers <- parallelly::availableCores()

        if (is.character(parallel.plan) && parallel.plan == "auto"){
          future::plan(future::multisession, workers = workers) # the default is multisession, since it works interactively
        } else { # plan set as option parallel.plan
          future::plan(parallel.plan, workers = workers)
        }
        on.exit(future::plan(o.plan), add = TRUE)
      }
    }


    if (!is.null(ignore.y.indices)){
      y.complete <- y
      y <- y[,-ignore.y.indices, drop = FALSE]
    }

    stopifnot(sum(is.na(y)) == 0)
    stopifnot(sum(is.na(x)) == 0)

    # mandatory, otherwise passed to rpart:
    cp <- -Inf  # RFs do not regularize - Ensures negative nll values don't conflict
    xval <- 0

    if (is.null(dim(x))) x <- matrix(x, nrow = length(x))

    if (is.null(sampsize)){
      if (!undersample.binary){
        if (replace) sampsize <- nrow(x) else sampsize <- ceiling(.632*nrow(x))
      }
    }

    nrx <- nrow(x)

    idxS <- 1:ntree
    with_progress(enable = progress.bar, expr = {
      p <- progressor(along = idxS)
      if (lapply.opt == "future_lapply") {
        rf <- future.apply::future_lapply(future.packages = "rpart",
                                          future.seed = TRUE,
                                          future.stdout = FALSE,
                                          X = idxS, FUN = function(idxt){

                              if (resample) {
                                sid <- sample(1:nrx, size = sampsize, replace = replace)

                                if (oob.prunning){
                                  idxoob <- setdiff(1:nrx, sid)

                                  if (!is.null(dim(x))) xoob <- x[idxoob, , drop = FALSE ] else xoob <- x[idxoob, drop = FALSE]
                                  if (!is.null(dim(y))) yoob <- y[idxoob, , drop = FALSE] else yoob <- y[idxoob, drop = FALSE]
                                }

                                if (!is.null(dim(x))) x <- x[sid, , drop = FALSE] else x <- x[sid , drop = FALSE]
                                if (!is.null(dim(y))) y <- y[sid, , drop = FALSE] else y <- y[sid , drop = FALSE]
                              } else if (oversample.binary){
                                os <- oversample(y = y, x = x, printm = FALSE)

                                y <- os$y
                                x <- os$x
                                os <- NULL
                              } else if (undersample.binary){
                                us <- undersampleBinary(y = y, x = x,
                                                        size = sampsize,
                                                        replace = replace,
                                                         print.info = T)

                                y <- us$IS$y
                                x <- us$IS$x
                                us <- NULL
                              }

                              tree <-
                                rpart( formula = y ~ x,
                                       data = data.frame(y = I(y), x = I(x)),
                                       mtry = mtry,
                                       minsplit = minsplit,
                                       minbucket = minbucket,
                                       cp = cp,
                                       maxdepth = maxdepth,
                                       xval = 0,
                                       method = method,
                                       weights = weights,
                                       parms = parms,
                                       control = NULL,
                                       x = keep.x,
                                       y = keep.y
                                       )

                              if (resample && oob.prunning){
                                tree <- pruneFromSample(tr = tree, x = xoob, y = yoob,
                                          oob.prunning.function = oob.prunning.function,
                                                        plot.tree.sequence = FALSE)
                              }

                              if (resample){
                                attr(tree, "resample.indices") <- sid
                                if (!is.null(ignore.y.indices)){
                                  tree$y <- y.complete[sid,]
                                }
                              } else {
                                if (!is.null(ignore.y.indices)){
                                  tree$y <- y.complete
                                }
                              }

                              p(message = sprintf("Tree %g/%g", idxt, ntree))
                              gc()
                              return(tree)
                            })
      } else if (lapply.opt == "lapply"){
        rf <- lapply(X = idxS, FUN = function(idxt){

          if (resample) {
            sid <- sample(1:nrx, size = sampsize, replace = replace)

            if (oob.prunning){
              idxoob <- setdiff(1:nrx, sid)

              if (!is.null(dim(x))) xoob <- x[idxoob, , drop = FALSE ] else xoob <- x[idxoob, drop = FALSE ]
              if (!is.null(dim(y))) yoob <- y[idxoob, , drop = FALSE ] else yoob <- y[idxoob, drop = FALSE ]
            }

            if (!is.null(dim(x))) x <- x[sid, , drop = FALSE ] else x <- x[sid, drop = FALSE ]
            if (!is.null(dim(y))) y <- y[sid, , drop = FALSE ] else y <- y[sid, drop = FALSE ]
          } else if (oversample.binary){
            os <- oversample(y = y, x = x, printm = FALSE)

            y <- os$y
            x <- os$x
            os <- NULL
          } else if (undersample.binary){
            us <- undersampleBinary(y = y, x = x,
                                           size = sampsize,
                                           replace = replace,
                                           print.info = T)

            y <- us$IS$y
            x <- us$IS$x
            us <- NULL
          }

          tree <-
            rpart( formula = y ~ x,
                   data = data.frame(y = I(y), x = I(x)),
                   mtry = mtry,
                   minsplit = minsplit,
                   minbucket = minbucket,
                   cp = cp,
                   maxdepth = maxdepth,
                   xval = 0,
                   method = method,
                   weights = weights,
                   parms = parms,
                   control = NULL,
                   x = keep.x,
                   y = keep.y
            )

          if (oob.prunning){
            tree <- pruneFromSample(tr = tree, x = xoob, y = yoob,
                                    oob.prunning.function = oob.prunning.function,
                                    plot.tree.sequence = FALSE)
          }

          if (resample){
            attr(tree, "resample.indices") <- sid
            if (!is.null(ignore.y.indices)){
              tree$y <- y.complete[sid,]
            }
          } else {
            if (!is.null(ignore.y.indices)){
              tree$y <- y.complete
            }
          }

          p(message = sprintf("Tree %g/%g", idxt, ntree))



          return(tree)
        })
      } else {stop("Internal Error: lapply.opt not set")}

    })
  } else {

    if (!dir.exists(".tempRandomForestDistFiles")){
      delete.tempDir <- T
      dir.create(".tempRandomForestDistFiles")
    } else {
      delete.tempDir <- FALSE
    }


    last.grove <- ntree %% checkpoints.every

    ngroves <- (ntree %/% checkpoints.every)
    if (last.grove != 0) ngroves <- ngroves + 1
    else last.grove <- checkpoints.every


    fileS <- c()
    for ( igrove in 1:ngroves ){
      if (progress.bar){
        print(paste0("Building grove ", igrove , "/", ngroves, "..."))
      }
      if (igrove == ngroves) checkpoints.every <- last.grove

      rfdist.grove <-
        randomForestTrain(x = x, y = y,
                          ntree = checkpoints.every,
                          mtry = mtry,
                          minsplit = minsplit,
                          minbucket = minbucket,
                          maxdepth = maxdepth,
                          method = method,
                          resample = resample,
                          replace = replace,
                          sampsize = sampsize,
                          oversample.binary = oversample.binary,
                          undersample.binary = undersample.binary,
                          oob.prunning = oob.prunning,
                          oob.prunning.function = oob.prunning.function,
                          parallel.plan = parallel.plan,
                          workers = workers,
                          weights = weights,
                          parms = parms,
                          remove.leaf.info = remove.leaf.info,
                          keep.x = keep.x,
                          keep.y = keep.y,
                          ignore.y.indices = ignore.y.indices,
                          progress.bar = progress.bar
                          , checkpoints.every = NULL,
                          checkpoints.label = NULL,
                          delete.checkpoints.afterMerging = FALSE)

      file.name <- paste0(".tempRandomForestDistFiles", "/",
                          checkpoints.label, as.character(as.numeric(Sys.time())), ".rda")

      save(rfdist.grove,
           file = file.name)


      fileS <- c(fileS, file.name)

      rm(rfdist.grove)
      gc(full = TRUE)

    }


    if (progress.bar){ print("Merging groves...") }
    rf <-
      lapply(fileS, function(file){
        load(file)
        return(rfdist.grove)
      })

    rf <- do.call(c, rf)

    if (delete.checkpoints.afterMerging){
      if (progress.bar){ print("Deleting groves...") }
        lapply(fileS, function(file){
          file.remove(file)
        })
        file.remove(".tempRandomForestDistFiles")
    }

    if (progress.bar){ print("Done") }


  }

    if (    method =="binaryCrossEntropyMultivar"
         || method == "binaryCrossEntropyMultivarCorPenalization"
         || method == "binaryMultiEntropy"
         || method == "binaryMultiEntropyCond"
         || method == "binaryMargEntropyCond"
         || method == "multiBinaryGammaEntropy"
         || method == "MSEgammaDeviance"
         || method == "MSEbinaryEntropyGammaDeviance" || !is.null(dim(rf[[1]]$y))){
      attr(rf, "multiresponse") = TRUE
    } else {
      attr(rf, "multiresponse") = FALSE
    }

    class(rf) <- "RandomForestDist"

  return(rf)
}
