estimateLeavesBernoulliBC3 <- function(leaves.info, noninformative.beta = 1,
                                       parallel.plan = NULL, workers = NULL,
                                       progress.bar = TRUE){

  # CURRENTLY SET TO NOT PARALLEL
  # parallel.plan = NA
  # E CURRENTLY SET TO NOT PARALLEL


  # if (requireNamespace("future", quietly = TRUE) &&
  #     requireNamespace("future.apply", quietly = TRUE)
  # ) {
  #   lapply.opt <- "future_lapply"
  #   if (missing(parallel.plan) || is.null(parallel.plan)){
  #     parallel.plan <- future::plan()
  #   }
  #   else if (is.character(parallel.plan) && parallel.plan == "auto") {
  #     parallel.plan <- future::plan(future::multisession)
  #   }
  #   else {
  #     if (!is.list(parallel.plan) &&
  #         !is.function(parallel.plan) &&
  #         is.na(parallel.plan)) lapply.opt <- "lapply"
  #     else{
  #       o.plan <- future::plan()
  #       if (missing(workers)) future::plan(parallel.plan)
  #       else future::plan(parallel.plan,
  #                         workers = if (is.null(workers) || workers == 0) (availableCores()) else workers
  #       )
  #       on.exit(future::plan(o.plan), add = TRUE)
  #     }
  #   }
  # } else { # package future or future.apply not available
  #   lapply.opt <- "lapply"
  # }

  idx = 1:length(leaves.info)

  with_progress(enable = progress.bar,
                expr = {
    p <- progressor(along = idx)

    # if (lapply.opt == "future_lapply") {
    #
    #   prl <- future.apply::future_lapply(future.stdout = NA,
    #                                     X = idx, FUN = function(ii){
    #                                       tbr <- .Call(`_RandomForestDist_estimateBernoulliGammabc3`,
    #                                                    leaves.info[[ii]], noninformative.beta)
    #                                                           p(message = sprintf("%g/%g", ii, length(leaves.info)))
    #
    #                                       return(tbr)
    #                                     })
    #
    # } else {

      prl <- lapply(X = idx,
                    FUN = function(ii){
                      tbr <- .Call(`_RandomForestDist_estimateBernoulliGammabc3`,
                                   leaves.info[[ii]], noninformative.beta)
                      p(message = sprintf("%g/%g", ii, length(leaves.info)))

                      return(tbr)
                  })
#
  # }
  })

  prl = do.call(rbind, prl)

  return(prl)
}
