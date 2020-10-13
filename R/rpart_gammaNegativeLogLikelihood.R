gammaParameters <- function(shape, rate = 1, m = NULL, s = NULL){
  if (is.null(m)){
    if (is.null(s)) stop("Provide the standard deviation s")
    else return(list(shape = (m^2)/(s^2), rate = m/s^2))
  } else {
    return(list(mean = shape/rate, sd = sqrt(shape)/rate))
  }
}


itemp <- function(y, offset, parms, wt) {   # Initialization function
  if (is.matrix(y) && ncol(y) > 1)
    stop("Multivariate response not yet implemented")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", dev=" , format(signif(dev, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

etemp <- function(y, wt, parms) {  # Evaluation function
  label <- sum(y*wt)/sum(wt)
  return(list(label = label,
              deviance = - sum(dgamma(x = y, shape = label, rate = 1,
                                      log = T))
  ))
}




stemp <- function(y, wt, x, parms, continuous)   # Split Function, higher goodness is better. <= 0 is no improvement
                                                 # For Negative Log Likelihood lower is better. This implies:
                                                 # Root node negative LL - Sum(Leaves negative LL) =
                                                 # 0  : no improvement
                                                 # <0 : split worsens results
                                                 # >0 : improvement

{
  n <- length(y)
  if (continuous) {
    # continuous x variable
    lmean <- cumsum(y*wt)[-n]/cumsum(wt)[-n]
    rmean <- rev( cumsum(rev(y[-1])*rev(wt[-1]))/cumsum(rev(wt[-1])) )

    if (is.null(parms$rate)){ # fixed rate
      goodness <-
        sapply(1:(n-1), function(idx){
          - sum(dgamma(x = y, shape = sum(y*wt)/sum(wt), rate = 1, log = T)) + # root node negative LL - sum of leaves negative LL
            sum(dgamma(x = y[1:idx], shape = lmean[idx], rate = 1, log = T)) +  #
            sum(dgamma(x = y[(idx+1):(n)], shape = rmean[idx], rate = 1, log = T))
        })
    } else{
      stop("To be implemented")
    }
    # print(paste0("Which Goodness Improvement Sorted Descending (mean =", sum(y*wt)/sum(wt) , "):"))
    # print(which(goodness == goodness[order(goodness, decreasing = T)[1]]))
    # print(goodness[order(goodness, decreasing = T)[1]])

    return( list(goodness = goodness, direction = rep(-1,length(goodness))) )
  } else {
    stop("To be implemented")
    # Categorical X variable
    # ux <- sort(unique(x))
    # wtsum <- tapply(wt, x, sum)
    # ysum <- tapply(y*wt, x, sum)
    # means <- ysum/wtsum
    # # For anova splits, we can order the categories by their means
    # # then use the same code as for a non-categorical
    # ord <- order(means)
    # n <- length(ord)
    # temp <- cumsum(ysum[ord])[-n]
    # left.wt <- cumsum(wtsum[ord])[-n]
    # right.wt <- sum(wt) - left.wt
    # lmean <- temp/left.wt
    # rmean <- -temp/right.wt
    # list(goodness= (left.wt*lmean^2 + right.wt*rmean^2)/sum(wt*y^2),
    #      direction = ux[ord])
  }
}

rpart_gammaNegativeLogLikelihood <- function() list(eval = etemp, split = stemp, init = itemp)


# # SIMPLE TEST
# # SIMPLE TEST
# # SIMPLE TEST
# # SIMPLE TEST
# # SIMPLE TEST
# # SIMPLE TEST
# # SIMPLE TEST
#
#
#
# library(rpart)
#
#
# nllt <- function(y, n1){
# tbr <- c(- sum(dgamma(x = y,
#              shape = mean(y), rate = 1,
#              log = T)),
#
#          - sum(dgamma(x = y[1:n1],
#              shape = mean(y[1:n1]), rate = 1,
#              log = T)) -
#            sum(dgamma(x = y[(n1+1):length(y)],
#                shape = mean(y[(n1+1):length(y)]), rate = 1,
#                log = T))
# )
# names(tbr) <- c("or", "sp")
# return(tbr)
# }
#
#
# sh1 <- 100
# n1 <- 10
# sh2 <- 0.5
# n2 <- 1000
#
# sh3 <- 15
# n3 <- 16
#
# y <- c(rgamma(n1, sh1), rgamma(n2, sh2), rgamma(n1, sh1), rgamma(n3, sh3))
# wt <- rep(1,length(y))
# x <- seq(1, length(y))
#
# y <- rev(y)
# x <- rev(x)
#
#
# cosa <- rpart(y ~ x, data = data.frame(y, x),
#               method = rpart_gammaNegativeLogLikelihood(), cp = 0.01 )
#
# nllt(y, 10)
# s1i <- y[1:10]
# s1d <- y[11:1010]
# mean(s1d)
#
# gg <- stemp(y = s1d, wt = rep(1, length(s1d)), x = seq(1, length(s1d)), continuous = T, parms = NULL)$goodness
#
#
# (nllt(s1d, 661)[1] - nllt(s1d, 661)[2])/nllt(s1d, 661)[1]
# s2i <- s1d[1:661]
# s2d <- s1d[662:length(s1d)]
# mean(s2i)
# mean(s2d)
#
# nllt(s2d, 329)
#
# s3i <- s2d[1:329]
# s3d <- s2d[330:length(s2d)]
# mean(s3i)
# mean(s3d)
#
# stemp(y = s3d, wt = rep(1, length(s3d)), x = seq(1, length(s3d)),
#             continuous = T, parms = NULL)$goodness
#
# stemp(y = s3i, wt = rep(1, length(s3i)), x = seq(1, length(s3i)),
#       continuous = T, parms = NULL)$goodness
#
# s4i <- s3i[1:250]
# s4d <- s3i[251:length(s3i)]
# mean(s4i)
# mean(s4d)
#
# stemp(y = s4d, wt = rep(1, length(s4d)), x = seq(1, length(s4d)),
#       continuous = T, parms = NULL)$goodness
#
# stemp(y = s4i, wt = rep(1, length(s4i)), x = seq(1, length(s4i)),
#       continuous = T, parms = NULL)$goodness
#
# s5i <- s4i[1:201]
# s5d <- s4i[202:length(s4i)]
# mean(s5i)
# mean(s5d)
