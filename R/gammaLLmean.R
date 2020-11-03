
itemp.gammaLLmean <- function(y, offset, parms, wt) {   # Initialization function
  if (is.matrix(y) && ncol(y) > 1)
    stop("Multivariate response not yet implemented")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean = ", format(signif(yval, digits)),
          ", dev =" , format(signif(dev, digits)),
          sep = '')
  }
  environment(sfun) <- .GlobalEnv
  list(y = c(y), parms = NULL, numresp = 1, numy = 1, summary = sfun)
}

etemp.gammaLLmean <- function(y, wt, parms) {  # Evaluation function

  y <- y*wt
  my <- mean(y)

  return(list(label = my,
              deviance = - sum(dgamma(x = y, shape = my, rate = 1,
                                    log = T)))
  )
}

stemp.gammaLLmean <- function(y, wt, x, parms, continuous)   # Split Function, higher goodness is better. <= 0 is no improvement
  # For Negative Log Likelihood lower is better. This implies:
  # Root node negative LL - Sum(Leaves negative LL) =
  # 0  : no improvement
  # <0 : split worsens results
  # >0 : improvement

{
  n <- length(y)
  hc.minbucket <- 5

  if (continuous) {
    # continuous x variable
    # Minimum amount of elements per node hardcoded to 5 for a reasonable estimate

    ywt <- y*wt

    temp <- cumsum(ywt[-n])
    left.wt <- cumsum(wt[-n])
    right.wt <- sum(wt) - left.wt
    lmean <- (temp/left.wt)
    rmean <- (rev(cumsum(rev(ywt)[-n]))/right.wt)


    # lsy <- c(rep(-Inf, hc.minbucket-1), temp[1,], rep(NA, hc.minbucket-1))
    # rsy <- c(rep(-Inf, hc.minbucket-1), temp[2,], rep(NA, hc.minbucket-1))


    llor <- sum(dgamma(x = ywt,
                       shape = mean(ywt),
                       rate = 1,
                       log = T))

    goodness <- sapply(1:length(lmean), function(idx){
                    sum(dgamma(x = y[1:idx],
                               shape = lmean[idx], rate = 1, log = T)) +  #
                      sum(dgamma(x = y[(idx+1):(n)],
                                 shape = rmean[idx], rate = 1, log = T))
                  }) - llor

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

gammaLLmean <- function() list(eval = etemp.gammaLLmean,
                              split = stemp.gammaLLmean,
                              init = itemp.gammaLLmean)


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

# library(future.apply)
# library(fitdistrplus)
# sh1 <- 2.5
# n1 <- 50
# rt1 <- 0.5
#
#
# sh2 <- 0.5
# n2 <- 50
#
# sh3 <- 15
# n3 <- 50
#
# plan(multisession, workers = 12)
# nsa <- 10000
# cosa <-
# # future_lapply(future.seed = T,
#   lapply(
#               X = 1:nsa,FUN =  function(idnt){
#
#
# y <- c(rgamma(n = n1, shape = sh1, rate = rt1), rgamma(n = n2, shape =  sh2), rgamma(n = n3, shape = sh3))
# wt <- rep(1,length(y))
# x <- seq(1, length(y))
#
#
# n <- length(y)
# hc.minbucket <- 5
#
# ywt <- y*wt
#
# temp <- cumsum(ywt[-n])
# left.wt <- cumsum(wt[-n])
# right.wt <- sum(wt) - left.wt
# lmean <- (temp/left.wt)[hc.minbucket:(n-hc.minbucket)]
# rmean <- (rev(cumsum(rev(ywt)[-n]))/right.wt)[hc.minbucket:(n-hc.minbucket)]
#
# temp <- vapply( hc.minbucket:(n-hc.minbucket),
#                 function(i) c(sd(ywt[1:i]), sd(ywt[(i+1):n])), c(1,1))
#
# # lsy <- c(rep(-Inf, hc.minbucket-1), temp[1,], rep(NA, hc.minbucket-1))
# # rsy <- c(rep(-Inf, hc.minbucket-1), temp[2,], rep(NA, hc.minbucket-1))
#
# lshape = (lmean^2)/(temp[1,]^2)
# lrate = lshape / lmean
#
# rshape = (rmean^2)/(temp[2,]^2)
# rrate = rshape / rmean
#
# shpor <- (mean(ywt)/sd(ywt) )^2
#
# llor <- sum(dgamma(x = ywt,
#                    shape = shpor,
#                    rate = shpor/mean(ywt),
#                    log = T))
#
# goodness <- c( rep(0, hc.minbucket - 1),
#               sapply(1:length(lshape), function(idx){
#                 sum(dgamma(x = y[1:(idx-1+hc.minbucket)],
#                            shape = lshape[idx], rate = lrate[idx], log = T)) +  #
#                   sum(dgamma(x = y[(idx+hc.minbucket):(n)],
#                              shape = rshape[idx], rate = rrate[idx], log = T))
#               }) - llor, rep(0, hc.minbucket - 1))
#
# est1 <- fitdist(y[1:50], distr = "gamma", method = "mle")$estimate
# tbr <- cbind(c(lshape[46], lrate[46]), c(est1))
# colnames(tbr) <- c("mme", "fdp")
#
# return(tbr)
#
# })
#
# dev.new()
# par(mfrow = c(2,2))
# hist(sapply(cosa, function(m) m[1,1]), breaks = nsa/25, freq = F, xlim = c(min(sapply(cosa, function(m) m[1,1])),max(sapply(cosa, function(m) m[1,1]))),
#      xlab = "", main = "mme", ylab = "shape", ylim = c(0,1))
# abline(v = sh1, col = "red")
# abline(v = mean(sapply(cosa, function(m) m[1,1])), col = "green")
# abline(v = median(sapply(cosa, function(m) m[1,1])), col = "blue")
#
# hist(sapply(cosa, function(m) m[1,2]), breaks = nsa/25, freq = F, xlim = c(min(sapply(cosa, function(m) m[1,1])),max(sapply(cosa, function(m) m[1,1]))),
#      xlab = "", ylab = "", main = "mle", ylim = c(0,1))
# abline(v = sh1, col = "red")
# abline(v = mean(sapply(cosa, function(m) m[1,2])), col = "green")
# abline(v = median(sapply(cosa, function(m) m[1,2])), col = "blue")
# legend("topright", legend = c("True Value", "Mean", "Median" ),  col=c("red","green" ,"blue"), lty=1, cex=0.8)
#
#
# hist(sapply(cosa, function(m) m[2,1]), breaks = nsa/25, freq = F, xlim = c(min(sapply(cosa, function(m) m[2,1])), max(sapply(cosa, function(m) m[2,1]))),
#      xlab = "", main = "", ylab = "rate", ylim = c(0,5))
# abline(v = rt1, col = "red")
# abline(v = mean(sapply(cosa, function(m) m[2,1])), col = "green")
# abline(v = median(sapply(cosa, function(m) m[2,1])), col = "blue")
#
# hist(sapply(cosa, function(m) m[2,2]), breaks = nsa/25, freq = F, xlim = c(min(sapply(cosa, function(m) m[2,1])),max(sapply(cosa, function(m) m[2,1]))),
#      xlab = "", ylab = "", main = "", ylim = c(0,5))
# abline(v = rt1, col = "red")
# abline(v = mean(sapply(cosa, function(m) m[2,2])), col = "green")
# abline(v = median(sapply(cosa, function(m) m[2,2])), col = "blue")
# dev.print(pdf, "../downscaleR.forests/plots/mmevsmle2.pdf")
