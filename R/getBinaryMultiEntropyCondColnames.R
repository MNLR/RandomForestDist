getBinaryMultiEntropyCondColnames <- function(n.predictands){
  return(
    c("P(Y1)",
      do.call(c,
              lapply(seq(from = 2, to = n.predictands, by = 1), function(varindex){
                mat01 <- expand.grid(rep(list(c(0,1)), varindex - 1))

                paste0(
                  paste0("Y", varindex, "|"),
                  sapply(1:nrow(mat01),
                         function(if01)
                           paste(paste(paste0("Y",
                                              1:length(mat01[if01,])),
                                       mat01[if01,], sep = "="),
                                 collapse = ",")
                  )
                )
              } )
      )
    )
  )
}
