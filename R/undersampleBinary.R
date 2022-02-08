undersampleBinary <- function(y, x,
                              size = NULL, replace = T,
                              print.info = FALSE){

  if (is.null(dim(y))){
    p.vector <- rep(1, length(y))
    collapsed <- y
    t.collapsed <- table(collapsed)

    idxS <-
      lapply(names(t.collapsed), function(nt){
        which(collapsed == nt)
      })

    names(idxS) <- names(t.collapsed)

    for (iidx in names(idxS)){
      p.vector[idxS[[iidx]]] <- t.collapsed[iidx]
    }

    if (is.null(size)) size <- length(t.collapsed)*min(t.collapsed)

    idx.sample <- sample(1:length(y), prob = 1/p.vector,
                         replace = replace, size = size)

    tbr <-
      list(IS = list(x = x[ idx.sample, ],
                     y = y[ idx.sample ]),
           OOS = list(x = x[ -idx.sample, ],
                      y = y[ -idx.sample ])
      )
    attr(tbr, "idx.sample") <- idx.sample

  } else {
    p.vector <- rep(1, nrow(y))

    collapsed <- apply(y, MARGIN = 1, paste, collapse = ".")
    t.collapsed <- table(collapsed)

    idxS <-
      lapply(names(t.collapsed), function(nt){
        which(collapsed == nt)
      })

    names(idxS) <- names(t.collapsed)

    for (iidx in names(idxS)){
      p.vector[idxS[[iidx]]] <- t.collapsed[iidx]
    }

    if (is.null(size)) size <- length(t.collapsed)*min(t.collapsed)

    idx.sample <- sample(1:nrow(y), prob = 1/p.vector, replace = replace, size = size)

    tbr <-
      list(IS = list(x = x[ idx.sample, ],
                     y = y[ idx.sample, ]),
           OOS = list(x = x[ -idx.sample, ],
                      y = y[ -idx.sample, ])
      )

    attr(tbr, "idx.sample") <- idx.sample
  }

  return(tbr)
}
