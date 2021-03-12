isDescendant <- function (all.leaves, node) {
  # If no leaves are given, get out.
  # Convert the others to numeric, because they may have started as row numbers.
  if (length (all.leaves) == 0) return (logical (0))
  all.leaves <- as.numeric (all.leaves); node <- as.numeric (node)
  if (missing (node)) return (NA)

  result <- rep (FALSE, length (all.leaves))
  for (i in 1:length (all.leaves)) {
    LEAF <- all.leaves[i]
    while (LEAF > node) {
      LEAF <- trunc (LEAF/2)
      if (LEAF == node) result[i] <- TRUE
      break
    }
  }
  return (result)
}


rpartPredictLeaves <- function(rp, newdata){
  # search-for-corresponding-node-in-a-regression-tree-using-rpart

  rp$frame$yval <- 1:nrow(rp$frame)
  should.be.leaves <- which(rp$frame[, 1] == "<leaf>")

  # Sometimes -- I don't know why -- the rpart.predict() function will
  # give back a "leaf membership" that's not a leaf. See if that's true.
  # Thanks to  http://stackoverflow.com/questions/5102754/

  leaves <- predict(rp, newdata = newdata, type = "vector") # vector of "int"

  bad.leaves <- leaves[!is.element (leaves, should.be.leaves)]
  if (length (bad.leaves) == 0) return (leaves)
  #
  #
  # If we got here there are some elements of "leaves" that are not in
  # fact leaves. Strictly we should visit the most populous of its children,
  # then the most populous of *that* node's children, and so on until we got
  # to a leaf. But in fact we'll just choose the most populous leaf underneath
  # our landing point.
  #
  # "Leaves" are row numbers in the rpart frame. "Nodes" are actual node
  # numbers -- also row *names* in the rpart frame. We'll need both.
  #
  u.bad.leaves <- unique (bad.leaves)
  u.bad.nodes <- row.names (rp$frame)[u.bad.leaves]
  all.nodes <- row.names (rp$frame)[rp$frame[,1] == "<leaf>"]
  #
  # We'll make use of this function, which given a vector of leaf numbers
  # produces a logical of the same length whose ith entry is TRUE if that
  # leaf number is a descendant of node.
  #

  #
  # Okay. Look through each unique bad entry in u.nodes. For each
  # one, figure out which leaves are its descendants, then consult
  # the table of leaves to find out the most populous of the entries
  # associated with descendants (in the training set, as recorded
  # in the tree's "where" entry). Set the relevant indices of  "leaves."
  #
  # Remember that "where.tbl" has some shaky entries in it -- the very
  # ones we're here to remove.
  #
  where.tbl <- table (rp$where)
  names (where.tbl) <- row.names (rp$frame)[as.numeric (names (where.tbl))]
  #
  # Loop over the set of nodes that aren't leaves but think they are.
  #
  for (u in 1:length (u.bad.nodes)) {
    desc.vec <- isDescendant(all.nodes, u.bad.nodes[u])
    #
    # Use "all.nodes" to extrac from where.tbl only those entries that
    # belong to legitimate leaves. Then pull out the descendants.
    #
    me <- where.tbl[all.nodes][desc.vec]
    #
    # Find the maximal entry, grab its name. The [1] breaks ties.
    #
    winner <- names (me)[me == max(me)][1]
    leaves[leaves == u.bad.leaves[u]] <- which (row.names (rp$frame) == winner)
  }
  return (leaves)
}






predictLeaves <- function(model, newdata){
  y.original <- model[[1]]$call$data$y

  if (class(y.original) == "factor"){
    labels <- levels(y.original)

    tbr <-
      simplify2array(
        lapply(model, function(tr){
          tbr <- predict(object = tr, newdata = newdata,
                         type = "matrix")[, 2:(length(labels)+1)]
          colnames(tbr) <- labels
          return(tbr)
        })
      )

    tbr <- apply(tbr, MARGIN = c(1,2), sum) #Information per leaf is lost

  } else if (model[[1]]$method == "bernoulliLL") {
    labels <- c("0", "1")

    tbr <-
      simplify2array(
        lapply(model, function(tr){
          tbr <- predict(object = tr,
                         newdata = newdata, type = "matrix")

          return(tbr)
        })
      )

    tbr <- sapply(1:dim(tbr)[1],
           function(iinst){
             nn <- sum(tbr[iinst, , ][2,])
             return(
              sum((tbr[iinst, , ][1,])*(tbr[iinst, , ][2,]))/nn
             )
            })

  } else {
    prl <-
      lapply(model, function(tr){
        pr.leaves <- rpartPredictLeaves(tr,
                                        newdata = newdata
                                        )
        pr.leaveselements <- lapply(pr.leaves,
                                    function(ll){
                                      tr$y[which(ll == tr$where)]
                                    })

        return( pr.leaveselements )
      })

    tbr <-
      lapply(1:length(prl[[1]]),
             function(ix) {
               do.call(c, lapply(prl, function(tr){
                 return(tr[[ix]])
               })
               )
             })
  }

  return(tbr)
}
