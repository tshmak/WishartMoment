count <- function(index.matrix, allpairs, index, Sigma,
                  row=1, col=1, tally=NULL, p=ncol(index.matrix)) {
  #' @param index.matrix A 2*nallpairs matrix of indices
  #' @param col The column of index.matrix we're working on
  #' @param allpairs A matrix of all possible splits by pair
  #' @param index An index reference
  #' @param Sigma A matrix of the covariances
  #' @param row The current row we're working on in tally
  #' @param tally A vector to keep counts of the products

  s <- index.matrix # Short hand

  if(is.null(tally)) {
    # tally <- rep(NA, p^ncol(s) * length(allpairs))
    tally <- rep(NA, p^(ncol(s)-1) * length(allpairs)) # New trick
  }
  if(col < ncol(s)) {
    for(a in 1:p) {
      s[,col] <- a
      result <- count(index.matrix=s,
                      allpairs=allpairs, index=index, Sigma=Sigma,
                      row=row, col=col+1, tally=tally, p=p)
      row <- result$row
      tally <- result$tally
      if(col == 1) break # New trick
    }
  } else {
    for(a in 1:p) {
      s[,col] <- a
      for(x in 1:length(allpairs)) {
        # x <- 1
        prod <- 1
        for(y in 1:ncol(allpairs[[x]])) {
          # y <- 1
          ii <- allpairs[[x]][,y]
          if(s[ii[1]] == s[ii[2]]) { # This is the same column
            E <- index[ii]
            E2 <- Sigma[E[1], E[2]]
            prod <- prod * E2
          } else {
            prod <- 0
          }
          if(prod == 0) break
        }
        tally[row] <- prod
        row <- row + 1
        if(row >= 100000 & row %% 100000 == 0) {
          cat(row, " out of ", length(tally), " complete.\n")
        }
      }
    }
    result <- list(tally=tally, row=row)
  }
  return(result)
}
