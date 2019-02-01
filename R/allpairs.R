allpairs <- function(vec, M=matrix(integer(0),2,0)) {
  stopifnot(length(vec) %% 2 == 0)
  
  rm.colnames <- function(M) {
    colnames(M) <- NULL
    return(M)
  }
  
  if(length(vec) == 2) {
    return(list(rm.colnames(cbind(M, vec))))
  }
  
  i <- vec[1]
  MM <- list()
  k <- 1
  for(j in vec[2:length(vec)]) {
    ij <- c(i,j)
    remaining <- vec[!(vec %in% ij)]
    TT <- allpairs(remaining, M=matrix(ij, 2,1))
    if(length(TT) > 1) {
      for(kk in 1:length(TT)) {
        MM[k] <- list(rm.colnames(cbind(ij, TT[[kk]])))
        k <- k+1
      }
    } else {
      MM[k] <- TT
      k <- k+1
    }
  }
  return(MM)
}

