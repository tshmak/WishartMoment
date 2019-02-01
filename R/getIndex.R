getIndex <- function(pairs, Sigma) {
  p <- strsplit(pairs, split="")
  stopifnot(all(sapply(p, length) == 2))
  
  m <- matrix(NA, 2, length(p)) 
  cols <- colnames(Sigma)
  for(i in 1:length(p)) {
    w <- sapply(p[[i]], function(c) which(cols == c))
    if(is.list(w)) {
      stop("the variables don't match.")
    }
    m[,i] <- w
  }
  return(m)
}