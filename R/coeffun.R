coeffun <- function(N, p.symbol="p^") {
  # This is for calculating the coefficients in the expansion p(p-1)(p-2)(p-3)...
  stopifnot(N >= 1)
  if(N == 1) {
    result <- 1
  } else {
    n <- N - 1
    n.required <- function(v, c) {
      cc <- combn(v, c)
      s <- apply(cc, 2, prod)
      return(sum(s))
    }
    a <- -(1:n)
    result <- rep(NA, N)
    result[1] <- 1
    for(i in 1:n) {
      result[i+1] <- n.required(a, i)
    }
  }

  names(result) <- paste0(p.symbol, N:1)
  return(result)
}
