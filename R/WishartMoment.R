#' @title Function to derive the higher moments of a Wishart distribution symbolically
#'
#' @description If \eqn{r_{ab}, r_{bc}, etc.} have a Wishart distribution, this program
#' calculates \eqn{E(r_{ab}r_{bc}\ldots)}{E(r_{ab}*r_{bc}*\ldots )} symbolically. Currently,
#' this can only calculate up to the 6th order.
#'
#' @param input For \eqn{E(r_{ab}r_{bc})}{E(r_{ab}*r_{bc})}, for example, type \code{c("ab", "bc")}. See
#' the details section below. Note that variables must be denoted by single letters.
#' @param Diag1 Forces \eqn{\Sigma_{11}, \Sigma_{22}}, etc. to be 1.
#' @param IdentityMatrix Uses the Identity matrix as \eqn{\Sigma}
#' @param Sigma An arbitrary matrix for matrices with constraints. This must be a symmetric
#' matrix with row and column names corresponding to the variable names. The entries should
#' be either 0, 1, or a prime number. Equal prime number will constrain the entries to be
#' the same.
#' @param p.symbol The name of the parameter to denote the number of observations
#'
#' @details Assume a,b,c are multivariate Normally-distributed vectors with
#' each element independently having mean 0 and variance \eqn{\sigma_{aa}},
#' \eqn{\sigma_{bb}},  \eqn{\sigma_{cc}}, respectively.
#' Assume \deqn{Cov(a_i, b_i) = \sigma_{ab}}
#' \deqn{Cov(a_i, c_i) = \sigma_{ac},   etc.} and
#' \deqn{Cov(a_i, b_j) = Cov(a_i, c_j) = 0,   etc. for i != j.}
#' Let
#' \deqn{r_{aa} = \sum a_i a_i}
#' \deqn{r_{ab} = \sum a_i b_i, etc.,}
#' then
#' \deqn{r_{aa}, r_{ab}, \ldots \sim Wishart(\Sigma, p)}{r_{aa}, r_{ab}, \ldots ~ Wishart(\Sigma, p)}
#' where \eqn{\Sigma_{11} = \sigma_{aa}, \Sigma_{12} = \sigma_{ab}, \ldots}.
#'
#' This program allows you to calculate
#' \deqn{E(r_{aa}r_{ab}r_{bc}\ldots)}{E(r_{aa}*r_{ab}*r_{bc}\ldots)}
#' symbolically for arbitrary moments of \eqn{r_{??}}
#'
#'
#' @export
#'
#' @examples
#' WishartMoment(c("ab", "cd"))
#' WishartMoment(c("ab", "bb", "bc"))
#'
#' @return A matrix of counts corresponding to the number of particular terms. For example,
#' \tabular{rrr}{
#'  \tab   ac \tab ab*bc \cr
#' p^2 \tab  0  \tab   1 \cr
#' p^1 \tab  1 \tab    1
#' }
#' means \eqn{\sigma_{ac}p+\sigma_{ab}\sigma_{bc}(p+p^2)}{\sigma_{ac}*p+\sigma_{ac}*\sigma_{bc}*(p+p^2)}.
#'
#'
WishartMoment <- function(input, Diag1=TRUE, IdentityMatrix=FALSE,
                          Sigma=NULL, p.symbol="p^") {

  # constants <- c("p", "a", "m", "n")
  # cons <- primes[1:length(constants)]
  # names(cons) <- constants
  pairs <- input
  degree <- length(pairs)
  if(degree > 6) stop("Order higher than 6 is not currently possible.")
  if(is.null(Sigma)) {
    Sigma <- getSigma(pairs,Diag1 = Diag1, IdentityMatrix=IdentityMatrix)
  }
  nvars <- ncol(Sigma)
  npairs <- length(pairs)

  Allpairs <- allpairs(1:(npairs*2)) # This gives all possible split of 1:(npairs*2) by pairs.
  index <- getIndex(pairs, Sigma) # Find the index of pairs given Sigma
  colnames(index) <- pairs
  p <- npairs

  # This is the main engine for enumerating and tallying the possibilities
  index.matrix <- matrix(1, 2, npairs) # Initialization
  colnames(index.matrix) <- pairs
  res <- count(index.matrix = index.matrix, allpairs = Allpairs, index=index, Sigma=Sigma, p=p)

  Table <- data.frame(type=res$tally)
  ff <- as.data.frame(factorial_design(npairs, levels=1:p) %x% matrix(1, length(Allpairs), 1))
  ff <- ff[ff[,1] == 1, ,drop=FALSE] # New trick
  colnames(ff) <- letters[1:npairs]
  Table <- cbind(Table, ff)
  Table$unique <- sapply(1:nrow(Table), function(i) length(unique(as.matrix(ff)[i,])))

  fun <- function(N) {
    # This is for calculating the coefficients in the expansion p(p-1)(p-2)(p-3)...
    stopifnot(N >= 1)
    if(N == 1) return(1)
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
    return(result)
  }

  list.of.tables <- list()
  for(i in 1:max(Table$unique)) {
    # i <- 1
    ss <- subset(Table, unique == i)
    # if(i == 1) pp <- p else pp <- pp * (p-i+1)
    if(i == 1) pp <- 1 else pp <- pp * (p-i+1) # New trick to speed up computation.
    t <- table(ss$type) / pp
    f <- c(rep(0, npairs - i), fun(i))
    m <- outer(f, t)
    rownames(m) <- paste0(p.symbol, npairs:1)
    # class(m) <- "CountTable"
    list.of.tables[[i]] <- m
  }
  # print(list.of.tables)
  # print(Sigma)
  # list.of.tables$remove0 <- remove0
  out <- do.call("mergeTables", list.of.tables)
  colnames(out) <- renameColumns(colnames(out), Sigma)
  attr(out, "Sigma") <- Sigma
  return(out)

}

