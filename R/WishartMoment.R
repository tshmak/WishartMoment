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
#' @param p The parameter \eqn{p} in the Wishart distribution. The result should be the same if \eqn{p} >= \code{length(input)},
#' although the time taken may be much longer.
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
#' \tabular{rrrr}{
#'  \tab   1 \tab ac \tab ab*bc \cr
#' p^2 \tab  0 \tab 0  \tab   1 \cr
#' p^1 \tab 1 \tab 1 \tab    1
#' }
#' means \eqn{p + \sigma_{ac}p+\sigma_{ab}\sigma_{bc}(p+p^2)}{p+\sigma_{ac}*p+\sigma_{ab}*\sigma_{bc}*(p+p^2)}.
#'
#'
WishartMoment <- function(input, Diag1=FALSE, IdentityMatrix=FALSE,
                          Sigma=NULL, p=length(input), p.symbol="p^") {

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

  # This is the main engine for enumerating and tallying the possibilities
  index.matrix <- matrix(1, 2, npairs) # Initialization
  colnames(index.matrix) <- pairs
  res <- count(index.matrix = index.matrix, allpairs = Allpairs, index=index, Sigma=Sigma, p=p)

  Table <- data.frame(type=res$tally)

  ff <- factorial_design(npairs-1, levels=1:p) %x% matrix(1, length(Allpairs), 1)
  ff <- cbind(1, ff)

  colnames(ff) <- letters[1:npairs]
  # Table <- cbind(Table, ff)
  Table$unique <- sapply(1:nrow(Table), function(i) length(unique(ff[i,])))


  list.of.tables <- list()
  for(i in 1:max(Table$unique)) {
    # i <- 1
    ss <- subset(Table, unique == i)
    # if(i == 1) pp <- p else pp <- pp * (p-i+1)
    if(i == 1) pp <- 1 else pp <- pp * (p-i+1) # New trick to speed up computation.
    t <- table(ss$type) / pp
    f <- c(rep(0, npairs - i), coeffun(i))
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

