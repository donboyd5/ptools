
#' Get long-run geometric mean associated with arithmetic mean
#' 
#' This is the exact calculation rather than the gm = r - var /2 approximation.
#' See https://web.stanford.edu/~wfsharpe/mia/rr/mia_rr3.htm
#' 
#' @param r Numeric vector of arithmetic returns.
#' @param sd standard deviation
#' @return The long-run geometric mean, given \code{r} and \code{sd}.
#' @export
#' @examples
#' r_to_gm(.08, .12)
r_to_gm <- function(r, sd){
  # Get long-run geometric mean associated with arithmetic mean
  # https://web.stanford.edu/~wfsharpe/mia/rr/mia_rr3.htm
  # this is the exact calculation rather than the gm = r - var /2 approximation
  gm <- {(1 + r)^2 - sd^2}^(1/2) - 1
  return(gm)
}


#' Get arithmetic mean associated with a long-run geometric mean
#' 
#' This is the exact calculation rather than the ar = gr + var /2 approximation
#' See https://web.stanford.edu/~wfsharpe/mia/rr/mia_rr3.htm
#' 
#' @param gm Numeric vector of long-run geometric-mean returns.
#' @param sd standard deviation
#' @return The arithmetic mean, given \code{gm} and \code{sd}.
#' @export
#' @examples
#' gm_to_r(0.07331263, .12)
gm_to_r <- function(gm, sd){
  # get arithmetic mean associated with a long-run geometric mean
  # https://web.stanford.edu/~wfsharpe/mia/rr/mia_rr3.htm
  # this is the exact calculation rather than the ar = gr + var /2 approximation
  r <- {(1 + gm)^2 + sd^2}^(1/2) - 1
  return(r)
}


#' Get expected geometric mean at year n, for a given arithmentic mean r
#' 
#' 
#' @param r Numeric vector of arithmetic returns
#' @param sd standard deviation
#' @param n year
#' @return Expected geometric mean at year \code{n}, given \code{r} and \code{sd}.
#' @export
#' @examples
#' r_to_gm.n(.08, .12, 1)
#' r_to_gm.n(.08, .12, 10)
#' r_to_gm.n(.08, .12, 100)
#' r_to_gm.n(.08, .12, 1000000)
#' # compare to the exact calculation -- hmmm...
#' r_to_gm(.08, .12)
#' # compare to the approximation
#' .08 - .12^2 / 2
r_to_gm.n <- function(r, sd, n){
  # get expected geometric mean at year n, for a given arithmentic mean r
  # r	expected arithmetic return
  # gm.n	expected geometric return at n
  # var	variance of r
  # a	= (1 + r)^2
  # b	= (1 - n) / 2*n
  # gm.n = (1 + r) * (1+ var /a) ^ b - 1
  var <- sd^2
  a <- (1 + r)^2
  b <- (1 - n) / (2 * n)
  gm.n <- (1 + r) * (1 + var / a)^b - 1
  return(gm.n)
}


#' Find the arithmetic mean needed to result in a particular long-run geometric mean gm
#' 
#' Use brute force by minimizing error
#' 
#' @param gm Numeric vector of long-run geometric-mean returns.
#' @param sd standard deviation of the arithmetic mean
#' @return The approximated arithmetic mean, given \code{gm} and \code{sd}.
#' @export
#' @examples
#' gm.lr_to_r(0.07331263, .12)
#' # compare to exact calculation
#' gm_to_r(0.07331263, .12)
gm.lr_to_r <- function(gm, sd){
  # find the arithmetic mean needed to result in a particular long-run geometric mean gm.lr
  # use brute force by minimizing error
  obj <- function(armean, sd, target){
    gmean <- r_to_gm.n(armean, sd, 1e9)
    obj <- (gmean - gm)^2
    return(obj)
  }
  opt <- optim(gm, obj, sd=sd, target=gm, method="BFGS")
  return(opt$par)
}


