# ptools_financialfunctions.r
# Don Boyd
# 2/23/2017


#' @title Constant dollar amortization method
#'
#' @description \code{amort_cd} initial payment for a graduated annuity growing at rate g
#' @usage amort_cd(p, i, m, end)
#' @param p principal
#' @param i interest rate
#' @param m number of periods
#' @param end if TRUE, payment at the end of period
#' @details Returns POSITIVE payment for positive principal 
#' @return The amortization payment
#' @keywords amort_cd
#' @export
#' @examples
#' amort_cd(100, 0.08, 10, FALSE)
amort_cd <- function(p, i, m, end = FALSE) {rep(pmt(p, i, m, end), m)}


#' @title Constant percent amortization method
#'
#' @description \code{amort_cp} stream of payments for a graduated annuity growing at rate g
#' @usage amort_cp(p, i, m, g, end)
#' @param p principal
#' @param i interest rate
#' @param m number of periods
#' @param g growth rate
#' @param end if TRUE, payment at the end of period
#' @details Returns POSITIVE payment for positive principal 
#' @return The amortization payment
#' @keywords amort_cp
#' @export
#' @examples
#' amort_cp(100, 0.08, 10, .03, FALSE)
amort_cp <- function(p, i, m, g, end = FALSE) {gaip(p, i, m, g, end)*(g + 1)^(1:m - 1)}


#' @title Constant percent amortization method
#'
#' @description \code{amort_sl} stream of payments for straight-line payments
#' @usage amort_sl(p, i, m, end)
#' @param p principal
#' @param i interest rate
#' @param m number of periods
#' @param end if TRUE, payment at the end of period
#' @details Returns POSITIVE payment for positive principal 
#' @return The amortization payment
#' @keywords amort_sl
#' @export
#' @examples
#' amort_sl(100, 0.08, 10, FALSE)
#' 
#' amort_cd(100, 0.08, 10, FALSE)
#' amort_cp(100, 0.08, 10, 0.05, FALSE)
#' amort_sl(100, 0.08, 10, FALSE)
amort_sl <- function(p, i, m, end = FALSE){
  # # Straight line amortization method
  # See Winklevoss(1993, p101)
  if(end){
    sl <- i*(p - p*(0:(m - 1))/m) + p/m
  } else {
    d <- 1/(1+i)
    sl <- d*(p - p*(1:m)/m) + p/m}
  return(sl)
}


#' @title Function for choosing amortization methods
#'
#' @description \code{amort_LG} stream of payments for straight-line payments
#' @usage amort_LG(p, i, m, g, end, method)
#' @param p principal
#' @param i interest rate
#' @param m number of periods
#' @param g growth rate
#' @param end if TRUE, payment at the end of period
#' @param method one of c("cd", "cp", "sl")
#' @details Returns stream of payments
#' @return The stream of payments
#' @keywords amort_LG
#' @export
#' @examples
#' amort_LG(100, 0.08, 10, .03, method="cd")
#' amort_LG(100, 0.08, 10, .03, method="cp")
#' amort_LG(100, 0.08, 10, .03, method="sl")
amort_LG <- function(p, i, m, g, end = FALSE, method = "cd"){
  # amortize the gain/loss using specified amortization method
  switch(method,
         cd = amort_cd(p, i ,m, end),
         cp = amort_cp(p, i, m, g, end),
         sl = amort_sl(p, i, m, end)
  )
}


#' @title Graduated annuity initial payment (payments grow at constant annual rate).
#'
#' @description \code{gaip} initial payment for a graduated annuity growing at rate g
#' @usage gaip(p, i, n, g, end)
#' @param p principal
#' @param i interest rate
#' @param n number of periods
#' @param g growth rate of payments (annual)
#' @param end if TRUE, payment at the end of period
#' @details Returns POSITIVE payment for positive principal 
#' @return The initial payment in the graduated annuity
#' @keywords gaip
#' @export
#' @examples
#' gaip(100, 0.08, 10, 0.04)
#' gaip(100, 0.08, 10, 0.02, end = TRUE)
gaip <- function(p, i, n, g, end = FALSE){
  # p=principal, i=interest rate, n=periods, g=growth rate in payments
  # calculating gaip directly
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i) 
  k <- (1 + g)/(1 + i)
  a_sn <- (1 - k^n )/(1 - k)
  pmt <- p/a_sn
  return(pmt)
}


#' @title Percent difference, first value relative to second
#'
#' @description \code{pdiff} % difference, first value vs. second value
#' @usage pdiff(first, second)
#' @param first first value
#' @param second second value
#' @details Use, for example, to get percent difference of actual assets from expected assets
#' @return The percent difference
#' @keywords pdiff
#' @export
#' @examples
#' pdiff(100, 90)
pdiff <- function(first, second) {
  return((first - second)/second*100)
}


#' @title Amortization function
#'
#' @description \code{pmt} level amortization for principal p, interest rate i, n periods
#' @usage pmt(p, i, n, end)
#' @param p principal
#' @param i interest rate
#' @param n number of periods
#' @param end if true, payment at end of period, otherwise beginning
#' @details Returns POSITIVE payment for positive principal 
#' @return The level payment in the annuity
#' @keywords pmt
#' @export
#' @examples
#' pmt(100, 0.08, 10)
#' pmt(100, 0.08, 10, TRUE)
pmt <- function(p, i, n, end = FALSE){
  # amortization function with constant payment at each period 
  # p = principal, i = interest rate, n = periods. 
  # end: , if TRUE, payment at the end of period. 
  if(end) p <- p*(1 + i)
  a_n <- (1 - (1 + i)^(-n))/(1 - 1/(1 + i))
  pmt <- p / a_n
  return(pmt)  
}


#' @title Present value of an annuity-immediate (with pmt at end of period)
#'
#' @description \code{pvann} present value of annuity-immediate
#' @usage pvann(i, n, pmt)
#' @param i interest rate
#' @param n number of periods
#' @param pmt periodic payment
#' @details Returns a POSITIVE present value for positive payment
#' @return The initial payment
#' @keywords pvann
#' @export
#' @examples
#' pvann(.05, 30, 100)
pvann <- function(i, n, pmt) {
  # present value of an annuity-immediate (with pmt at end of period)
  if(i==0) pv <- n*pmt else pv <- pmt * ((1-(1+i) ^ (-n)) / i)
  return(pv)
}

