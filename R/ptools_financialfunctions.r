# ptools_financialfunctions.r
# Don Boyd
# 6/2/2014


#' @title Present value of an annuity-immediate (with pmt at end of period)
#'
#' @description \code{pvann} present value of annuity-immediate
#' @usage pvann(i, n, pmt)
#' @param i interest rate
#' @param n number of periods
#' @param p periodic payment
#' @details Returns a POSITIVE present value for positive payment
#' @return The initial payment
#' @keywords pvann
#' @export
#' @examples
#' pvann(.05, 30, 100000)
pvann<-function(i, n, pmt){
  # present value of an annuity-immediate (with pmt at end of period)
  if(i==0) pv<-n*pmt else pv<-pmt*((1-(1+i)^(-n))/i)
  return(pv)
}

#' @title Graduated annuity initial payment (payments grow at constant annual rate).
#'
#' @description \code{gaip} initial payment for a graduated annuity growing at rate g
#' @usage gaip(p, i, g, n)
#' @param p principal
#' @param i interest rate
#' @param g growth rate of payments (annual)
#' @param n number of periods
#' @details Returns POSITIVE payment for positive principal 
#' @return The initial payment in the graduated annuity
#' @keywords gaip
#' @export
#' @examples
#' gaip(450, .07, .03, 20)
gaip<-function(p, i, g, n){
  # returns POSITIVE payment for positive principal 
  imgc<-(1+i)/(1+g)-1 # i minus g, compounded
  gaf<- (1+i)/(1-pvann(imgc, n-1, -1)) # grad annuity factor - note "minus" pvann to adjust sign vs Excel
  return(gaf*p)
}

