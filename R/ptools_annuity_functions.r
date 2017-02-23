


#' @title Calculate temporary annuity values from age x to retirment age (fixed end)
#'
#' @description \code{get_tla} present value of annuity-immediate
#' @usage get_tla(px, i, scale, cashflow)
#' @param px a vector of composite survivial probs from age x to x + n - 1. Length = n
#' @param i discount rate, scalar
#' @param scale numeric vector of length px
#' @param cashflow The cashflow of the annuity can be specified by this argument. This is useful when calculating PV of benefits with COLA.
#' @details  Suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
#' The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN.
#' The length of px is f - a1 
#' Note that the last element is redundant, just used as a place holder. 

#' @return an n vector storing the value of temporary life annuities from age x to age x + n - 1
#' @keywords get_tla
#' @export
#' @examples
#' get_tla(rep(0.98, 55), 0.08)
get_tla <- function(px, i, scale = rep(1, length(px)), cashflow = rep(1, length(px))){
  # suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
  # The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN. 
  # The length of px is f - a1 
  # Note that the last element is redundant, just used as a place holder. 
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n
  # i:  discount rate, scalar
  # scale: how the annuity scale up over time. eg: 
  #        1) salary scale. default is a n vector of 1, meaning no salary scale. used when calculating career based annuity
  #        2) simple COLA scale: COLA increasing at a fixed percentage very year.  
  # cashflow: The cashflow of the annuity can be specified by this argument. This is useful when calculating PV of benefits with COLA.
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla <- numeric(length(px))
  n <- length(tla)
  
  for(j in 1:n){
    v   <- 1/(1 + i)^(0:(n - j)) # dicount vector
    if(j < n) pxr <- cumprod(c(1, px[j:(n - 1)])) else pxr = 1      # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- scale[j:n]/scale[j]                # scale
    tla[j] = sum(SS * v * pxr)                # computing annuity value at j
  } 
  return(tla)
}


#' @title Calculate temporary annuity values from a fixed entry age y to x (fixed start)
#'
#' @description \code{get_tla2} present value of annuity-immediate
#' @usage get_tla2(px, i, sx)
#' @param px a vector of composite survivial probs from age x to x + n - 1. Length = n
#' @param i discount rate, scalar
#' @param sx salary scale. default is an n vector of 1, meaning no salary scale. 
#' @details  Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
#' This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 
#'
#' Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
#' For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
#' eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
#' eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
#' 
#' Note that the last 2 survival rates and last salary scale are redundant in the calculation, they are just used as place holders. 
#' calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
#' because the last annuity payment is paid at the begining of 64. )
#' @return an n vector storing the value of temporary life annuities from age x to age x + n - 1.
#' @keywords get_tla2
#' @export
#' @examples
#' get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65))
get_tla2 = function(px, i, sx = rep(1, length(px))){
  # Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
  # This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 
  
  # Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
  # For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
  # eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
  # eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
  # Note that the last 2 survival rates and last salary scale are redundant in the calculation, they are just used as place holders. 
  #(calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
  # because the last annuity payment is paid at the begining of 64. )
  
  # inputs:
  # px: an vector of composite survivial probs from age x to x + n - 1. Length = n. The minimum length of px allowed is 2. 
  # i:  discount rate, scalar
  # sx: salary scale. default is an n vector of 1, meaning no salary scale. 
  # output:
  # tla: an n vector storing the value of temporary life annuities from age x to age x + n - 1.
  tla = numeric(length(px))
  n = length(tla)
  
  # tla[1] will be kept as 0, next calculate tla[2:n]:
  for(j in 1:(n - 1)){
    v   <- 1/(1 + i)^(0:(j - 1))                                  # dicount vector
    if(j == 1) pxr <- 1 else pxr <- cumprod(c(1, px[1:(j - 1)]))  # survival probability to retirment at age x. Note that participant always survives at the beginning of age x
    SS  <- sx[1:j]/sx[1]                                          # salary scale
    tla[j + 1] = sum(SS * v * pxr)                                # computing annuity value at j;
  } 
  return(tla) 
}


#' @title Calculate temporary annuity values from a fixed entry age y to x (fixed start), simpler implementation
#'
#' @description \code{get_tla2a} present value of annuity-immediate
#' @usage get_tla2a(px, i, sx)
#' @param px a vector of composite survivial probs from age x to x + n - 1. Length = n
#' @param i discount rate, scalar
#' @param sx salary scale. default is an n vector of 1, meaning no salary scale. 
#' @details  Suppose the age corresponding to px runs from a1 to aN, y = a1 (eg. age 30:65, y = 30)
#' This function conputes a..{y, x - y} and s_a..{y, x - y}, x ruuning from a1 to aN. 

#' Note that when x = a1 = y, we define a..{y, 0} = 0. so the first element is always 0. 
#' For age x > y, the number of years receiviing annuity is x - y, the resulting annuity value will be placed at age x. 
#' eg1: x = 31, y = 30,  annuity ($1) received only once at age 30, but the resulting annuity value will be placed at age 31.
#' eg2: x = 65, y = 30, annuity received from age 30 to 64(total 65 years), the resulting annuity value will be placed at age 65
#' Note that the last 2 survival rates and last salary scale are redundant in the calculation, they are just used as place holders. 
#' calculating the value of annuity running from 30 to 64 only involves survival rate from 30 to 63, 
#' because the last annuity payment is paid at the begining of 64. )

#' @return an n vector storing the value of temporary life annuities from age x to age x + n - 1.
#' @keywords get_tla2a
#' @export
#' @examples
#' get_tla2a(rep(0.98, 65), 0.08, rep(1.1, 65))
#' get_tla2(rep(0.98, 65), 0.08, rep(1.1, 65))
get_tla2a <- function(px, i, sx = rep(1, length(px))){
  
  n <- length(px)
  tla <- numeric(n)
  v <- 1/(1 + i)
  
  tla[-1] <- cumsum(cumprod(c(1,px[1:(n-2)])* v * sx[1:(n - 1)]/sx[1])/v)
  
  return(tla)   
}


#' @title PVFB of term costs
#'
#' @description \code{get_PVFB} present value of annuity-immediate
#' @usage get_PVFB(px, v, TC)
#' @param px numeric vector of length n. Probability of survival at time 1 through n
#' @param v numeric. discount factor 1/(1 + i)
#' @param TC numeric vector of length n. A series of term costs. Term costs are valued at the begninning of period.
#' @details Returns a POSITIVE present value for positive payment
#' @return vector length n, with PVFBs of fixed end contracting windows of TC
#' @keywords get_PVFB
#' @export
#' @examples
#' get_PVFB(.9^(1:20), 1/(1.08), 10*(1.03)^(1:20))
get_PVFB <- function(px, v, TC){ # present values of subsets of TC (fixed end)
  # This function compute the total present value of TC[j:n] at the beginning of time j, with j running from 1 to n. 
  # The function can be used to calculate PVFB of term costs of ancillary benefits or retirement benefits with multiple
  # retirement ages. 
  
  # Inputs
  # px: numeric vector of length n. Probability of survival at time 1 through n
  # v : numeric. discount factor 1/(1 + i)
  # TC: numeric vector of length n. A series of term costs. Term costs are valued at the begninning of period. 
  # Returns
  # PVFBs of fixed end contracting windows of TC. 
  
  n <- length(px)
  
  PVFB <- sapply(seq_len(n), function(j) ifelse(j == n, TC[j], sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n])))
  
  return(PVFB)
}

# # 1.4 NC of UC and PUC
# #' @title Present value of an annuity-immediate (with pmt at end of period)
# #'
# #' @description \code{pvann} present value of annuity-immediate
# #' @usage pvann(i, n, pmt)
# #' @param i interest rate
# #' @param n number of periods
# #' @param p periodic payment
# #' @details Returns a POSITIVE present value for positive payment
# #' @return The initial payment
# #' @keywords pvann
# #' @export
# #' @examples
# #' pvann(.05, 30, 100000)
# get_NC.UC <- function(px, v, TC){
#   # This function is a variation of get_PVFB. It is used to calculate NC under UC and PUC methods.
#   # Below we explain the major difference between get_NC.UC and get_PVFB:
#   # 1. Why TC[(j + 1):n]?  Remember NC is the discounted value of benefit accrual. During age x, the individual can 
#   #    accrue benefit for age x + 1 to r'', so the corresponding elements in TC are TC[(j + 1):n]. Note that 
#   #    TC[j+1] is gx.r(j+1)*qxr(j+1)*ax(j+1) in PUC. 
#   # 2. Why start discounting from the 1st element? Since at j the individual starts accruing benefit from j + 1, 
#   #    we need to discount the value in j + 1.  
#   # Note The last elements (at age r'') of the result is NA by construction. 
#   # px must be survival probability from min(age) to r''.
#   # TC must be defined as  
#   #  UC for retirement:    gx.r(x) * qxr(x) * ax(x), x running from y (entry age) to r'' (eg. 20 to 65 in Winklevoss book)
#   #  PUC for retirement:   Bx(x)/(x - y) * gx.r(x) * qxr(x) * ax(x), x running from entry age (y) to r'' (0 when x = y)      
#   #  PUC for vested terms: Bx(x)/(x - y) * gx.r(x) * qxt.a * lead(pxRm) * v^(r.max - age) * ax[age == r.max]
#   n <- length(px) # n is r''
#   
#   Fun_NC <- function(j) ifelse(j == n, NA, sum(cumprod(px[j:(n - 1)]) * v^(1:(n-j)) * TC[(j + 1):n]))
#   
#   NC <- sapply(seq_len(n), Fun_NC)
#   
#   return(NC)
# }
# 
# # 1.5 AL of PUC
# #' @title Present value of an annuity-immediate (with pmt at end of period)
# #'
# #' @description \code{pvann} present value of annuity-immediate
# #' @usage pvann(i, n, pmt)
# #' @param i interest rate
# #' @param n number of periods
# #' @param p periodic payment
# #' @details Returns a POSITIVE present value for positive payment
# #' @return The initial payment
# #' @keywords pvann
# #' @export
# #' @examples
# #' pvann(.05, 30, 100000)
# get_AL.PUC <- function(px, v, TC){
#   # This function is a variation of get_PVFB. It is used to calculate AL under PUC methods.
#   
#   # Note that the only difference between get_AL.PUC and get_PVFB is that TC[j] is multiplied by (j - 1)
#   
#   # Note that y(entry age) corresponds to index 1 and age x corresponds to index j, so at age x the individual
#   # has been accruing benefits for x - y years, which is equal to j - 1 years. (eg. Assuming y = 20, then when x = 21 and j = 2 the 
#   # individual have accrued benefits for 1 year (x - y = 21 - 20 and j - 1 =  2 - 1).
#   
#   # TC must be defined the same way as in get_NC.UC. 
#   # the first element (age y) should be zero, the last element should be the same as the last element in TC.
#   
#   n <- length(px) # n is r'' - y + 1
#   AL <- sapply(seq_len(n), 
#                function(j) ifelse(j == n, TC[j]*(j-1), sum(cumprod(c(1, (px[j:(n - 1)] * v))) * TC[j:n] * (j - 1)))
#   )
#   return(AL)
# }
