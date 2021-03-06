% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ptools_annuity_functions.r
\name{get_tla}
\alias{get_tla}
\title{Calculate temporary annuity values from age x to retirment age (fixed end)}
\usage{
get_tla(px, i, scale, cashflow)
}
\arguments{
\item{px}{a vector of composite survivial probs from age x to x + n - 1. Length = n}

\item{i}{discount rate, scalar}

\item{scale}{numeric vector of length px}

\item{cashflow}{The cashflow of the annuity can be specified by this argument. This is useful when calculating PV of benefits with COLA.}
}
\value{
an n vector storing the value of temporary life annuities from age x to age x + n - 1
}
\description{
\code{get_tla} present value of annuity-immediate
}
\details{
Suppose the age corresponding to px runs from a1 to aN, and f = aN + 1 (eg. age 30:64, f = 65)
The function computes a..{x, f - x} and s_a..{x, f - x}, x runing from a1 to aN.
The length of px is f - a1 
Note that the last element is redundant, just used as a place holder.
}
\examples{
get_tla(rep(0.98, 55), 0.08)
}
\keyword{get_tla}
