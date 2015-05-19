
#' @title Character to numeric
#'
#' @description
#' \code{cton} returns a numeric vector, converted from character
#'
#' @usage cton(cvec)
#' @param cvec character vector that we want to convert to numeric
#' @details
#' Eliminates "," "$" "%"  Other chars will become NA
#' @return numeric vector
#' @keywords cton
#' @export
#' @examples
#' cvec <- c("12345678", "123.45678", "$123,456.78", "A123,456.78")
#' cton(cvec)
cton <- function (cvec) as.numeric(gsub("[ ,$%]", "", cvec))  # character to numeric, eliminating "," "$" "%". chars will become NA

# f2n <- function(x) as.numeric(levels(x)[x])
# 
# ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail
# 
# memory<-function(maxnobjs=5){
#   # function for getting the sizes of objects in memory
#   objs <- ls(envir=globalenv())
#   nobjs <- min(length(objs),maxnobjs)
#   tmp <- as.data.frame(sapply(objs, function(x) object.size(get(x)))/1048600)
#   tmp <- data.frame(name=row.names(tmp), sizeMB=tmp[,1])
#   tmp <- tmp[order(-tmp$sizeMB),]
#   tmp$sizeMB <- formatC(tmp$sizeMB,format="f",digits=2,big.mark=",",preserve.width="common")
#   print(paste0("Memory available: ", memory.size(NA)))
#   print(paste0("Memory in use before: ", memory.size()))
#   print("Memory for selected objects: ")
#   print(head(tmp, nobjs))
#   print(gc())
#   print(paste("Memory in use after: ", memory.size(), sep=""))
# }
# 
# na2zero <- function(x){x[is.na(x)] <- 0 ;return(x)}




