
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


#' @title Factor to numeric
#'
#' @description
#' \code{f2n} returns a numeric vector, converted from factor
#'
#' @usage f2n(fctr)
#' @param fctr factor that we want to convert to numeric
#' @details
#' Returns a pure numeric vector
#' @return numeric vector
#' @keywords f2n
#' @export
#' @examples
#' set.seed(1234)
#' fctr <- factor(sample(1:4, 50, replace=TRUE), levels=1:4)
#' fctr
#' f2n(fctr)
f2n <- function(fctr) as.numeric(levels(fctr)[fctr])


#' @title Show head and tail of a vector, matrix, table, data frame or function
#'
#' @description \code{ht} head and tail of a vector, matrix, table, data frame or function
#' @usage ht(df, nrecs=6)
#' @param df The object. No default.
#' @param nrecs number of records, rows, whatever to show at head and at tail
#' @details show head and tail of a vector, matrix, table, data frame or function
#' @keywords ht
#' @export
#' @examples
#' ht(mtcars, 4)
ht <- function (df, nrecs=6) {print(head(df, nrecs)); print(tail(df, nrecs))} # head tail


 
#' @title Describe memory usage and collect garbage
#'
#' @description \code{memory} describe memory usage and collect garbage
#' @usage memory(maxnobjs=5)
#' @param maxnobjs The number of objects to display. Default is 5.
#' @details Describes memory usage and collects garbage 
#' @keywords memory
#' @export
#' @examples
#' memory(4)
memory <- function(maxnobjs=5){
  # function for getting the sizes of objects in memory
  objs <- ls(envir = globalenv())
  nobjs <- min(length(objs), maxnobjs)
  
  getobjs <- function() {
    f <- function(x) utils::object.size(get(x)) / 1048600
    sizeMB <- sapply(objs, f)
    tmp <- data.frame(sizeMB)
    tmp <- cbind(name=row.names(tmp), tmp) %>% arrange(desc(sizeMB))
    # tmp <- tmp[order(-tmp$sizeMB), ]
    row.names(tmp) <- NULL
    tmp$sizeMB <- formatC(tmp$sizeMB, format="f", digits=2, big.mark=",", preserve.width="common")
    return(tmp)
  }
  
  print(paste0("Memory available: ", utils::memory.size(NA)))
  print(paste0("Memory in use before: ", utils::memory.size()))
  
  if(nobjs>0){
    print("Memory for selected objects: ")
    print(head(getobjs(), nobjs))
  }
  print(gc())
  print(paste0("Memory in use after: ", utils::memory.size()))
}



#' @title Convert NA to zero
#'
#' @description \code{na2zero} converts NA to zero
#' @usage na2zero(vec)
#' @param vec The vector to convert
#' @details Converts all NAs in a vector to zero.
#' @return The revised vector.
#' @keywords na2zero
#' @export
#' @examples
#' na2zero(NA)
# na2zero <- function(x) {x[is.na(x)] <- 0; return(x)}




