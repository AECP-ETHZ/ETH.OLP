#' Print Duals
#' 
#' Generic print function for "Duals" objects
#' 
#' @export
print.Duals <- function(x){
   x <- as.matrix(x)
   x[is.infinite(x)] <- NA
   print(as.matrix(x),digits=2,na.print = "")
}