#' Print Sensitivities
#' 
#' Generic print function for "Sensitivity"
#' 
#' @export
print.Sensitivity <- function(x){
   print(as.matrix(x),digits=3,na.print = "")
}