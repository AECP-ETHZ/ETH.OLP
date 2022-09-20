#' Get the objective value from an `PMP` object
#' 
#' @param x An "LP"-class object whose objective value should be retrieved.
#' 
#' @export
objective <- function(x){
   return(x$objective_value)
}