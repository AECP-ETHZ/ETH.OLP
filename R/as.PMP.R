#' Convert an `LP` object to a `PMP` object
#' 
#' @param LP A linear programming problem as an `LP` object
#' @param xstar Observations of `x`
#' @param Q The Q-matrix for the PMP problem
#'  
#' 
#' @export 
as.PMP <- function(LP, xstar, Q = NULL){
   PMP(c = LP$c, A = LP$A, b = LP$b, xstar = xstar, Q = Q)
}
