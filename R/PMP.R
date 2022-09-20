#' Create an `PMP` object for positive mathematical programming
#' 
#' This function is designed to set up an `PMP`-class object, which offers a range of further functions to work with.
#' 
#' @usage `PMP(c, A, b, xstar, Q, ...)`
#' 
#' @param c A vector containing the target function coefficients.
#' @param A A matrix with the technical coefficients.
#' @param b A vector containing the ressource endowments.
#' @param maximize Direction of the optimisation task. If `TRUE`, the problem gets maximized, if `FALSE`, it gets minimized.
#' @param Q Q-matrix with the elaticities. If not specified, the Q-matrix is directly estimated.
#' 
#' @export
PMP <- function(c, A, b, xstar, Q = NULL, maximize = TRUE){
   
   # Save variable and constraint vector lengths
   nx <- length(c)
   nb <- length(b)
   
   # If there is no Q-matrix specified, estimate one
   if(is.null(Q)) Q <- -diag(c/(xstar))
   dimnames(Q) <- list(names(c), names(c))
   
   # Create a list with all the important values
   PMP <- list(c = c,
               A = A,
               b = b,
               xstar = xstar,
               Q = Q,
               nx = nx,
               nb = nb,
               maximize = maximize,
               impossible = names(b)[which(A%*%xstar>b)])
   class(PMP) <- "PMP"
   
   return(PMP)
}
