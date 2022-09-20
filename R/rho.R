#' Duals of the target function coefficients
#' 
#' This function returns the duals referring to coefficients (not constraints) of an `LP` object.
#'
#' @param LP An `LP` class object
#' @param what Name or index a variable; if selected, the dual value of only this variable will be returned as a single value.
#' @param bounds Should the upper and lower bounds of the duals be returned?
#'  
#' @export
rho <- function(LP, what = NULL, bounds = FALSE){
   x <- LP$rho
   class(x) <- c("Rho", "data.frame")
   if (is.null(what)){
      if(bounds){
         return(x)
      } else{
         rho <- x[,3]
         names(rho) <- rownames(x)
         return(rho)
      }
   } else if (!all(is.na(x[what,]))) {
      rho <- x[what,"Duals"]
      names(rho) <- rownames(x[what,])
      return(rho)
   } else {
      stop("Selection was misspecified.")
   }
}