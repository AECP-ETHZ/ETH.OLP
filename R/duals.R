#' Shadow prices
#' 
#' This function returns shadow prices (a.k.a. the duals) of an `LP` object.
#'
#' @param LP An "LP" class object
#' @param what Name of a constraint; if selected, the dual value of only this constrant will be returned as a single value.
#'  
#'
#' @export
duals <- function(LP, what = NA){
   x <- LP$duals
   class(x) <- c("Duals", "data.frame")
   if (is.na(what)){
      return(x)
   } else if (what %in% rownames(x)) {
      return(x[what,"Duals"])
   } else {
      stop("Selection was misspecified.")
   }
}