#' Generate a plot of the duals
#' 
#' This function generates a dotchart of the duals (the shadow prices) from an `LP` object of class `Duals`.
#' For the function to work properly, the duals must have been calculated prior to the function call.
#'
#' @param x A object of class `Duals` with computed shadow prices from an `LP` object.
#' 
#' @export
plot.Duals <- function(x){
   dotchart(x$Duals, labels = rownames(x), pch = 16)
}