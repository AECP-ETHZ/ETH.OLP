#' Target function sensitivity
#' 
#' This function returns the sensitivities of the target function coefficients from an `LP` object.
#'
#' @param LP An "lp" class object with computed sensitivities.
#'
#' @export
sensitivity <- function(LP){
   sens <- LP$sensitivities
   #sens[sens<=0] <- NA
   x <- data.frame("Coefficient" = LP$c, sens, "Value" = LP$solution)
   x[x< -1e+25] <- -Inf
   x[x>1e+25] <- Inf
   class(x) <- c("Sensitivity", "data.frame")
   return(x)
}