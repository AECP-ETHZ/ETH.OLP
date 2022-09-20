#' Create an `PMP` object for positive mathematical programming
#' 
#' This function is designed to set up an `PMP`-class object, which offers a range of further functions to work with.
#' 
#' @usage `PMP(c, A, b, xstar, Q, ...)`
#' 
#' @param PMP An already specified positive mathematical programming model of class `PMP`
#' @param c A vector containing the new target function coefficients with which we want to run the model.
#' 
#' @export
predict.PMP <- function(PMP, c = PMP$c){
   
   nx <- PMP$nx
   nb <- PMP$nb
   Q <- PMP$Q
   b <- PMP$b
   A <- PMP$A
   xstar <- PMP$xstar
   maximize <- PMP$maximize
   
   f <- function(x){
      f <- t(c)%*%x + 1/2*t(x)%*%Q%*%x
      return(as.numeric(f)*ifelse(maximize, -1, 1))
   }
   
   g <- function(x) as.numeric(A%*%x)
   
   sink("file")
   results <- Rsolnp::solnp(pars = rep(mean(xstar), nx),
                            fun = f,
                            ineqfun = g,
                            ineqLB = rep(0,nb),
                            ineqUB = b,
                            LB = rep(0,nx))
   sink()
   
   if(results$convergence!=0){
      warning("There might be a problem with the model specification, the solver could not converge.")
   }
   
   solution <- results$pars
   names(solution) <- names(PMP$c)
   return(solution)
}