#' Simulate multiple "lp" model runs with varying coefficients
#' 
#' This function generates a dataset with `n` simulated observations.
#' If `range` is a single numeric value, the coefficients \eqn{\beta_{\text{sim}}} are sampled from an uniform distribution with:
#' \deqn{\beta_{\text{sim}} \sim U(\beta^*(1-r), \ \beta^*(1+r))}
#' where \eqn{\beta^*} are the original coefficients, and \eqn{r} is the specified `range` parameter.
#' Alternatively, a matrix with as many columns as variables can be passed to `range`, where the rows contain lower and upper limits for the uniform distributions.
#' The coefficients are sampled via an improved latin hypercube sampling method in order to achieve a better spread of the sampling points in higher dimensions.
#' 
#' @param LP A "lp" class object.
#' @param n Number of simulation runs. Default is n = 1000.
#' @param range The range, in which the coefficients of the linear programming problem might vary. Default is range = 0.5, which means the coefficients for the simulations are uniformly distributed in a range of ± 50 %. It can also be a matrix with upper and lower bounds for the uniform distribution.
#' 
#' @export
simulate.LP <- function(LP, n = 1000, range = 0.5){
   randoms <- lhs::improvedLHS(n, LP$nx)
   if(length(range)==1 & is.numeric(range)){
      data <- as.data.frame(t(apply(randoms, 1, function(x) LP$c+LP$c*(2*x-1)*range)))
   } else if(is.matrix(range) & is.numeric(range)){
      data <- as.data.frame(t(apply(randoms, 1, function(x) apply(range, 2, min)+x*apply(range, 2, function(d) max(d)-min(d)))))
   } else {
      stop("Please use either a single numeric value or a valid matrix as \"range\".")
   }
   Y <- apply(data, 1, function(x) {
      LP$c <- x
      LP <- update(LP)
      c(Objective = LP$objective_value, LP$solution)
      }) |> t()
   data <- list(Input = data, Output = as.data.frame(Y))
   class(data) <- "simdata"
   return(data)
}