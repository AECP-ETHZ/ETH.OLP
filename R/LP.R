#' Create an `LP` object for linear programming
#' 
#' This function is designed to set up an `LP`-class object, which offers a range of further functions to work with.
#' 
#' @usage `LP(c, A, b, ...)`
#' 
#' @param c A vector containing the target function coefficients.
#' @param A A matrix with the technical coefficients.
#' @param b A vector containing the ressource endowments.
#' @param maximize Direction of the optimisation task. If `TRUE`, the problem gets maximized, if `FALSE`, it gets minimized.
#' @param direction Direction of the constraints. By default, these are all "<=" (smaller than or equal to). However, it could also be ">=", "<", ">", or "=". Alternative directions need to be provided as a vector with the same length as `b`.
#' @param integer Vector of indices, which variables should only allowed to be integer?
#' @param binary Vector of indices, which variables should only allowed to be binary?
#' 
#' @note If the matrix A already has row- or column names, those are prioritised over element names of vectors c and b.
#' 
#' @example
## Create c, A, and b
#' c <- c(Wheat = 1000, Potato = 2000)
#' b <- c(Area = 20, Labour = 1800)
#' A <- matrix(c(1, 1, 40, 120), ncol = 2, byrow = TRUE)
#' ## Solve the problem
#' LP1 <- LP(c, A, b)
#' print(LP1)
#' 
#' @export
LP <- function(c, A, b, maximize = TRUE, directions = rep("<=", length(b)), integer = NULL, binary = NULL) {
   
   # Adopt the rownames from A, if they exist.
   if (!is.null(rownames(A))){
      names(b) <- rownames(A)
   } else {
      b <- rename(b, "b")
   }
   
   # Adopt the colnames from A, if they exist.
   if (!is.null(colnames(A))){
      names(c) <- colnames(A)
   } else{
      c <- rename(c, "c")
   }
   
   # Remove the dimension names of A
   A <- as.matrix(A)
   colnames(A) <- rownames(A) <- NULL
   
   # Solve the LP-problm using lpSolve
   lp_out <- lpSolve::lp(direction = ifelse(maximize, "max", "min"),
                         objective.in = c,
                         const.mat = A,
                         const.rhs = b,
                         const.dir = directions,
                         compute.sens = TRUE,
                         binary.vec = binary,
                         int.vec = integer)
   
   # Save the solution
   Solution <- lp_out$solution
   names(Solution) <- names(c)
   
   # Save the sensitivities of the target function coefficients
   Sensitivities <- data.frame("Lower" = lp_out$sens.coef.from, "Upper" = lp_out$sens.coef.to)
   rownames(Sensitivities) <- names(c)
   
   # Save the duals, replace the diverging values with +/- infinity
   Duals <- data.frame("Lower" = lp_out$duals.from, "Upper" = lp_out$duals.to, "Duals" = lp_out$duals)[1:length(b),]
   rownames(Duals) <- names(b)
   Duals[Duals < -1e+29] <- -Inf
   Duals[Duals > 1e+29] <- Inf
   
   # Save the duals of the target function variable as "Rho"
   Rho <- data.frame("Lower" = lp_out$duals.from, "Upper" = lp_out$duals.to, "Duals" = lp_out$duals)[-(1:length(b)),]
   rownames(Rho) <- names(c)
   Rho[Rho < -1e+29] <- -Inf
   Rho[Rho > 1e+29] <- Inf

   # Make a list
   LP_out <- list(maximize = maximize,
                  c = c,
                  A = A,
                  b = b,
                  directions = directions,
                  objective_value = lp_out$objval,
                  solution = Solution,
                  sensitivities = Sensitivities,
                  duals = Duals,
                  rho = Rho,
                  status = lp_out$status,
                  nx = length(c),
                  nb = length(b),
                  integer = lp_out$int.vec,
                  binary = lp_out$binary.vec)
   class(LP_out) <- "LP" # Assign class "LP"
   return(LP_out)
}