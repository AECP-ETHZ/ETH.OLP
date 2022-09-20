#' Plot Simulated Data for a `LP` Sensitivity Analysis
#' 
#' This function generates scatterplot of the data simulated by the `simulate.LP()` function.
#' 
#' @param simdata A `simdata` class object to be inspected.
#' @param y Name of the dependent variable. By default, this is the value of the objective function ("Objective"). Can be any other column name in `simdata$Output`.
#'
#' 
#' @export
plot.simdata <- function(simdata, y = "Objective"){
   X <- colnames(simdata$Input)
   for (i in 1:length(X)) {
      readline(prompt="Press `ENTER` for the next plot: ")
      x <- X[i]
      plot(simdata$Output[,y] ~ simdata$Input[,x], ylab = y, xlab = x, cex = 0.5, pch = 16, xaxs = "i", main = paste(i,"/",length(X),sep=""))
      model <- lm(simdata$Output[,y] ~ simdata$Input[,x])
      abline(model, lwd = 2)
      grid(col=par()$fg)
   }
}