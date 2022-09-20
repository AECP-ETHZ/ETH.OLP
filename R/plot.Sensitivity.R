#' Generate a plot of the variable sensitivities
#' 
#' This function generates a dotchart of the sensitivities from an `LP` object of class `Sensitivity`.
#' For the function to work properly, the sensitivities must have been calculated prior to the function call.
#'
#' @param x A object of class `Sensitivity` with computed sensitivities from an `LP` object.
#' 
#' @export
plot.Sensitivity <- function(x){
   linewidth = 1
   range <- range(x[,1:3][x[,1:3]!=Inf & x[,1:3]!=-Inf], na.rm = TRUE)
   bar <- function(center, x0, x1, i, col, pch, lty){
      segments(x0 = x0, x1 = x1, y0 = i, col = par()$bg, lty = 1, lwd = linewidth)
      segments(x0 = x0, x1 = x1, y0 = i, col = par()$fg, lty = lty, lwd = linewidth)
      points(x = center, y = i, pch = pch, col = par()$fg, cex = 1, lwd = linewidth)
   }
   ticks <- function(x, i) segments(na.omit(as.numeric(x)), y0 = rep(i-0.1, length(x)), y1 = rep(i+0.1, length(x)), lwd = linewidth)
   dotchart(x[,1], xlim = range, labels = rownames(x), ylim = c(1,row(x)), lcolor = "grey", frame.plot = TRUE)
   for (i in 1:nrow(x)) {
      currentpch <- ifelse(x[i,"Value"]==0, 1, 16)
      if(anyNA(x[i,1:3])){
         bar(center = x[i,1], x0 = x[i,3], x1 = x[i,3]+diff(range)/10, i = i, pch = currentpch, lty = 2)
         ticks(x[i,2:3], i)
      }
      else if(any(x[i,]==Inf,na.rm=TRUE)){
         bar(center = x[i,1], x0 = x[i,2], x1 = 1e+30, i = i, pch = currentpch, lty = 2)
         ticks(x[i,2:3], i)
         
      } else{
         bar(center = x[i,1], x0 = x[i,2], x1 = x[i,3], i = i, pch = currentpch, lty = 1)
         ticks(x[i,2:3], i)
      }
   }
}
