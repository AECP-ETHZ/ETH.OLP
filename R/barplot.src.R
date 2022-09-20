#' Plot Standardized Regression Coefficients
#' 
#' This function generates a barplot of the "Standardized Regression Coefficients" in an `src` object.
#' The arrows on the bars indicate the upper and lower confidence intervals, 95% by default or any others which were manually set.
#'
#' @param x A "src" class object containing information about the standardized regression coefficients
#' @param ylim Vector containing the lower and upper limit of the y-axis.
#' @param col Bar fill color.
#' @param las Label style, see ?par() for further information.
#' @param names.arg Names below the bars.
#' @param main Cutom main title.
#' @param border Should a box be drawn around the plot?
#' 
#' @export
barplot.src <- function(x, ylim = NULL, col = NA, las = 1, names.arg = colnames(x$X), main  = NA, xlab = "", ylab = "SRC", border = TRUE, ...) {
   if(x$rank) X <- x$SRRC
   else X <- x$SRC
   if(is.null(ylim) & x$rank){
      ylim <- range(X[,4:5])+c(-0.05,0.05)
      ylim <- c(min(max(-1,ylim[1]),0),max(min(1,ylim[2]),0))
   }
   else if(is.null(ylim)){
      ylim <- range(X[,1])+c(-0.05,0.05)
      ylim <- c(min(max(-1,ylim[1]),0),max(min(1,ylim[2]),0))
   }
   b <- barplot(X[,1], ylim = ylim, col = col, space = rep(1,nrow(X)), names.arg = names.arg,
                las = las, ylab = ylab, xlab = xlab, main = ifelse(is.na(main),"Standardized Regression Coefficients",main), ...)
   abline(h=seq(-1,1,0.1),col=par()$fg,lty=3)
   barplot(X[,1], ylim = ylim, col = col, space = rep(1,nrow(X)), names.arg = names.arg,
           las = las, add = TRUE, xlab = xlab, ...)
   if(x$nboot>1){
      arrows(b, y0 = X[,4], y1 = X[,5], angle = 90, code = 3, length = 1.5/diff(range(b)))
      par(font.main = 3)
      if(is.na(main)) title(main = paste("\n\n(±",100*x$conf,"%-confidence-interval of bootstrapped values)",sep=""))
      par(font.main = 2)
   }
   if(border) box()
}