#' Plot a linear programming problem
#' 
#' This function can plot an "LP" object with exactly two variables.
#' If there are more than two variables, the function will plot only the first two, or any specified set of variables.
#' The legend will either use added constraint names or letters, beginning from A.
#' 
#' @param LP A "LP" class object which we wish to plot.
#' @param which A vector with exactly two indices of which variables should be plotted.
#'
#' @export
plot.LP <- function(LP, which = 1:2, col = NULL, lty = NULL, lwd = 1) {
   with(LP, {
      
      # Check if the "which" argument is valid
      if(length(which)!=2 | max(which)>nx){
         warning("The which argument was misspecified!\nWill work with which = c(1,2).")
         which <- 1:2
      }
      
      # Make a "Points" list with the Points defining the constraints (x0, x1, y0, y1)
      Points <- cbind(0, b/A[,which], 0)
      ID <- which(Points == "Inf", arr.ind = TRUE)
      Points[ID] <- NA
      xmax <- max(Points[,1:2], na.rm = TRUE); ymax <- max(Points[,3:4], na.rm = TRUE)
      if(nrow(ID)>0){
         for (i in 1:nrow(ID)) {
            if(ID[i,2]%in%3:4) Points[ID[i,1],] <- c(rep(max(Points[ID[i,1],],na.rm=TRUE),2),0,ymax)
            if(ID[i,2]%in%1:2) Points[ID[i,1],] <- c(0,xmax,rep(max(Points[ID[i,1],],na.rm=TRUE),2))  
         }
      }
      
      # Make the plot regiopn
      plot(NA, xlim = c(0,xmax), ylim = c(0,ymax),
           xlab = names(c)[which[1]], ylab = names(c)[which[2]],
           las = 1, xaxs = "i", yaxs = "i")
      
      # Define line types and colours, if not specified
      if(is.null(col)) cols <- rep(par()$fg, LP$nb)
      else cols <- rep(col, LP$nb)
      if(is.null(lty)) ltys <- 1:LP$nb
      else ltys <- rep(lty, LP$nb)
      
      # Add each constraint
      for (i in 1:nb) segments(x0 = Points[i,1],
                               x1 = Points[i,2],
                               y0 = Points[i,3],
                               y1 = Points[i,4],
                               lty = ltys[i],
                               col = cols[i],
                               lwd = lwd)

      # Make legend
      legend(ifelse(maximize,"topright","bottomleft"), title = "Constraints", col = cols, lty = ltys, legend = names(b), lwd = lwd)
   })
}
