#' Function to combine constraints and limits
#' 
#' @param constraints A data frame containing constraints in the usual format
#' @param limits A data frame containing the limits as in the template
#' 
combine <- function(constraints, limits, objective){
   limits[limits%%1==0] <- NA
   for (side in c("Minimum", "Maximum")) {
      for (i in names(objective)) {
         lim <- limits[i, side]
         direction <- ifelse(side=="Minimum",">=","<=")
         if(!is.na(lim)){
            name <- paste(side, "_", i, sep="")
            if(any(rownames(constraints) %in% name)) next
            constraints[nrow(constraints)+1,] <- NA
            constraints[nrow(constraints),c("Direction","RHS")] <- c(direction, constraints["Area","RHS"])
            rownames(constraints)[nrow(constraints)] <- name
            for (j in names(objective)) {
               if(j == i){
                  constraints[name,j] <- 1/lim
               } else{
                  constraints[name,j] <- 0
               }
            }
         }
      }
   }
   return(constraints)
}
