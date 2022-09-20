#' Convert a data frame to a `LP` problem
#' 
#' @param x A linear programming problem in tabular form.
#' 
#' @export 
as.LP <- function(x, integer = NULL, binary = NULL){
   nc <- ncol(x)-2
   nb <- nrow(x)-1
   maximize <- ifelse(tolower(rownames(x)[1])=="max", TRUE, FALSE)
   directions <- x[-1,nc+1]
   b <- x[-1,nc+2]
   c <- as.numeric(x[1,1:nc])
   A <- x[-1,1:nc]
   LP(c = c, A = A, b = b, maximize = maximize, directions = directions, integer = integer, binary = binary)
}