#' Generic print function for `LP` objects
#' 
#' 
#' @export
print.LP <- function(LP){
   make_line <- function(char) cat(strrep(char, getOption("width")-5), "\n")
   A <- LP$A
   A[A==0] <- NA
   Problem <- rbind(c(LP$c, NA, NA), data.frame(A, LP$directions, LP$b))
   colnames(Problem) <- c(names(LP$c), "", "")
   rownames(Problem)[1] <- ifelse(LP$maximize,"Maximize:","Minimize:")
   Problem <- as.matrix(format.data.frame(Problem, digits = 3))
   Problem[grepl("NA",Problem)] <- NA
   Solution <- t(t(LP$solution))
   colnames(Solution) <- ""
   make_line("=")
   cat("LINEAR OPTIMIZATION PROBLEM:\n")
   print(Problem, digits = 3, na.print = "", quote = FALSE)
   make_line("-")
   cat("OPTIMAL SOLUTION:")
   print(Solution, digits = 3)
   cat(paste("\nAchieved function value with optimal solution:",round(LP$objective_value,2),"\n"))
   make_line("=")
}