#' Generic print function for `PMP` objects
#' 
#' @param PMP
#' 
#' @export
print.PMP <- function(PMP){
   make_line <- function(char) cat(strrep(char, getOption("width")-5), "\n")
   cosine <- function(a,b) sum(a*b)/sqrt(sum(a^2)*sum(b^2))
   
   LP <- LP(PMP$c, PMP$A, PMP$b, PMP$maximize)
   A <- LP$A
   A[A==0] <- NA
   Problem <- rbind(c(LP$c, NA, NA), data.frame(A, LP$directions, LP$b))
   colnames(Problem) <- c(names(LP$c), "", "")
   rownames(Problem)[1] <- ifelse(LP$maximize,"Maximize:","Minimize:")
   Problem <- as.matrix(format.data.frame(Problem, digits = 3))
   Problem[grepl("NA",Problem)] <- NA
   Solution <- cbind(Obs. = PMP$xstar, PMP = predict(PMP), LP = LP$solution)
   Solution <- rbind(round(Solution, 3), rep("", 3), Solution = c(round(PMP$xstar%*%PMP$c), round(predict(PMP)%*%PMP$c), round(LP$objective_value)))
   Solution <- cbind(Solution,
                     " " = rep("    ", PMP$nx+2) ,
                     "Similarities" = c(rep("", PMP$nx-1), paste("Cosine   = ", round(cosine(LP$solution, predict(PMP)),3)*100, "%", sep = ""), "", paste("Solution = ", round(100*predict(PMP)%*%PMP$c/LP$objective_value,1),"%",sep="")))
   make_line("=")
   cat("POSITIVE MATHEMATICAL PROGRAMMING PROBLEM:\n")
   print(Problem, digits = 3, na.print = "", quote = FALSE)
   make_line("-")
   cat("SOLUTION:\n")
   print(noquote(Solution))
   make_line("-")
   cat("Q-MATRIX\n")
   print(PMP$Q, digits = 4)
   if(length(PMP$impossible)>0){
      make_line("-")
      cat("Some constraints are violated by the PMP model:",paste(c(PMP$impossible), collapse = ", "),"\n")
   }
   make_line("=")
}
