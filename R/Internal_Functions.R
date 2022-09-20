get_constraint_names <- function(LP) {
   if(unique(rownames(LP$constraints)[1:LP$const.count])[1]==""){constraint_names <- LETTERS[1:LP$const.count]}
   else {constraint_names <- rownames(LP$constraints)[1:LP$const.count]}
   return(constraint_names)
}

get_direction_char <- function(x) sapply(x, function(i) c("<=", ">=", "==")[i])

rename <- function(x, letter){
   replacements <- paste(rep(letter, length(x)), 1:length(x), sep = "")
   if(is.null(names(x))) names(x) <- replacements
   for (i in 1:length(x)) if(names(x)[i]=="") names(x)[i] <- replacements[i]
   return(x)
}