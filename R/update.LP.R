#' Re-calculate an "LP" object with changed internal states
#' 
#' @export
update.LP <- function(LP,
                      c = LP$c,
                      A = LP$A,
                      b = LP$b,
                      maximize = LP$maximize,
                      directions = LP$directions,
                      integer = LP$integer,
                      binary = LP$binary) {
   LP(c = c, A = A, b = b, maximize = maximize, directions = directions, integer = integer, binary = binary)
}