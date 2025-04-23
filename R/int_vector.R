#' int_vector
#'
#' Generate corresponding integer vector with smallest difference to the input vector 
#' 
#' @param x A numerical vector.  
#' 
#' @return 
#' An integer vector.
#' 
#' @examples
#' x <- runif(5) * 10
#' int_vector(x)
#' 
#' @export
int_vector <- function(x) {
  x_int <- floor(x)
  x_dec <- x - x_int
  x_num <- sum(x_dec, na.rm=T)
  x_num <- round(x_num)
  index <- order(x_dec, decreasing = T)[0:x_num]
  x_int[index] <- x_int[index] + 1
  return(x_int)
}
