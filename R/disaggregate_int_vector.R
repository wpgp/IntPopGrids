#' disaggregate_int_vector
#'
#' Disaggregate an integer vector into a integer matrix. 
#' Row sums equals to the input integer vector.
#' Col sums equals to the input proportions multiplying the sum of the input vector.
#' 
#' @param x An integer vector.
#' @param r A vector of proportions, sum to 1.
#' @param method The method used to distribute population. "random" (the default) put people randomly based on probabilities where higher decimals with higher probabilities. "rank" put people based on the decimal values from the highest to the lowest.
#' 
#' @return 
#' An integer matrix
#' 
#' @examples
#' x <- sample.int(100,5)
#' r <- c(0.2, 0.8)
#' X %o% r
#' disaggregate_int_vector(x,r,method="random")
#' 
#' @export
disaggregate_int_vector <- function(x,r,method) {
  y <- x %*% r
  y_int <- floor(y)
  x_num <- x - rowSums(y_int, na.rm = T)
  r <- int_vector(sum(x, na.rm = T) * r)
  r_num <- r - colSums(y_int, na.rm = T)
  y_dec <- y - y_int
  rm(x, y, r)
  gc()
  if (missing(method)) {
    method = "random"
  }
  for (i in sample(seq(length(r_num)))) {
    if (method=="random") {
      while(r_num[i]>0) {
        index <- which(x_num >= 1)
        add_location <- sample(1:length(index),
                               size = min(length(index), r_num[i]), 
                               replace = FALSE,
                               prob = y_dec[index, i]/sum(y_dec[index, i], na.rm = T))
        add_location <- index[add_location]
        x_num[add_location] <- x_num[add_location] - 1
        r_num[i] <- r_num[i] - min(length(index), r_num[i])
        y_int[add_location, i] <- y_int[add_location, i] + 1
      }
    }
    if (method=="rank") {
      while(r_num[i]>0) {
        index <- which(x_num >= 1)
        add_location <- order(y_dec[index, i], decreasing = T)
        add_location <- add_location[1:min(length(index), r_num[i])]
        x_num[add_location] <- x_num[add_location] - 1
        r_num[i] <- r_num[i] - min(length(index), r_num[i])
        y_int[add_location, i] <- y_int[add_location, i] + 1
      }
    }
  }
  return(y_int)
}
