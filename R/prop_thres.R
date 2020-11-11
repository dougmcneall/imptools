#' Proportion of elements of a vector below a threshold
#'
#' @param x vector
#' @param thres threshold
#' @param above logical: should the function calculate the proportion above the threshold?
#'
#' @return
#' @export
#'
#' @examples prop_thres(x = 1:10, thres = 3, above = FALSE)
prop_thres <- function(x, thres, above = FALSE){
  # propotion of vector x below a threshold thres
  n <- length(x)
  
  if(above) bt <- length(x[x > thres])
  
  else bt <- length(x[x < thres])
  
  prop <- bt/n
  
  prop
}