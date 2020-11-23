#' Elements of a vector that fall between thresholds                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
#'
#' @param x vector
#' @param maxes maxima
#' @param mins minima
#'
#' @return logical vector of length one.
#' @export
#'
#' @examples
withinRange = function(x, maxes, mins){
  # which elements of a vector are between
  # elements of the min and max vectors?
  
  all(x < maxes && x > mins)
}                                  

  





  







    