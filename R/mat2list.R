#' Create a list of each column of a matrix
#'
#' @param X 
#'
#' @return list of each column of X
#' @export
#'
#' @examples
mat2list <- function(X){
  
  # Turns the p columns of a matrix into a p length list,
  # with each column becoming an element of the list
  
  out <- vector(mode = 'list', length = ncol(X))
  for(i in 1:ncol(X)){
    out[[i]] <- X[ , i]
  }
  out
  
}