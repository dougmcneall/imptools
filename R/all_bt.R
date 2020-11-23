#' Are all elements of a vector below a threshold?
#'
#' @param x vector
#' @param thres threshold
#'
#' @return logical
#' @export
#'
#' @examples all_bt(c(1,2,3), 3)
all_bt = function(x, thres){
  
  all(x < thres)
}