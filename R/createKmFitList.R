#' Create a list of dicekriging fits
#'
#' @param X matrix of inputs (design), variables in columns
#' @param Y matrix of outputs, variables in columns
#'
#' @return list of km (dicekriging) model fits, with each element of the list corresponding to a column of Y.
#' @export
#'
#' @examples
createKmFitList = function(X, Y){
  # create a list of km fits for multivariate output
  Y_list = as.list(as.data.frame(Y))
  
  fit_list = NULL
  for(i in 1:length(Y_list)){
    
    fit = km(formula = ~.,  design = X, response = Y_list[[i]])
    fit_list = c(fit_list, fit)
  }
  fit_list
}
