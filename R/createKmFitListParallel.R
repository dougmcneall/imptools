#' Create a list of dicekriging fits using parallel processing
#'
#' @param X X matrix of inputs (design), variables in columns
#' @param Y Y matrix of outputs, variables in columns
#' @param formula formula for the prior km fit, with a default as a linear model '~.'
#' @param multistart number of starts for the hyperparameter optimisation
#' @param ... 
#'
#' @return list of km (dicekriging) model fits, with each element of the list corresponding to a column of Y.
#' @export
#'
#' @examples
createKmFitListParallel <- function (X, Y, formula = ~., multistart = 4,  ...){
  # Wrapper function to parallel compute a list of emulator fits, one for each column of Y
  d <- ncol(Y)
  Y_list <- as.list(as.data.frame(Y))
  
  foreach(i = 1:d, .combine = 'c', .packages = 'DiceKriging') %dopar% {
    
    fit <- km(formula = formula, design = X, response = Y_list[[i]], multistart = multistart, ...)
    list(fit)
    
  }
}