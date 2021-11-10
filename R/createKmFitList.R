#' Create a list of dicekriging fits
#'
#' @param X matrix of inputs (design), variables in columns
#' @param Y matrix of outputs, variables in columns
#'
#' @return list of km (dicekriging) model fits, with each element of the list corresponding to a column of Y.
#' @export
#'
#' @examples
createKmFitList = function(X, Y, formula = ~., mc.cores = 2, ...){

  Y_list <- mat2list(Y)

  fit_list <- mclapply(Y_list, FUN = km, formula = formula, design = X, mc.cores = mc.cores, ...)

  fit_list
}
