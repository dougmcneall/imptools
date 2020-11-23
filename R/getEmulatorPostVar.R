#' Returns the min, mean and max emulator posterior variance for each output in a wave object
#'
#' @param waveobj 
#'
#' @return
#' @export
#'
#' @examples
getEmulatorPostVar = function(waveobj){
  # Returns the min, mean and max emulator posterior variance for a wave,
  # for each output
  # 
  n = nrow(waveobj$X_aug)
  p = length(waveobj$pred_list)
  
  pv_mat = matrix(ncol = p , nrow = n)
  
  for(i in 1:p){
    pv_mat[, i]  =   (waveobj$pred_list[[i]]$sd)^2
    
  }
  pv_mean = apply(pv_mat,2, mean)
  pv_max = apply(pv_mat, 2, max)
  pv_min = apply(pv_mat,2, min)
  
  
  return(list(pv_mat = pv_mat, pv_mean = pv_mean, pv_min = pv_min, pv_max = pv_max) )           
}