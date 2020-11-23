#' Choose a set of NROY points with the largest minimum distrance
#'
#' @param n_app desired size of new design
#' @param waveobj wave object
#' @param nreps repetitions
#'
#' @return matrix of design points
#' @export
#'
#' @examples
chooseMaximinNroy = function(n_app, waveobj, nreps){
  # Choose a set of NROY points with the largest minimum
  # distance
  ix_list = vector(mode='list', length = nreps)
  mindist_vec = rep(NA, nreps)
  
  for(i in 1:nreps){
    ix = sample(1:nrow(waveobj$X_nroy), n_app)
    X_cand = waveobj$X_nroy[ix, ]
    ix_list[[i]] = ix
    mindist = min(dist( X_cand))
    mindist_vec[i] = mindist
  }
  ix_chosen = ix_list[[which.max(mindist_vec)]]
  
  return(waveobj$X_nroy[ix_chosen, ])
}
