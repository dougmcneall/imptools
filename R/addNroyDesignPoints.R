#' Augment a design with NROY design points
#'
#' @param X original design matrix
#' @param Y model output matrix
#' @param Y_target target output or observation
#' @param n_aug number of candidate points with which to augment the design
#' @param mins_aug minima of n_aug
#' @param maxes_aug maxima of n_aug
#' @param thres implausibility threshold
#' @param disc_list model discrepancy (bias)
#' @param disc_sd_list model discrepancy uncertainty
#' @param obs_sd_list observation (Y_target) uncertainty
#' @param n_app final number of points with which to augment the design
#' @param nreps number of iterations in the maximin search
#' @param shrink keep only members of the original design hat fall within marginal limits of the NROY
#'
#' @return a list
#' @export
#'
#' @examples
addNroyDesignPoints = function(X, Y, Y_target, n_aug,
                               mins_aug, maxes_aug,
                               thres = 3, disc_list,
                               disc_sd_list, obs_sd_list,
                               n_app,
                               nreps,
                               shrink = TRUE){

  # Add NROY design points to a design, using uniform sampling from the
  # entire input space.
  # Inputs
  # X            ...       design matrix (output from maximinLHS)
  # Y            ...       model output matrix
  # Y_target     ...       Target output, or "observation"
  # n_aug        ...       number of candidate points to augment the lhs
  # mins_aug
  # maxes_aug    ...       Limits on the candidate points

  # At the moment, the function keeps design points outside the margins of the
  # NROY space.

  # list of fitted km objects, one list element for each output
  fit_list = createKmFitList(X=X, Y=Y)

  # How good is the fit?
  loo_list = lapply(fit_list, FUN = leaveOneOut.km, type = 'UK', trend.reestim = TRUE)

  loo_mse_vec = rep(NA, length(loo_list))
  for(i in 1:length(loo_list)){

    loo_mse = mean((loo_list[[i]]$mean - Y_target[,i])^2)
    loo_mse_vec[i] = loo_mse
  }

  # create a new set of candidate points
  #X_aug = augmentLHS(X, n_aug) # expensive
  X_aug = samp_unif(n_aug, mins = mins_aug, maxes = maxes_aug)
  colnames(X_aug) = colnames(X)

  # predicting the output at each design point
  pred_list = lapply(fit_list, FUN = 'predict', newdata = X_aug, type = 'UK')
  # calculate the implausibility at each design point

  impl_mat = NULL
  for(i in 1:length(pred_list)){

    pred_impl = impl(em = pred_list[[i]]$mean,
                     em_sd = pred_list[[i]]$sd,
                     disc = disc_list[[i]],
                     disc_sd = disc_sd_list[[i]],
                     obs = Y_target[[i]],
                     obs_sd = obs_sd_list[[i]])

    impl_mat = cbind(impl_mat, pred_impl)
  }

  # Which of the candidte design points are NROY?

  # create a matrix of the implausibility measures
  # find the indices of the matrix where all are below the threshold.
  nroy_tf = apply(impl_mat, 1, FUN = all_bt, thres = thres)
  nroy_ix = which(nroy_tf==TRUE)
  X_nroy = X_aug[nroy_ix, ]

  # I think this code was wrong
  X_nroy_max = apply(X_nroy, 2, max)
  X_nroy_min = apply(X_nroy, 2, min)

  # find a subset of the NROY points which are far apart, according to a
  # maximin algorithm
  ix_list = vector(mode='list', length = nreps)
  mindist_vec = rep(NA, nreps)

  for(i in 1:nreps){
    ix = sample(1:nrow(X_nroy), n_app)
    X_cand = X_nroy[ix, ]
    ix_list[[i]] = ix
    mindist = min(dist( X_cand))
    mindist_vec[i] = mindist
  }
  ix_chosen = ix_list[[which.max(mindist_vec)]]

  # X_mm is the new maximin sample appending the design
  X_mm = X_nroy[ix_chosen, ]

  # Add the NROY sample to the design
  if(shrink){

    # Keep only members of the original design
    # that fall within marginal limits of the NROY

    keep_ix = which(apply(X, FUN = withinRange,1,
                          maxes = X_nroy_max,
                          mins = X_nroy_min))
    X_kept = X[keep_ix, ]
    X_out = rbind(X_kept, X_mm)
  }

  else{
    X_kept = X
    X_out = rbind(X, X_mm)
  }

  return(list(X_nroy = X_nroy,
              X_aug = X_aug,
              X_mm = X_mm,
              X_out = X_out,
              impl_mat = impl_mat,
              loo_mse_vec = loo_mse_vec,
              fit_list = fit_list,
              pred_list = pred_list,
              nroy_ix = nroy_ix,
              ix_chosen = ix_chosen,
              X_nroy_max = X_nroy_max,
              X_nroy_min = X_nroy_min,
              X = X,
              Y = Y)
  )
}
