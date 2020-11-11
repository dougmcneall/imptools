#' Calculate Implausibility
#'
#' @param em Emulator mean
#' @param em_sd Emulator uncertainty (standard deviation)
#' @param disc Model discrepancy mean
#' @param disc_sd Model discrepancy uncertainty (standard deviation)
#' @param obs Observation mean
#' @param obs_sd Observation uncertainty (standard deviation)
#'
#' @return Implausibility score
#' @export
#'
#' @examples
impl <- function(em, em_sd, disc, disc_sd, obs, obs_sd){
  # implausibility function
  # All uncertainties should be expressed as a single standard deviation.

  impl_squared <-  (em - disc - obs)^2 / (em_sd^2 + disc_sd^2 + obs_sd^2)

  impl <- sqrt(impl_squared)

  impl
}
