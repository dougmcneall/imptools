#' Calculate Implausibility
#'
#' @param em Emulator mean
#' @param em.sd Emulator uncertainty (standard deviation)
#' @param disc Model discrepancy mean
#' @param disc.sd Model discrepancy uncertainty (standard deviation)
#' @param obs Observation mean
#' @param obs.sd Observation uncertainty (standard deviation)
#'
#' @return Implausibility score
#' @export
#'
#' @examples
impl <- function(em, em.sd, disc, disc.sd, obs, obs.sd){
  # implausibility function
  # All uncertainties should be expressed as a single standard deviation.

  impl.squared <-  (em - disc - obs)^2 / (em.sd^2 + disc.sd^2 + obs.sd^2)

  impl <- sqrt(impl.squared)

  impl
}
