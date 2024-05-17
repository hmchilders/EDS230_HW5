#'  Forest growth Model
#' @param T  period of growth (years)
#' @param C initial Carbon (kgC)
#' @param r exponential growth rate
#' @param thresh canopy threshold
#' @param g linear growth rate
#' @param K maximum carbon threshold (carrying capacity)
#' @return derivative of Carbon at time T
#'
canopy_growth = function(T, C, parameters) {
  # Extract parameters
  r <- as.numeric(parameters["r"])
  thresh <- as.numeric(parameters["thresh"])
  g <- as.numeric(parameters["g"])
  K <- as.numeric(parameters["K"])
  # exponential growth for a forest below the carbon threshold
  dCdt = r*C
  # check to see if greater than threshold
  if (C > thresh) {
    dCdt= g*(1- (C/K))
  }
  #check to see if greater than carrying capacity
  if(C >= K){
    dCdt = K
  }
  return(list(dCdt))
}