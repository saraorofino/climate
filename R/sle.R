#' Describes the equivalent change in sea level rise from ice volume
#'
#' Calculate the sea level rise equivalent (in mm) from melting any given volume of sea ice
#' @param ice_vol a single numeric value sea ice volume in kilometers cubed
#' @param above the percent of the sea ice that is above the surface of the water, in decimal format
#'
#' @return the sea level rise equivalent (in mm) caused by the melting of the input volume of sea ice (ice_vol)
#'
#' @examples
#' sle(ice_vol = 500, above = 0.6)


sle <- function(ice_vol, above){

  #Calculating based on one value for volume
  if(length(ice_vol) > 1){
    return("Provide a single numeric value for ice_vol, not a vector")
  }

  #Above input should be a decimal with a value between 0 and 1
  if(above < 0 | above > 1){
    return("Above parameter should be a percent in decimal format between 0 and 1")
  }

  #only want to include volume of ice above the water that will melt from climate change
  #volume in km^3
  calc_vol = ice_vol* above

  #calculate the mass for the above water ice
  #mass (GT) = volume of ice (km^3) * density of ice (GT/km^3)
  calc_mass = calc_vol * 0.9167

  #calculate sea level rise equivalent - given that 361.8 GT of ice will raise sea levels by 1 mm
  #sle in mm = mass of ice (GT) * (1 mm sea level rise / 361.8 GT of ice)
  sle = calc_mass * (1/361.8)

  return(sle)
}
