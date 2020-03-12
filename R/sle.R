#' Function to calculate the sea level rise equivalent of melting any given amount of ice
#'
#'


sle <- function(ice_vol, above){

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
