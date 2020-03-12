#' Function to find the rate of change in the monthly extent of Arctic sea ice between any two years
#'
#'  @data ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/
#'  https://nsidc.org/data/G02135/versions/3
#'

extent <- function(year1, year2, ext){

  #Get the value of sea ice extent for the first input year
  ext1 <- subset(ext$extents, years == year1)

  #Get the value of sea ice extent for the second input year
  ext2 <- subset(ext$extents, years == year2)

  #calculate the rate of change (change in sea ice extent / change in time)
  rate = (ext2 - ext1) / (year2 - year1)

  return(rate)
}
