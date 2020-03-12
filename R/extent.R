#' Describe rate of change in sea ice extent between two years
#'
#' Calculate rate of change in monthly sea ice extent in the Northern Hemisphere
#' @param year1 the earlier year of interest, four digit format (i.e. 1985)
#' @param year2 the later year of interest, four digit format (i.e. 1990)
#' @param ext data frame with two columns: year, extent (in millions of square kilometers)
#' @param showplot optional to display the sea ice extent for all the years in the ext data frame, default showplot=F
#' @return a list with the following items
#' \describe{
#' \item{rate}{The rate of change in sea ice extent from year1 to year2}
#' \item{plt}{A plot of sea ice extent over all the years in the ext data frame, null if showplot=F}
#' }
#'
#' @examples
#' Generate some input data:
#' input <- data.frame(year = seq(2000,2019,1), extents = runif(20,3,7))
#' extent(year1 = 2005, year2 = 2010, ext = input, showplot=TRUE)
#'
#' @references
#' Example data downloaded from [ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/]
#' Metadata for package data: [https://nsidc.org/data/G02135/versions/3]


extent <- function(year1, year2, ext, showplot = FALSE){

  #want to be sure the ext input dataframe uses the same column names as the function requires
  col_names <- c("years", "extent")
  colnames(ext) <- col_names

  #Get the value of sea ice extent for the first input year
  ext1 <- subset(ext$extent, years==year1)

  #Get the value of sea ice extent for the second input year
  ext2 <- subset(ext$extent, years==year2)

  #calculate the rate of change (change in sea ice extent / change in time)
  rate = (ext2 - ext1) / (year2 - year1)

  #Optional plot of sea ice extent over the years:
  if(showplot == TRUE){

    #packages needed for graph
    library(tidyverse)
    library(RColorBrewer)

    p = ggplot(ext, aes(x = years, y = extent)) +
      geom_point(color = "dodgerblue4") +
      geom_line(color = "dodgerblue4") +
      scale_x_continuous(expand = c(0.005,0.005)) +
      scale_y_continuous(expand = c(0.05,0.05)) +
      labs(x = "Year",
           y = expression(Sea~Ice~Extent~(km^2))) +
      theme_light()
  }

  if(showplot == FALSE){
    p=NULL #no plot if showplot = false
  }

  return(list(rate = rate, plt = p))
}





