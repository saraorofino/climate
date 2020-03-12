#' Describe sea ice extent extremes in the Northern Hemisphere
#'
#' Find sea ice extent maximum and minimum with corresponding year and month
#' @param si data frame of sea ice data containing at least three columns: year, mo, extent
#' @param showtable displays optional table of outputs, default is false
#' @return list with the following items
#' \describe{
#' \item{max}{Maximum sea ice extent in millions of squre kilometers}
#' \item{max_yr}{Year corresponding to the maximum sea ice extent}
#' \item{max_mo}{Month corresponding to the maximum sea ice extent}
#' \item{min}{Minimum sea ice extent in millions of square kilometers}
#' \item{min_yr}{Year corresponding to the minimum sea ice extent}
#' \item{min_mo}{Month corresponding to the minimum sea ice extent}
#' \item{si_table}{Table displaying the above output values, null if showtable=F}
#' }
#'
#' @examples
#' Generate some input data:
#' input <- data.frame(year = rep(1990, 12), mo = seq(1,12,1), extent = runif(12,2,15))
#' si_extremes(si = input, showtable = T)
#'
#'
#' @references
#' Example data downloaded from [ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/]
#' Metadata for package data: [https://nsidc.org/data/G02135/versions/3]



si_extremes <- function(si, showtable = FALSE){

  #The extent shouldn't be below zero
  #Some data may have no data values below zero - provide warning if any values are below 0
  if(any(si$extent < 0)){
      warning("Warning: extent should be positive, only positive values used")
    }

  #use only positive extent values
  si_pos <- si %>%
    filter(extent > 0)

  #find the maximum sea ice extent wth corresponding year and month
  max_si <- max(si_pos$extent)
  max_si_yr <- si_pos$year[which.max(si_pos$extent)]
  max_si_m <- si_pos$mo[which.max(si_pos$extent)]

  #find the minimum sea ice extent with corresponding year and month
  min_si <- min(si_pos$extent)
  min_si_yr <- si_pos$year[which.min(si_pos$extent)]
  min_si_m <- si_pos$mo[which.min(si_pos$extent)]


  #Optional table to display results:
  if(showtable == TRUE){

    #libraries needed:
    library(kableExtra)
    library(knitr)

    #create a df for the table:
    tbl_df <- data.frame(extreme = c("Max", "Min"), ext = c(max_si, min_si),
                         year = c(max_si_yr, min_si_yr), month = c(max_si_m, min_si_m))

    #create the table:
    t <- kable(tbl_df, col.names = c("Extreme", "Extent", "Year", "Month"),
          caption = "Sea Ice Extent Extremes") %>%
      kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                    full_width = F, position = "left") %>%
      row_spec(1:2, color = "grey")
  }

  if(showtable == FALSE){
    t=NULL #no table
  }

  #return values as a list
  return(list(max = max_si, max_yr = max_si_yr, max_mo = max_si_m,
              min = min_si, min_yr = min_si_yr, min_mo = min_si_m,
              si_table = t))

}





