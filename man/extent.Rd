% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extent.R
\name{extent}
\alias{extent}
\title{Describe rate of change in sea ice extent between two years}
\usage{
extent(year1, year2, ext, showplot = FALSE)
}
\arguments{
\item{year1}{the earlier year of interest, four digit format (i.e. 1985)}

\item{year2}{the later year of interest, four digit format (i.e. 1990)}

\item{ext}{data frame with at least two columns: year, extent (in millions of square kilometers)}

\item{showplot}{optional to display the sea ice extent for all the years in the ext data frame, default showplot=F}
}
\value{
a list with the following items
\describe{
\item{rate}{The rate of change in sea ice extent from year1 to year2}
\item{plt}{A plot of sea ice extent over all the years in the ext data frame, null if showplot=F}
}
}
\description{
Calculate rate of change in monthly sea ice extent in the Northern Hemisphere
}
\examples{
Generate some input data:
input <- data.frame(year = seq(2000,2019,1), extents = runif(20,3,7))
extent(year1 = 2005, year2 = 2010, ext = input, showplot=TRUE)

}
\references{
Example data downloaded from [ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/]
Metadata for package data: [https://nsidc.org/data/G02135/versions/3]
}
