% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{transform_latlon_to_different_CRS}
\alias{transform_latlon_to_different_CRS}
\title{Convert coordinate reference system}
\usage{
transform_latlon_to_different_CRS(xy, epsg_code_from, epsg_code_to)
}
\arguments{
\item{xy}{lat lon coordinates}

\item{epsg_code_from}{code for coordinates given (make optional if already spatial object)}

\item{epsg_code_to}{the epsg_code that should be transformed into}
}
\description{
Convert coordinate reference system
}
