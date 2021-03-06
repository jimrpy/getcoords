% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode_amap.R
\name{geocode_amap_base}
\alias{geocode_amap_base}
\title{Get the GCJ02 Coordinates Parsed by Amap}
\usage{
geocode_amap_base(location, key = key)
}
\arguments{
\item{location}{Location name, vector, maximum length is 10.}

\item{key}{Application key of amap.com.}
}
\value{
A tibble with 4 variables, \code{location}, location parsed, longitude, latitude.
}
\description{
Get the GCJ02 Coordinates Parsed by Amap
}
