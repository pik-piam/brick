% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runCalibration.R
\name{.getDims}
\alias{.getDims}
\title{Get dimension names from calibration targets}
\usage{
.getDims(calibTarget)
}
\arguments{
\item{calibTarget}{list, calibration targets}
}
\description{
Gives the column names of a list of data frames except the first and the last
one, i.e. \code{qty} and \code{value}.
}
