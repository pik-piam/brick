% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createInputData.R
\name{createInputData}
\alias{createInputData}
\title{Create input data}
\usage{
createInputData(path, config, aggregateProblem = FALSE, overwrite = FALSE)
}
\arguments{
\item{path}{character vector with folders to write input data into}

\item{config}{named list with run configuration}

\item{aggregateProblem}{boolean, should the problem be agregated?}

\item{overwrite}{boolean, should existing input.gdx be overwritten?}
}
\description{
Create a complete set of input data for the gams optimisation.
}
\details{
This is still mostly test data created with ugly code that will be replaced
by proper input data preparation bit by bit.
}
\author{
Robin Hasse
}
