% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readSymbol.R
\name{readSymbol}
\alias{readSymbol}
\title{Read symbol from gams container}
\usage{
readSymbol(x, symbol = NULL, selectArea = TRUE, stringAsFactor = TRUE)
}
\arguments{
\item{x}{gams Container, Parameter, Variable or Set}

\item{symbol}{character, name of gams object if x is a Container else NULL}

\item{selectArea}{logical, select area quantity and remove this dimension}

\item{stringAsFactor}{logical, keep default factors from gams}
}
\description{
Read symbol from gams container
}
\author{
Robin Hasse
}
