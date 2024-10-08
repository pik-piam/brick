% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.explicitZero}
\alias{.explicitZero}
\title{Make zeros explicit}
\usage{
.explicitZero(x, value = "value")
}
\arguments{
\item{x}{data.frame}

\item{value}{character, name of value column}
}
\value{
data.frame with explicit zeros in value column
}
\description{
Replace zeros in value column with the gams special value EPS. This way, the
value is saved in gdx files by gams.
}
\author{
Robin Hasse
}
