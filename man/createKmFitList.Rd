% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createKmFitList.R
\name{createKmFitList}
\alias{createKmFitList}
\title{Create a list of dicekriging fits}
\usage{
createKmFitList(X, Y, formula = ~., mc.cores = 2, ...)
}
\arguments{
\item{X}{matrix of inputs (design), variables in columns}

\item{Y}{matrix of outputs, variables in columns}
}
\value{
list of km (dicekriging) model fits, with each element of the list corresponding to a column of Y.
}
\description{
Create a list of dicekriging fits
}
