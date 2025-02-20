% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_thermo2.R
\name{soln_saha}
\alias{soln_saha}
\title{Solution for Ionization Fraction \(X_e = 0.5\)}
\usage{
soln_saha(z)
}
\arguments{
\item{z}{Numeric. The redshift value.}
}
\value{
Numeric. The result of the equation \(X_e - 0.5\).
}
\description{
This function solves the equation \(X_e - 0.5 = 0\) for the redshift \(z\).
}
\examples{
soln_saha(1350)
}
