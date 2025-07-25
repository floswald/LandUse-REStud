% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/petite-region-agricole.R
\name{pra_2015}
\alias{pra_2015}
\title{Add Prices to PRA+Results 1999+}
\usage{
pra_2015()
}
\description{
We use this function to assign 2015 PRA prices to cities.
}
\details{
This uses data from interactive tableau
hence, it's an AVERAGE PRICE over all land types. Regarding the mapping,
this could be improved by relying on \code{\link[=PRA_xw]{PRA_xw()}} instead of \code{\link[=get_PRA_result_name_1999]{get_PRA_result_name_1999()}}.
}
