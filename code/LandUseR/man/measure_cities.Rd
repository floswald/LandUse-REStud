% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GHS.R
\name{measure_cities}
\alias{measure_cities}
\title{Measure City Extents}
\usage{
measure_cities(
  topn = 100,
  overwrite = FALSE,
  cutoff = 30,
  w = 9,
  h = 6,
  checkplots = FALSE
)
}
\arguments{
\item{topn}{integer how many cities to consider. default: 100.}

\item{overwrite}{TRUE/FALSE}

\item{cutoff}{numeric indicating the cutoff value (percent) of built-up area above which we classify as "urban"

We read the list of cropped city bounding boxes from \code{\link{cropboxes}} in order to classify them as either urban area or not.
The classification of urban or not is governed by two constraints:
\enumerate{
\item the \code{cutoff} parameter. If a grid cell is measured with built-up surface of more than \code{cutoff}, the cell is retained as \code{urban}.
\item Contiguity of grid cells. Given that classified grid cells could be separated in space, we check for the largest connect set. The check uses the "rook move" as criterion.
}
As a second}
}
\description{
Takes cropped cities and counts the number of grid cells which are classified as "urban area".
}
