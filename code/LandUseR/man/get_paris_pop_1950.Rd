% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cities-defs.R
\name{get_paris_pop_1950}
\alias{get_paris_pop_1950}
\title{Get Paris Population in 1950}
\usage{
get_paris_pop_1950(p)
}
\description{
Our definition of Paris is much greater than the administrative boundary of the City of Paris in 1950.
}
\details{
We manually check which surrounding communities fall within our classification of the Paris area in 1950 and
add the name of those communities to our list of constituting jurisdictions making up the greater Paris population.
Paris region candidate communes from https://fr.wikipedia.org/wiki/Réorganisation_de_la_région_parisienne_en_1964.
These are \emph{all} the communes. Some will not be part of our delineation of Paris.
need to check manually one by one against our screen shot of the Paris area in 1950.
}
