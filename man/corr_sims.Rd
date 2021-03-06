% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_sims.R
\name{corr_sims}
\alias{corr_sims}
\title{Compute correlations between two matrices of similarity vectors}
\usage{
corr_sims(sims1, sims2, type = "pearson")
}
\arguments{
\item{sims1}{matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)}

\item{sims2}{matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)}

\item{type}{\code{pearson} or \code{rank} correlation}
}
\value{
a tibble with mean and standard error of correlation overt set of cues
}
\description{
Compute correlations between two matrices of similarity vectors
}
\note{
embedding models will be subsetted to common vocabulary
}
