% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_indices.R
\name{order_rows}
\alias{order_rows}
\title{Order row indices}
\usage{
order_rows(ds, order_option, M = is.na(ds))
}
\arguments{
\item{ds}{A data frame}

\item{order_option}{This option defines the ordering of the indices. Possible
choices are "lowest_md_first", "highest_md_first", "increasing_index",
"decreasing_index".}

\item{M}{Missing data indicator matrix}
}
\value{
The ordered row indices of \code{ds} as a vector.
}
\description{
Order the indices of the rows of \code{ds} for imputation.
}
\examples{
ds <- data.frame(X = c(NA, NA, 3, 4), Y = c(1, NA, NA, 4))
order_rows(ds, "lowest_md_first")
}
