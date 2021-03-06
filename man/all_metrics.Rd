% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_metrics.R
\name{all_metrics}
\alias{all_metrics}
\title{Calculate all metrics in iglu}
\usage{
all_metrics(data)
}
\arguments{
\item{data}{DataFrame object with column names "id", "time", and "gl".}
}
\value{
A tibble object with 1 row per subject and one column per metric is returned.
}
\description{
The function all_metrics runs all of the iglu metrics, and returns the results with
one column per metric.
}
\details{
All iglu functions are calculated within the all_metrics function, and the resulting tibble
is returned with one row per subject and a column for each metric. Time dependent functions are
calculated together using the function optimized_iglu_functions. For metric specific
information, please see the corresponding function documentation.
}
\examples{
data(example_data_1_subject)
all_metrics(example_data_1_subject)

}
