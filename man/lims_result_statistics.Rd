% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lims_result_statistics.R
\name{lims_result_statistics}
\alias{lims_result_statistics}
\title{Title}
\usage{
lims_result_statistics(data, plot = "boxplot", log = TRUE)
}
\arguments{
\item{data}{lims report data (from lims_report_data)}

\item{plot}{if NULL no plot is created, if "boxplot" a boxplot is shown,
if "histogram" a histogram}

\item{log}{when plot, use the log-scale?}
}
\value{
list with measured parameters and some base statistics
}
\description{
Title
}
\examples{
\dontrun{
view(lims_result_statistics(reportdata))
}
}
