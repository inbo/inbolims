% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lims_plots.R
\name{lims_histogram}
\alias{lims_histogram}
\title{Histogram van het resultaat}
\usage{
lims_histogram(
  data,
  group = NULL,
  xlab,
  ylab,
  file = NA,
  show = TRUE,
  suffix = "",
  dpi = 300,
  width = 7,
  height = 5,
  ncol = 1,
  ...
)
}
\arguments{
\item{data}{lims dataset to plot}

\item{group}{charactervariabele die de groeperingskolom bepaalt}

\item{xlab}{naam x-as}

\item{ylab}{naam y-as}

\item{file}{bestandsnaam waar de figuur wordt bewaard (in de werkdirectory)}

\item{show}{toon de figuur op het scherm}

\item{suffix}{korte naam voor de figuur, deze zal scatter_date_suffix.png noemen}

\item{dpi}{resolutie}

\item{width}{breedte in inch}

\item{height}{hoogte in inch}

\item{ncol}{Als een groepvariabele gedefinieerd is, in hoeveel kolommen moet de figuren getoond worden}

\item{...}{andere variabelen zoals bins of breaks die gebruikt worden door geom_histogram}
}
\value{
gg histogram
}
\description{
Histogram van het resultaat
}
