% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lims_plots.R
\name{lims_boxplot}
\alias{lims_boxplot}
\title{Boxplot van het resultaat}
\usage{
lims_boxplot(
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

\item{...}{parameters die aan opgeroepen subfuncties kunnen meegegeven worden}
}
\value{
gg boxplot
}
\description{
Boxplot van het resultaat
}
