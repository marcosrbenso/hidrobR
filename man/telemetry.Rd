% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/telemetry_hidroweb.R
\name{telemetry}
\alias{telemetry}
\title{Donwload data from telemetry}
\usage{
telemetry(CodEstacao, DataInicio, DataFim)
}
\arguments{
\item{CodEstacao}{integer. Station code retrieved from HidroWeb}

\item{DataInicio}{character. Date of the beginning of the observation in the format 'day/month/Year'}

\item{DataFim}{character. Date of the ending of the observation in the format 'day/month/Year'}
}
\description{
Donwload data from telemetry
}
\examples{
data <- telemetry(65945400,DataInicio="01/01/2020",DataFim = "01/01/2021")
head(data)

}
