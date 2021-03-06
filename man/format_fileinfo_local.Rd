% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/azure-datalake.R, R/local.R
\name{format_fileinfo_adls}
\alias{format_fileinfo_adls}
\alias{format_fileinfo_local}
\title{Format a file-info data frame}
\usage{
format_fileinfo_adls(.data, tz = "UTC", ...)

format_fileinfo_local(.data, tz = "UTC", ...)
}
\arguments{
\item{.data}{object containing file information, usually a `data.frame`}

\item{tz}{`character`, name of timezone to use}

\item{...}{additional arguments passed to [dplyr::transmute()] to add
variables to the data frame.}
}
\value{
[tibble::tibble()] with varaibles:
\describe{
  \item{basename}{`character`, basename of the file path}
  \item{type}{`character`, can be `"file"` or `"directory"`}
  \item{length}{`double`, file-length (bytes)}
  \item{modified}{`POSIXct`, modification time}
}
}
\description{
Format a file-info data frame
}
