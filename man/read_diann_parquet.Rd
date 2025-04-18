% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_diann_tsv.R
\name{read_diann_parquet}
\alias{read_diann_parquet}
\title{Reads DIA-NN parquet report files}
\usage{
read_diann_parquet(path, basename = FALSE, ...)
}
\arguments{
\item{path}{Filepath to DIA-NN parquet report file}

\item{basename}{`T` or `F` to apply `sample_basename()`}

\item{...}{
  Arguments passed on to \code{\link[=sample_basename]{sample_basename}}
  \describe{
    \item{\code{drop_ext}}{Options to drop file extensions: "no", "last", "all", see Details}
  }}
}
\value{
A data frame
}
\description{
Reads DIA-NN parquet report files
}
\details{
DIA-NN parquet report files contain full path names in the `Run` column.
The option `basename` applies the function `sample_basename` to the `Run` column.
}
\examples{
\dontrun{
x <- "path/to/your/diann_report.parquet"
read_diann_parquet(x, basename = TRUE, drop_ext = "all")
}
}
