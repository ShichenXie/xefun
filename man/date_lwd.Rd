% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_funcs.R
\name{date_lwd}
\alias{date_lwd}
\title{latest workday}
\usage{
date_lwd(n, to = Sys.Date())
}
\arguments{
\item{n}{number of days}

\item{to}{a date, default is current system date.}
}
\value{
It returns the latest workday date that is n days before a specified date.
}
\description{
The latest workday date of n days before a specified date.
}
\examples{
date_lwd(5)
date_lwd(3, "2016-01-01")
date_lwd(3, "20160101")

}
