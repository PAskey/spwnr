% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TAUC.R
\name{TAUC}
\alias{TAUC}
\title{TAUC}
\usage{
TAUC(t, obs, t_ends = NULL)
}
\arguments{
\item{t}{an ordered time series of count days (e.g days since Jan. 1)}

\item{obs}{a series of observed counts of live fish corresponding to each t}

\item{t_ends}{a vector of two values to define the first and last day of spawning season to be assigned 0 values.
These values are needed when the data series does not start and end with 0s.}
}
\description{
A function to estimate the TAUC from a time series of spawner counts. See Askey et al. 2023 CJFAS.
}
\details{
This function is used to calculate the number of total fish days based on a series of spawner counts. Uses the trapezoidal method.
     Requires the first and last observation be 0, and that the observations are sorted by time.
}
\examples{
#Kokanee data from Coldstream Creek in 2021
t = c(270,273,276,282,287,290,295)
obs = c(603,1010,1380,4183,4158,3721,4423)
TAUC(t,obs)
#Note warning produced.
#Must ensure that 0 observations occur at start and end, and days are in order
TAUC(t,obs, t_ends = c(263,303))
}
\keyword{AUC}
\keyword{TAUC}
