#' Visual counts of Kokanee spawners in Okanagan, BC streams from ground based surveys
#'
#'
#' @format A data frame with 145 rows and 9 variables:
#' \describe{
#'   \item{STREAM}{Character string describing stream name}
#'   \item{YEAR}{Year of survey}
#'   \item{MONTH}{Month of survey}
#'   \item{DAY}{Day within month of survey}
#'   \item{DOY}{Day within year (from Jan 1) of survey}
#'   \item{Date}{Character string of date}
#'   \item{NUM_DATE}{Numeric of date}
#'   \item{LIVE_COUNT}{Number of live Kokanee counted in stream}
#'   \item{SOURCE}{Character string describing if 'OBSERVED' data or an "ADDED_0" to define endpoints of run for AUC calculations}
#' }
#' @source PAskey personal copy with .csv stored in data-raw. Obtained from BC provincial government fisheries staff in Penticton.
"KO_cnts"

#' Fence counts of all Kokanee entering stream over entire run for several BC interior streams
#'
#'
#' @format A data frame with 14 rows and 3 variables:
#' \describe{
#'   \item{STREAM}{Character string describing stream name}
#'   \item{YEAR}{Year of survey}
#'   \item{FENCE_COUNT}{Total number of fish passing fence in the survey year}
#' }
#' @source PAskey personal copy with .csv stored in data-raw
"KO_fence"

#' Daily visual counts of Kokanee spawners in Mission Creek spawning channel, BC from ground based surveys
#'
#'
#' @format A data frame with 616 observations and 10 variables
#' \describe{
#'   \item{STREAM}{Character string describing stream name, only MISSION}
#'   \item{REACH}{Character string describing stream reach. Reach E selected as this is the spawning channel}
#'   \item{YEAR}{Year of survey}
#'   \item{MONTH}{Month of survey}
#'   \item{DAY}{Day within month of survey}
#'   \item{Date}{Character string of date}
#'   \item{NO_LIVE}{Number of live Kokanee counted in stream}
#'    \item{NO_DEAD}{Number of dead Kokanee counted in stream}
#'   \item{TEMP_C}{Numeric of stream temperature in celsius}
#'   \item{COMMENTS}{Character string of additional information collected by counting contractor}
#' }
#' @source PAskey personal copy. Raw csv file Mission_channel in data-raw folder. Obtained from BC fisheries staff in Penticton.
"Mission_ch"

#' A summary of the total number of spawners passing through a counting fence as compared to visual indices of fish days (GAUC and TAUC) and Peak Counts.
#'
#' @name spwnr_ests
#' @format A data frame with 25 rows and 8 variables:
#' \describe{
#'   \item{SPECIES}{Character string describing species of fish being counted, Kokanee or Pink Salmon}
#'   \item{STREAM}{Character string describing stream name}
#'   \item{YEAR}{Year of survey}
#'   \item{FENCE_COUNT}{Total number of fish passing fence in the survey year}
#'   \item{PEAK_COUNT}{Maximum number of fish observed during a single survey on a single day within a year}
#'   \item{TAUC}{Total fish days from Trapezoidal area under the curve}
#'   \item{GAUC}{Total fish days from Gaussian area under the curve}
#'   \item{fit_group}{Groups of streams for cross-validation analyses}
#'
#' }
#' @source PAskey personal copy compiled from .csv files KO_cnts, KO_fence, Pink_data in data-raw. See data_prep.R in data-raw. Pink data from Millar et la 2012 publication.
#' @references \url{https://www.researchgate.net/profile/Chris-Jordan-7/publication/237176128_Simple_estimators_of_salmonid_escapement_and_its_variance_using_a_new_area-under-the-curve_method/links/58d02719a6fdcc344b0c08b6/Simple-estimators-of-salmonid-escapement-and-its-variance-using-a-new-area-under-the-curve-method.pdf}
"spwnr_ests"

