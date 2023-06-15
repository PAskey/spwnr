#' A function to estimate the TAUC from a time series of spawner counts
#'
#' @details This function is used to calculate the number of total fish days based on a series of spawner counts. Uses the trapezoidal method.
#'      Requires the first and last observation be 0, and that the observations are sorted by time.
#' @title TAUC
#' @name TAUC
#' @param t  an ordered time series of count days (e.g days since Jan. 1)
#' @param obs a series of observed counts of live fish corresponding to each t
#' @param t_ends a vector of two values to define the first and last day of spawning season to be assigned 0 values.
#' These values are needed when the data series does not start and end with 0s.
#' @keywords TAUC AUC
#' @export
#' @importFrom zoo rollapply
#' @examples
#' #Kokanee data from Coldstream Creek in 2021
#' t = c(270,273,276,282,287,290,295)
#' obs = c(603,1010,1380,4183,4158,3721,4423)
#' TAUC(t,obs)
#' #Note warning produced.
#' #Must ensure that 0 observations occur at start and end, and days are in order
#' TAUC(t,obs, t_ends = c(263,303))


TAUC = function(t, obs, t_ends = NULL){

  if(!is.null(t_ends)){
  t = c(t,t_ends)
  obs = c(obs,0,0)
  }

  #Remove NA values in observations and sort
  sorted = order(t[!is.na(obs)])
  t = t[sorted]
  obs = obs[sorted]

  if (obs[which.min(t)]>0|obs[which.max(t)]>0) {
    warning("Time series not bracketed with 0 observations. Use t_ends to define spawn period outside range of current observations.")
  }


  0.5*sum((diff(t))*((zoo::rollapply(obs,2,sum, na.rm = T))))
}
