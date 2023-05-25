#' A function to estimate the TAUC from a time series of spawner counts
#'
#' @details This function is used to calculate the number of total fish days based on a series of spawner counts. Uses the trapezoidal method.
#'      Requires the first and last observation be 0, and that the observations are sorted by time.
#' @title TAUC
#' @name TAUC
#' @param t  an ordered time series of count days
#' @param obs a series of observed counts of live fish corresponding to each t
#' @keywords TAUC AUC
#' @export
#' @importFrom zoo rollapply
#' @examples
#'  #Kokanee data from Coldstream Creek in 2021
#' t = c(270,273,276,282,287,290,295)
#' obs = c(603,1010,1380,4183,4158,3721,4423)
#'  #Must ensure that 0 observations occur at start and end, and days are in order
#' t0 = c(263,303)
#' o0 = c(0,0)
#' t = c(t0,t)
#' obs = c(o0,obs)
#' TAUC(t,obs)


TAUC = function(t, obs){
  0.5*sum((diff(t))*((zoo::rollapply(obs,2,sum))))
}