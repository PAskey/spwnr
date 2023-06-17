#' A function to estimate the GAUC from a time series of spawner counts
#'
#' @details This function is used to calculate the number of total fish days based on a series of spawner counts. Assumes the relative number of fish over time follows a Gaussian curve.
#' @title GAUC
#' @name GAUC
#' @param t  an ordered time series of count days
#' @param obs a series of observed counts of live fish corresponding to each t
#' #' @param t_ends a vector of two values to define the first and last day of spawning season to be assigned 0 values.
#' These values are not required for GAUC, but can lead to more realistic results when the count data series does not start and end with 0s.
#' @param out  preferred type of output, default "AUC" = total fish days, "preds" returns the time specific predictions (e.g. for plotting)
#' @param preds.t a numeric vector to define predictions t for 'preds' output. Default are the t supplied the GAUC function call.
#' @keywords GAUC AUC
#' @export
#' @examples
#' #Kokanee data from Coldstream Creek in 2021
#' t = c(270,273,276,282,287,290,295)
#' obs = c(603,1010,1380,4183,4158,3721,4423)
#' GAUC(t,obs)
#'
#'  #Although bracketing with 0 observations is not necessary for GAUC (as it is in TAUC),
#'  #and often makes no difference, adding a defined spawning period (t_ends) can constrain to more realistic results
#'  #if the observations are on the edge of a spawning period and start or end with a high value.
#'  #Add 0 observations at start and end, so that same information as used for TAUC example
#'
#' GAUC(t,obs, t_ends = c(263,303))
#' #In this Kokanee stream-year the constraints make no difference.
#' #However, if we take the Herring 1990 stream year from the Pink Salmon data set we see the effect.
#'
#'  t = All_cnts%>%dplyr::filter(STREAM =='HERRING', YEAR == 1990, !is.na(AIR_LIVE))%>%dplyr::pull(DOY)
#'  t
#'  obs = All_cnts%>%dplyr::filter(STREAM =='HERRING', YEAR == 1990, !is.na(AIR_LIVE))%>%dplyr::pull(AIR_LIVE)
#'  obs
#'  GAUC(t,obs)
#'  #Compare this value to an estimate of total fish days with the spawn period defined the known value
#'  GAUC(t,obs,t_ends = c(198,271))


GAUC = function(t,obs, t_ends = NULL, out = 'AUC', preds.t = NULL){
#Some risk of unrealistic result if times series is not bracketed by low observations.
lows = unique(sort(obs))[2]
first = obs[!is.na(obs)][1]
last = tail(obs[!is.na(obs)],1)
  if(!(first<=lows&last<=lows)) warning(
    "Time series does not start and end with lowest 2 observations.
     Consider adding 0s with 't_ends' to define realistic spawning period.")

  g=glm(obs~t+I(t^2),family=quasipoisson)
  x=coef(g)

  if(out == 'AUC'){
  F=sqrt(-pi/x[[3]])*exp(x[[1]]-x[[2]]^2/(4*x[[3]]))
  return(F)}

  if(out == 'preds'){
  if(is.null(preds.t)) preds.t <- t
  preds = exp(predict(g, data.frame(t = preds.t)))
  return(preds)}
}
