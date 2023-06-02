#' A function to estimate the GAUC from a time series of spawner counts
#'
#' @details This function is used to calculate the number of total fish days based on a series of spawner counts. Assumes the relative number of fish over time follows a Gaussian curve.
#' @title GAUC
#' @name GAUC
#' @param t  an ordered time series of count days
#' @param obs a series of observed counts of live fish corresponding to each t
#' @param out  preferred type of output, default "AUC" = total fish days, "preds" returns the time specific predictions (e.g. for plotting)
#' @param preds.t a numeric vector to define predictions t for 'preds' output. Default are the t supplied the GAUC function call.
#' @keywords GAUC AUC
#' @export
#' @examples
#'  #Kokanee data from Coldstream Creek in 2021
#' t = c(270,273,276,282,287,290,295)
#' obs = c(603,1010,1380,4183,4158,3721,4423)
#'  #Add 0 observations at start and end, so that same information as used for TAUC example
#'  #Although 0 observations are not necessary for GAUC
#' t0 = c(263,303)
#' o0 = c(0,0)
#' t = c(t0,t)
#' obs = c(o0,obs)
#' GAUC(t,obs)


GAUC = function(t,obs, out = 'AUC', preds.t = NULL){
#Some risk of unrealistic result if times series is not bracketed by low observations.
lows = unique(sort(obs))[2]
first = obs[!is.na(obs)][1]
last = tail(obs[!is.na(obs)],1)
  if(!(first<=lows&last<=lows)) warning(
    "Time series does not start and end with lowest 2 observations.
     Consider adding 0s to define realistic spawning period.")

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
