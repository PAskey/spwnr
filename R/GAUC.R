#' A function to estimate the GAUC from a time series of spawner counts
#'
#' @details This function is used to calculate the number of total fish days based on a series of spawner counts. Assumes the relative number of fish over time follows a Gaussian curve.
#' @title GAUC
#' @name GAUC
#' @param t  an ordered time series of count days
#' @param obs a series of observed counts of live fish corresponding to each t
#' @param day1 first day to sum in fish days, an optional parameter if the user wishes to restrict or extend the predictions or summed fish days to a specific period
#' @param lday last day to sum in fish days, an optional parameter if the user wishes to restrict or extend the predictions or summed fish days to a specific period
#' @param out  preferred type of output, default "AUC" = total fish day, "preds" returns the time specific predictions (e.g. for plotting)
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


GAUC = function(t,obs, day1 = NULL, lday = NULL, out = 'AUC'){
  if(is.null(day1)) day1 <- min(t)
  if(is.null(lday)) lday <- max(t)

  g=glm(obs~t+I(t^2),family=quasipoisson)
  x=coef(g)
  F=sqrt(-pi/x[3])*exp(x[1]-x[2]^2/(4*x[3]))

  preds = exp(predict(g, data.frame(t = seq(day1, lday))))

  if(out == 'AUC'){return(sum(preds))}
  if(out == 'Millar'){return(F)}
  if(out == 'preds'){return(preds)}
}
