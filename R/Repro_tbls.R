#' A function to reproduce all results in tables in Askey et al. paper
#'
#' @title Repro_tbls
#' @name Repro_tbls
#' @keywords spwnr, Kokanee, Pink, Askey
#' @export
#' @param Output a character vector to describe output?
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'

Repro_tbls = function(){

lmfits = vector(mode = "list")
L = 3#Number of visual index columns
J = max(spwnr$fit_group)
Coef = RSQ = MPB = MPE = MDPE = maxE = pRSQ = pMPE = pMPB = pMDPE = pmaxE = vector(length = L)

#log-log scale
spwnr = spwnr%>%
  dplyr::mutate(
    lTAUC = log(TAUC),
    lGAUC = log(GAUC),
    lPEAK = log(PEAK_COUNT),
    lFENCE = log(FENCE_COUNT)
)%>%
  dplyr::relocate(lTAUC, lGAUC, lPEAK, TAUC, GAUC, PEAK_COUNT, lFENCE, FENCE_COUNT)

results = all_results = list()

for (S in levels(spwnr$SPECIES)){
  for (j in 0:J){
    vis = spwnr%>%dplyr::filter(fit_group != j, SPECIES == S)
    for (i in 1:L)
    {
      lmfits[[i]] <- lm(vis$lFENCE~ 1 + offset(dplyr::pull(vis[,i])))
      Coef[i]<-lmfits[[i]]$coef

      #Within sample metrics
      sqerr = (vis$FENCE_COUNT - exp(predict(lmfits[[i]])))^2
      sqtot = (vis$FENCE_COUNT - mean(vis$FENCE_COUNT))^2
      RSQ[i] = 1 -  sum(sqerr)/sum(sqtot)
      MPE[i] = mean(abs(vis$FENCE_COUNT - exp(predict(lmfits[[i]])))/vis$FENCE_COUNT)#mean percent error
      MDPE[i] = median(abs(vis$FENCE_COUNT - exp(predict(lmfits[[i]])))/vis$FENCE_COUNT)#median percent error
      MPB[i] = mean((vis$FENCE_COUNT - exp(predict(lmfits[[i]])))/vis$FENCE_COUNT)
      maxE[i] = max(abs(vis$FENCE_COUNT - exp(predict(lmfits[[i]])))/vis$FENCE_COUNT)#max percent error

      #Out of sample metrics
      psqerr = (spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S] - (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))^2
      psqtot = (spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S] - mean(spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S]))^2
      pRSQ[i] = 1 -  sum(psqerr)/sum(psqtot)
      pMPB[i] = mean((spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pMPE[i] = mean(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pMDPE[i] = median(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pmaxE[i] = max(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
    }
    results[[j+1]] <- data.frame(Group_out = rep(j,L), Method =names(vis[1:L]),
                                 Coef, RSQ, MPB, MPE, MDPE, maxE, pRSQ, pMPE, pMPB, pMDPE, pmaxE)

  }

  all_results[[S]] = do.call("rbind",results)%>%dplyr::mutate(Species = S)
}

all_results = do.call("rbind",all_results)
rownames(all_results)<-NULL
#factorize to preserve specific order in tables
all_results = all_results%>%
  dplyr::mutate(Method = substring(Method, 2),
                Method = factor(Method, c('PEAK', "TAUC", "GAUC")))

#Summary of within sample fits to data
table2data <<- all_results%>%
  dplyr::filter(Group_out==0)%>%#, Species == "KOKANEE"
  dplyr::mutate(Beta = exp(Coef))%>%
  dplyr::mutate(round(dplyr::pick(where(is.numeric)),2))%>%
  dplyr::arrange(Species, Method)%>%
  dplyr::select(Species, Method, Beta, RSQ,MPB, MPE, MDPE, maxE)

#sjPlot::tab_df(table2data, file = "Table2.doc", digits = 2)

table3data <<- all_results%>%
  dplyr::group_by(Species, Method)%>%
  dplyr::summarize(Beta_range = paste(round(range(exp(Coef)),2),collapse = ", "),
            MEF = mean(pRSQ, na.rm = T),
            MPB = mean(pMPB, na.rm = TRUE),
            MPE = mean(pMPE, na.rm = TRUE),
            MDPE = mean(pMDPE, na.rm = TRUE),
            maxE= max(pmaxE, na.rm = TRUE))%>%
  #dplyr::mutate(dplyr::across(where(is.numeric), round, 3))%>%
  dplyr::mutate(round(dplyr::pick(where(is.numeric)),2))%>%
  dplyr::arrange(Species, Method)

#sjPlot::tab_df(table3data, file = "Table3.doc", digits = 2)

#________________________________________________________________________________________
##Next Section is correlating single raw counts to abundance in Kokanee Table 4 in paper

#New data frame called few to select counts by the ordinal number
#Remove 0s added for TAUC and remove Redfish as only a single year in a different region.
few = KO_cnts%>%
  dplyr::filter(SOURCE == 'OBSERVED', STREAM != 'REDFISH')%>%
  dplyr::group_by(STREAM, YEAR)%>%
  dplyr::mutate(COUNT = rank(DOY), p_lpeak = LIVE_COUNT/max(LIVE_COUNT))



few = dplyr::left_join(few, KO_fence , by = c("STREAM", "YEAR"))


# this is looped over all visual predictors and streams
lmfits = POfits = vector(mode = "list")
J = 9#Number of different COUNTS
Coef = RSQ = MPB = MPE = MDPE = maxE=TU_bias = TU_slope = TU_error = numeric()
i = 1
j = 1



results = data.frame(Coef = numeric(0),RSQ = numeric(0),
                     MPB = numeric(0),MPE = numeric(0),MDPE = numeric(0),
                     maxE = numeric(0))
for (i in 1:J){
  vis = few%>%dplyr::filter(COUNT == i)

  lmfits[[i]] <- lm(vis$FENCE_COUNT~(0 + vis$LIVE_COUNT))
  Coef[i]<-lmfits[[i]]$coef
  sqerr = (vis$FENCE_COUNT - predict(lmfits[[i]]))^2
  sqtot = (vis$FENCE_COUNT - mean(vis$FENCE_COUNT))^2
  RSQ[i] = 1 -  sum(sqerr)/sum(sqtot)#need this equation, because canned r-squared calculation is wrong for lm with 0 intercept(way overestimates)

  #predicted vs observed
  MPE[i] = mean(abs(vis$FENCE_COUNT - predict(lmfits[[i]]))/vis$FENCE_COUNT)#mean percent error
  MDPE[i] = median(abs(vis$FENCE_COUNT - predict(lmfits[[i]]))/vis$FENCE_COUNT)#median percent error
  MPB[i] = mean((vis$FENCE_COUNT - predict(lmfits[[i]]))/vis$FENCE_COUNT)
  maxE[i] = max(abs(vis$FENCE_COUNT - predict(lmfits[[i]]))/vis$FENCE_COUNT)#max percent error
  #}

  vec = c(Coef[i], RSQ[i], MPB[i], MPE[i], MDPE[i], maxE[i])
  results[i,] = vec
}

labels = data.frame(label = paste0("~R^{2} == ",round(results$RSQ,2)), COUNT = c(1:9),x = rep(10000,9), y = rep(10000,9))

table4data <<- results%>%
  dplyr::mutate(round(dplyr::pick(where(is.numeric)),2))

#sjPlot::tab_df(table4data, file = "Table4.doc", digits = 2)

}
