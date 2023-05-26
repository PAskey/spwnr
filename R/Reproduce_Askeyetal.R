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
spwnr = spwnr%>%mutate(
  lTAUC = log(TAUC),
  lGAUC = log(GAUC),
  lPEAK = log(PEAK_COUNT),
  lFENCE = log(FENCE_COUNT)
)%>%
  relocate(lTAUC, lGAUC, lPEAK, TAUC, GAUC, PEAK_COUNT, lFENCE, FENCE_COUNT)

results = all_results = list()

for (S in levels(spwnr$SPECIES)){
  for (j in 0:J){
    vis = spwnr%>%dplyr::filter(fit_group != j, SPECIES == S)
    for (i in 1:L)
    {
      lmfits[[i]] <- lm(vis$lFENCE~ 1 + offset(pull(vis[,i])))
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
      psqerr = (spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S] - (exp(Coef[i])*pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))^2
      psqtot = (spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S] - mean(spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S]))^2
      pRSQ[i] = 1 -  sum(psqerr)/sum(psqtot)
      pMPB[i] = mean((spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pMPE[i] = mean(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pMDPE[i] = median(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
      pmaxE[i] = max(abs(spwnr$FENCE_COUNT[spwnr$fit_group == j& spwnr$SPECIES == S] - (exp(Coef[i])*pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L])))/spwnr$FENCE_COUNT[spwnr$fit_group == j & spwnr$SPECIES == S])
    }
    results[[j+1]] <- data.frame(Group_out = rep(j,L), Method =names(vis[1:L]),
                                 Coef, RSQ, MPB, MPE, MDPE, maxE, pRSQ, pMPE, pMPB, pMDPE, pmaxE)

  }

  all_results[[S]] = do.call("rbind",results)%>%mutate(Species = S)
}

all_results = do.call("rbind",all_results)
rownames(all_results)<-NULL
#factorize to preserve specific order in tables
all_results = all_results%>%mutate(Method = substring(Method, 2), Method = factor(Method, c('PEAK', "TAUC", "GAUC")))

#Summary of within sample fits to data
table2data = all_results%>%filter(Group_out==0)%>%#, Species == "KOKANEE"
  mutate(Beta = exp(Coef))%>%
  arrange(Species, Method)%>%
  select(Species, Method, Beta, RSQ,MPB, MPE, MDPE, maxE)

#sjPlot::tab_df(table2data, file = "Table2.doc", digits = 2)

table3data = all_results%>%group_by(Species, Method)%>%
  summarize(Beta_range = paste(round(range(exp(Coef)),2),collapse = ", "),
            MEF = mean(pRSQ, na.rm = T),
            MPB = mean(pMPB, na.rm = TRUE),
            MPE = mean(pMPE, na.rm = TRUE),
            MDPE = mean(pMDPE, na.rm = TRUE),
            maxE= max(pmaxE, na.rm = TRUE))%>%
  mutate(across(where(is.numeric), round, 3))%>%
  arrange(Species, Method)

#sjPlot::tab_df(table3data, file = "Table3.doc", digits = 2)

}
