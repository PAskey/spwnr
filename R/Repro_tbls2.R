lmfits <- vector(mode = "list")
L <- 3  # Number of visual index columns
J <- max(spwnr$fit_group)
Coef <- RSQ <- MPB <- MPE <- MDPE <- maxE <- pRSQ <- pMPE <- pMPB <- pMDPE <- pmaxE <- vector(length = L)

# log-log scale
spwnr <- spwnr %>%
  mutate(
    lTAUC = log(TAUC),
    lGAUC = log(GAUC),
    lPEAK = log(PEAK_COUNT),
    lFENCE = log(FENCE_COUNT)
  ) %>%
  relocate(lTAUC, lGAUC, lPEAK, TAUC, GAUC, PEAK_COUNT, lFENCE, FENCE_COUNT)

i = 1
j = 1
S = 'KOKANEE'
results <- all_results <- list()

for (S in levels(spwnr$SPECIES)){
  for (j in 0:J){
    vis <- spwnr %>% filter(fit_group != j, SPECIES == S)
    temp = NULL
    for (i in 1:L) {
      lmfits[[i]] <- lm(vis$lFENCE ~ 1 + offset(dplyr::pull(vis[, i])))
      Coef[i] <- lmfits[[i]]$coef

      #Within sample predictions
      vis$Method = names(vis[,i])
      vis$Group_out = j
      vis$coef = exp(Coef[i])
      vis$pred = exp(predict(lmfits[[i]]))

      #Out of sample predictions
      out = spwnr %>% filter(fit_group == j, SPECIES == S)
      out$Method = names(out[,i])
      out$Group_out = j
      out$coef = exp(Coef[i])
      out$pred = (exp(Coef[i])*dplyr::pull(spwnr[spwnr$fit_group == j & spwnr$SPECIES == S,i+L]))

      temp=rbind(temp, vis, out)
    }
    results[[j+1]] = temp
  }
  all_results[[S]] = do.call("rbind",results)
}

all_results = do.call("rbind",all_results)
#Tidy up data frame for tables and plotting
rownames(all_results)<-NULL
all_results = all_results%>%
  mutate(Method = substring(Method, 2), Method = factor(Method, c('PEAK', "TAUC", "GAUC")))%>%
  rename(Species = SPECIES)

#If the fit_group removed from fitting is '0' then all groups were included in fitting
in_sample = all_results%>%filter(Group_out == 0)

Table2 = in_sample%>%
  group_by(SPECIES, Method)%>%
  summarize(Beta = round(coef,2)[1],
            EF = 1 - (sum((pred-FENCE_COUNT)^2)/sum((mean(FENCE_COUNT)-FENCE_COUNT)^2)),
            MRB = mean((pred-FENCE_COUNT)/FENCE_COUNT),
            MRE = mean(abs(pred-FENCE_COUNT)/FENCE_COUNT),
            MdRE = median(abs(pred-FENCE_COUNT)/FENCE_COUNT),
            maxRE = max(abs(pred-FENCE_COUNT)/FENCE_COUNT))

#If the group out matches the fit group, then predictions were made form coefficient fit to other fit groups
out_sample = all_results%>%filter(Group_out != 0, fit_group==Group_out)

Table3 = out_sample%>%
  group_by(SPECIES, Method)%>%
  summarize(Beta_range = paste(round(range(coef),2),collapse = ", "),
            EF = 1 - (sum((pred-FENCE_COUNT)^2)/sum((mean(FENCE_COUNT)-FENCE_COUNT)^2)),
            MRB = mean((pred-FENCE_COUNT)/FENCE_COUNT),
            MRE = mean(abs(pred-FENCE_COUNT)/FENCE_COUNT),
            MdRE = median(abs(pred-FENCE_COUNT)/FENCE_COUNT),
            maxRE = max(abs(pred-FENCE_COUNT)/FENCE_COUNT))

#One stream with a huge error is Herring 1990
t = c(192,197,199,204,207,212,215,218,226,234,243,248)
obs = c(0,0,0,0,0,0,25,1000,200,1100,750,2700)

df = data.frame(t = seq(day1,lday), preds = GAUC(DOY,LIVE, out = 'preds'))
plot(df$t,df$preds)

#test GAUC
GAUC(DOY,LIVE)
GAUC(DOY,LIVE, out = 'Millar')

x = GAUC(t,obs, day1 = 1, lday = 1000, out = 'preds')
plot(x)
max(x)
#write.csv(out_sample, "out_sample.csv")

Long_data = in_sample%>%select(SPECIES, STREAM, YEAR, FENCE_COUNT, PEAK_COUNT, GAUC, TAUC, pred)%>%
  gather(vis, val, -c(SPECIES, STREAM, YEAR, FENCE_COUNT, pred))

  Pinklong = Pink%>%gather(vis, val, -c(SPECIES, STREAM, YEAR, FENCE_COUNT))

ggplot(data = Long_data, aes(x = val/sc, y = FENCE_COUNT/sc, group = SPECIES))+
  geom_point(aes(shape = SPECIES, fill = STREAM), size = 2)+
  geom_point(aes(x = val/sc, y = pred/sc), colour = 'blue')+
  #geom_line(aes(x = val/sc, y = pred/sc,lty = SPECIES))+
  geom_smooth(method = 'lm', se = F, formula = y ~ x + 0, aes(lty = SPECIES), colour = 'black', lwd = 0.75)+
  #geom_smooth(method = 'lm', se = F,formula=y~x-1, aes(lty = SPECIES))+
  facet_wrap(~vis, scales = "free")+
  labs(x = 'Visual index value (tens of thousands)', y = 'Fence count (tens of thousands)')+
  scale_shape_manual(values=c(21,24))+
  scale_size_manual(values = c(4,3))+
  scale_fill_grey()+
  theme_bw()+
  theme(legend.position="none")+ coord_trans(x="log", y="log")
