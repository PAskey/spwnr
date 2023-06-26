# spwnr
This package was created to make the raw data and functions available from 'Comparison of known spawner abundance from fence counts to visual counts for simplified spawner estimation methods.' Askey et al. (in review) in the Canadian Journal of Fisheries and Aquatic Sciences.

The package provides open access to the raw data used in the above publication. The Kokanee data are from the Fish and Wildlife branch in Penticton, BC, and provided by the paper authors. The Pink Salmon data were collected y the Alaska Department of Fish and Game, and have been published in several papers: Bue et al. 1998, English et al. (1992), Hilborn et al. (1999), and Millar et al. (2012, 2013). The raw data for the Pinks were provided by R. Millar, University of Aukland (lead author on 2012,2013 papers).


# Installation
Install with devtools package from PAskey github account

```R
#First install the devtools package from CRAN with:
install.packages("devtools")

#Next install the SPDT package from PAskey Github repo as:
devtools::install_github("PAskey/spwnr").
```

# Usage
The primary objective of this package is to make data available to others, and document code for key functions in the paper: 'Comparison of known spawner abundance from fence counts to visual counts for simplified spawner estimation methods.'

# Main functions and uses

There are two functions in the package: GAUC() and TAUC(). Each produces an estimate of total fish days based on a series of counts.

More detailed explanations of the functions below can be founds by typing ?function into the R console after the package has been loaded. E.g. ?GAUC. If you want to see the raw source code for any of the functions type View(function name) (e.g. View(GAUC)) or review in the R folder for this package on Github. If you would like to make changes of that code, ideally you go to my Github account to get the source code and create a branch with your potential edits.

There are currently 2 data sets available as .rda files that were used in the published data analyses: All_cnts and spwnr_ests. Type ?All_cnts for details.

These data sets are accessible in R by simply entering the dataset name when the package is loaded. To bring the data into the RStudio environment, you can use data() at the prompt.

```R
#First make sure pacakge is loaded.
library(spwnr)


#These data sets are accessible in R by name when package is loaded.
#To verify which data sets are included in any package use data():
data(package = "spwnr")

#The data sets are accessible by name. For example to see column names for spwnr_ests dataset:
names(spwnr_ests)

#To bring the data into your R environment you can use use data() function.
data(spwnr_ests)

#Or just call directly and assign to a name e.g. df
df = spwnr_ests

```
The data in the spwnr_ests data set is the summarized data set with 3 different visual indices: PEAK_COUNT, TAUC and GAUC. All explained in Askey et al. in review. The functions to calculate TAUC and GAUC are included in this package. You can delve into data-raw folder on Github to see the raw files and code to crate the two data sets.

```R
#Code to create and save first two figures in publication
#Copy and paste into your console after installing package as instructed above.
#Figures will be saved into your working directory.

library(tidyverse)
library(spwnr)


#Kokanee
All_cnts = spwnr::All_cnts%>%
  dplyr::group_by(STREAM_YR, YEAR, STREAM)%>%
  tidyr::complete(DOY = seq(min(DOY), max(DOY)))%>%#Need complete to smooth out predictions in plot
  dplyr::mutate(preds = GAUC(t = DOY, obs = GROUND_LIVE, out = 'preds'))

KO = All_cnts%>%dplyr::filter(SPECIES=='KOKANEE', STREAM != 'MISSION')
sc = 1000
#Figure 1 KO
ggplot(KO, aes(x = DOY, y = GROUND_LIVE/sc, group = STREAM))+
  geom_area(aes(x = DOY, y = FENCE_PASS/sc), fill = "light grey")+
  geom_line(aes(y = preds/sc), colour = 'dark grey', lwd = 1.1)+
  geom_line(data = KO[!is.na(KO$GROUND_LIVE),], lty = 2)+
  geom_line(data = KO[!is.na(KO$GROUND_LIVE)&KO$GROUND_INTERP == 0,])+
  geom_point(data = KO[KO$GROUND_INTERP == 0,], size = 2)+
  facet_wrap(~STREAM_YR, scales = "free", ncol = 3, dir = 'v')+
  labs(y = "Live count (thousands)", x = "Day of year")+
  theme_bw(base_size = 10)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "Figure_1.png", width = 6.5, height = 8.5, dpi = 300, units = "in")

#Pink data and estimate of live fish present as per Bue et al. 1998
PK = All_cnts%>%
  filter(SPECIES=='PINK')%>%
  group_by(STREAM_YR)%>%
  mutate(
  EST_LIVE = cumsum(replace_na(FENCE_PASS,0))-cumsum(replace_na(GROUND_NEW_DEAD,0))
  )

PK$EST_LIVE[PK$EST_LIVE<0]=0

ggplot(PK, aes(x = DOY, y = GROUND_LIVE/sc))+
  geom_line(aes(y = EST_LIVE/sc), colour = 'grey')+
  geom_point(pch = 1, colour = 'grey')+
  geom_point(data = PK[PK$ADDED_0 == 0&!is.na(PK$AIR_LIVE),],aes(y = AIR_LIVE/sc), pch = 24, size = 2,fill = 'grey')+
  geom_line(data = PK[PK$ADDED_0 == 0&!is.na(PK$AIR_LIVE),],aes(x = DOY, y = AIR_LIVE/sc))+
  geom_line(data = PK[PK$ADDED_0 == 0&PK$KO_sim ==1,],aes(x = DOY, y = GROUND_LIVE/sc))+
  geom_point(data = PK[PK$ADDED_0 == 0&PK$KO_sim ==1,],aes(x = DOY, y = GROUND_LIVE/sc), pch = 21, size = 2, fill = 'grey')+
  geom_line(data = PK[!is.na(PK$AIR_LIVE),],aes(x = DOY, y = AIR_LIVE/sc), lty = 2)+
  geom_line(data = PK[PK$ADDED_0 == 1|PK$KO_sim ==1,],aes(x = DOY, y = GROUND_LIVE/sc), lty = 2)+
  xlim(190,275)+#truncates some obs for Irish Creek at start
  facet_wrap(~STREAM_YR, scales = "free", ncol = 3, dir = 'v')+
  labs(y = "Live count (thousands)", x = "Day of year")+
  theme_bw(base_size = 10)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "Figure_2.png", width = 6.5, height = 8.5*.8, dpi = 300, units = "in")

```
# References

Bue, B.G., Fried, S.M., Sharr, S., Sharp, D.G., Wilcock, J.A., and Geiger, H.J., 1998. Estimating salmon escapement using the area-under-the-curve, aerial observer efficiency and stream-life estimates: The Prince William Sound pink salmon example, N. Pac. Anadr. Fish Comm. Bull, 1 pp. 240-250.

 English, K.K., Bocking, R.C. and Irvine, J.R., 1992. A robust procedure for estimating salmon escapement based on the area-under-the-curve method. Canadian Journal of Fisheries and Aquatic Sciences, 49(10), pp.1982-1989.
 
 Hilborn, R., Bue, B.G. and Sharr, S., 1999. Estimating spawning escapements from periodic counts: a comparison of methods. Canadian Journal of Fisheries and Aquatic Sciences, 56(5), pp.888-896.
 
 Millar, R.B., McKechnie, S. and Jordan, C.E., 2012. Simple estimators of salmonid escapement and its variance using a new area-under-the-curve method. Canadian journal of fisheries and aquatic sciences, 69(6), pp.1002-1015.
 
  Millar, R.B. and Jordan, C.E., 2013. A simple variance estimator for the trapezoidal area-under-the-curve estimator of the spawner abundance of Pacific salmon. Canadian journal of fisheries and aquatic sciences, 70(8), pp.1231-1239.
  
  
