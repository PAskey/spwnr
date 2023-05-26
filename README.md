# spwnr
This package was created to make the raw data and functions available from 'Comparison of known spawner abundance from fence counts to visual counts for simplified spawner estimation methods.' Askey et al. (2023)in review) in the Canadian Journal of Fisheries and Aquatic Sciences.      Includes data comparing visual counts of Kokanee spawners to census data from spawner fences. 

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

  * More detailed explanations of any of the functions below can be founds by typing ?function into the R console. E.g. ?GAUC. If you want to see the raw source code for any of the functions type View(function name) e.g. View(GAUC). If you would like to make changes of that code, ideally you go to my Github account to get the source code and create a branch with your potential edits.

There are currently 4 data sets available that were used in the published data analyses: KO_cnts, KO_fence, Mission_ch, and spwnr. 

These data sets are accessible in R by simply entering the dataset name. To bring the data into the RStudio environment, you can use data() at the prompt

```R
#These data sets are accessible in R by name.
#To verify which data sets are included in a package use data():
data(package = "spwnr")

#The data sets are accessible by name. For example to see column names for spwnr dataset:
names(KO_cnts)

#To bring the data into your R environment use data() function. There is one dataset that has the same name as the package. If you pute spwn without "" in the data function it loads the data set.
data(spwnr)

```
The data in the spwnr data set is the summarized data set with 3 different visual indices: PEAK_COUNT, TAUC and GAUC. All explained in ASkey et al. in review. The functions to calcualte TAUC and GAUC are included in this package by name and re-craetion of the summary data set can be accomplished as follows:

```R
library(dplyr)

KO_cnts = KO_cnts %>%# mutate_if(is.numeric,as.integer)%>%
  #mutate_if(is.character, as.factor)%>%
  filter(SOURCE == 'OBSERVED')%>%
  arrange(STREAM,YEAR,DOY)

vis = KO_cnts%>%
  group_by(STREAM_YR)%>%
  dplyr::summarize(TAUC = TAUC(DOY, LIVE_COUNT), GAUC = GAUC(DOY, LIVE_COUNT), PC = max(LIVE_COUNT))

vis = left_join(vis,KO_fence, by = "STREAM_YR")%>%select(-c(STREAM_YR, PEAK_VISUAL))

```
