#' This R code takes available raw data for Kokanee and Pink salmon and formats for analyses.
#' Pink fence and ground data was provided from R. Millar, see  Millar et al. 2012 CJFAS
#' Kokanee data from P Askey, and BC government staff in Penticton.

#' @title data_prep
#' @name data_prep
#' @keywords spwnr, data_prep
#' @export
#' @examples
#' data_prep()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


data_prep <- function(){

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Read in raw data from csv files in data-raw
All_cnts = read.csv("./SPAWNER_COUNTS.csv")%>%
  dplyr::filter(INCLUDED == 'Y')%>%#Stream data that were QC'd and included in previous publications
  dplyr::arrange(SPECIES, YEAR, STREAM, DOY)%>%
  dplyr::mutate(STREAM_YR = paste0(STREAM,"_",YEAR),
                GROUND_INTERP = tidyr::replace_na(GROUND_INTERP,0),
                FENCE_INTERP = tidyr::replace_na(FENCE_INTERP,0),
                ADDED_0 = tidyr::replace_na(ADDED_0,0))%>%
  dplyr::select(-INCLUDED)

#Createa data set of 0 assumptions
GR_Added_0s = All_cnts%>%
  filter(!is.na(GROUND_LIVE), ADDED_0 == 0)%>%
  group_by(SPECIES, STREAM_YR)%>%
  summarize(tmin = min(DOY),
            tmin_C = GROUND_LIVE[order(min(DOY))],
            tmax = max(DOY),
            tmax_C = GROUND_LIVE[order(max(DOY))])

#Create a simulation of KOkanee type counting frequency for Pink ground data
#Every 4th day starting on day 222 to a total of 9 counts (Except Cathead 1990 which is missing data on day 254).

ko = seq(222,by = 4, length.out = 9)

#Also keep the added 0 values to end AUC range similar to KO assumptions (e.g. Assume 0 on day 182 ).
All_cnts = All_cnts%>%
  dplyr::mutate(KO_sim = dplyr::if_else(SPECIES == 'PINK'&DOY%in%ko|SPECIES == 'KOKANEE'|ADDED_0==1,1,0))


spwnr = All_cnts%>%
  dplyr::group_by(STREAM_YR, STREAM, YEAR, SPECIES)%>%
  dplyr::filter(sum(FENCE_PASS, na.rm = T)>0)%>%
  dplyr::summarize(
    FENCE = sum(FENCE_PASS, na.rm = T)
  )

ground = All_cnts%>%
  dplyr::group_by(STREAM_YR)%>%
  dplyr::filter(STREAM_YR%in%spwnr$STREAM_YR, !is.na(GROUND_LIVE), KO_sim == 1)%>%
  dplyr::rename(Obs = GROUND_LIVE)%>%
  dplyr::summarize(
    PEAK_COUNT = max(Obs, na.rm = T),
    TAUC = TAUC(DOY,Obs),
    GAUC = GAUC(DOY,Obs)
  )%>%
  dplyr::mutate(Method = 'GROUND')

air = All_cnts%>%
  dplyr::group_by(STREAM_YR)%>%
  dplyr::filter(!is.na(AIR_LIVE))%>%
  dplyr::rename(Obs = AIR_LIVE)%>%
  dplyr::summarize(
    PEAK_COUNT= max(Obs, na.rm = T),
    TAUC = TAUC(DOY,Obs),
    GAUC = GAUC(DOY,Obs)
  )%>%
  dplyr::mutate(Method = 'AIR')

methods = rbind(ground, air)
spwnr_ests = dplyr::right_join(spwnr,methods)%>%
  dplyr::ungroup()

#Create fitting groups for cross validation testing
creek_groups = data.frame(
  STREAM = c("CATHEAD", "CHENEGA", "COLDSTREAM", "COUNTESS", "HAWKINS",
             "HAYDEN", "HERRING", "IRISH", "LOOMIS", "MIDDLE_VERNON", "PENTICTON",
             "REDFISH"),
  fit_group = c(1,1,1,1,2,2,2,3,3,2,3,3))

spwnr_ests = dplyr::left_join(spwnr_ests, creek_groups, by = 'STREAM')%>%
  dplyr::mutate(SPECIES = as.factor(SPECIES))

rm(air, ground, spwnr)


#SAve .rda files to data folder
save(All_cnts, file = "../data/All_cnts.rda")
save(spwnr_ests, file = "../data/spwnr_ests.rda")
write.csv(All_cnts, file = "./All_cnts.csv", row.names = F)
write.csv(spwnr_ests, file = "./spwnr_ests.csv", row.names = F)

}

