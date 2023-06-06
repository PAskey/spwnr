#' This R code takes available raw data for Kokanee and Pink salmon and formats for analyses.
#' Pink fence and AUC data is take from Millar et al. 2012 CJFAS
#' Pink peak counts are taken from Hilborn et al. 1999 Table 1, or digitizing from figures in Millar et al. 2012.

#' @title data_prep
#' @name data_prep
#' @keywords spwnr, data_prep
#' @export
#' @examples
#' data_prep()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


data_prep <- function(){

#Read in raw data from csv files in data-raw
All_cnts = read.csv("./SPAWNER_COUNTS.csv")%>%
  dplyr::filter(INCLUDED == 'Y')%>%#Stream data that were QC'd and included in previous publications
  dplyr::arrange(SPECIES, YEAR, STREAM, DOY)%>%
  dplyr::mutate(STREAM_YR = paste0(STREAM,"_",YEAR),
                GROUND_INTERP = tidyr::replace_na(GROUND_INTERP,0),
                FENCE_INTERP = tidyr::replace_na(FENCE_INTERP,0),
                ADDED_0 = tidyr::replace_na(ADDED_0,0))

#Create a simulation of KOkanee type counting frequency for Pink ground data
ko = c(182,seq(220,252, by = 4))#Every 4th day starting on day 220. Assume 0 on day 182 similar to KO assumptions.

All_cnts = All_cnts%>%
  dplyr::mutate(KO_sim = dplyr::if_else(SPECIES == 'PINK'&DOY%in%ko|SPECIES == 'KOKANEE',1,0))


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

#Mission_ch = read.csv("./Mission_channel.csv")
#KO_cnts = read.csv("./KO_cnts.csv")
#KO_fence = read.csv("./KO_fence.csv")
#Pink_data = read.csv("./Pink_data.csv")
#spwnr = read.csv("./spwnr.csv")

#Summarize raw Kokanee counts into different visual indices.
#KO_data = KO_cnts%>%
#  dplyr::group_by(SPECIES = "KOKANEE", STREAM, YEAR)%>%
#  dplyr::summarize(PEAK_COUNT = max(LIVE_COUNT), TAUC = spwnr::TAUC(DOY,LIVE_COUNT), GAUC = spwnr::GAUC(DOY,LIVE_COUNT))%>%
#  ungroup()

#KO_data = dplyr::full_join(KO_data, KO_fence, by = c('STREAM',"YEAR"))

#Pink data escapement needs to be converted to fish days to be comparable to KO
#Pink_data = Pink_data%>%
#  dplyr::mutate(SPECIES = "PINK", TAUC = TAUC_E*l*v, GAUC = GAUC_E*l*v)%>%
#  dplyr::select(-c(l,v,TAUC_E, HARR_E, GAUC_E))

#spwnr = dplyr::full_join(KO_data,Pink_data)



#SAve .rda files to data folder
save(All_cnts, file = "../data/All_cnts.rda")
#save(Mission_ch, file = "../data/Mission_ch.rda")
save(spwnr_ests, file = "../data/spwnr_ests.rda")
#save(KO_cnts, file = "../data/KO_cnts.rda")
}

