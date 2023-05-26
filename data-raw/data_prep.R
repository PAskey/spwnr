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
Mission_ch = read.csv("./Mission_channel.csv")
KO_cnts = read.csv("./KO_cnts.csv")
KO_fence = read.csv("./KO_fence.csv")
Pink_data = read.csv("./Pink_data.csv")
spwnr = read.csv("./spwnr.csv")

#Summarize raw Kokanee counts into different visual indices.
KO_data = KO_cnts%>%
  dplyr::group_by(SPECIES = "KOKANEE", STREAM, YEAR)%>%
  dplyr::summarize(PEAK_COUNT = max(LIVE_COUNT), TAUC = spwnr::TAUC(DOY,LIVE_COUNT), GAUC = spwnr::GAUC(DOY,LIVE_COUNT))%>%
  ungroup()

KO_data = dplyr::full_join(KO_data, KO_fence, by = c('STREAM',"YEAR"))

#Pink data escapement needs to be converted to fish days to be comparable to KO
Pink_data = Pink_data%>%
  dplyr::mutate(SPECIES = "PINK", TAUC = TAUC_E*l*v, GAUC = GAUC_E*l*v)%>%
  dplyr::select(-c(l,v,TAUC_E, HARR_E, GAUC_E))

spwnr = dplyr::full_join(KO_data,Pink_data)

#Create fitting groups for cross validation testing
creek_groups = data.frame(
  STREAM = c("CATHEAD", "CHENEGA", "COLDSTREAM", "COUNTESS", "HAWKINS",
             "HAYDEN", "HERRING", "IRISH", "LOOMIS", "MIDDLE_VERNON", "PENTICTON",
             "REDFISH"),
  fit_group = c(1,1,1,1,2,2,2,3,3,2,3,3))

spwnr = dplyr::left_join(spwnr, creek_groups, by = 'STREAM')%>%
  dplyr::mutate(SPECIES = as.factor(SPECIES))

#SAve .rda files to data folder
save(Mission_ch, file = "../data/Mission_ch.rda")
save(spwnr, file = "../data/spwnr.rda")
save(KO_cnts, file = "../data/KO_cnts.rda")
}

