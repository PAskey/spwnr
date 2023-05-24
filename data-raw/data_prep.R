




Mission_ch = read.csv("./data-raw/Mission_channel.csv")
KO_cnts = read.csv("./data-raw/KO_cnts.csv")
KO_fence = read.csv("./data-raw/KO_fence.csv")
spwnr = read.csv("./data-raw/spwnr.csv")

#usethis::use_data(spwnr, overwrite = TRUE, fileEncoding="UTF-8-BOM")
#use_data not working for some reason.

save(Mission_ch, file = "./data/Mission_ch.rda")

vis = KO_cnts%>%
  group_by(STREAM_YR)%>%
  dplyr::summarize(TAUC = TAUC(DOY, LIVE_COUNT), GAUC = GAUC(DOY, LIVE_COUNT), PC = max(LIVE_COUNT), top_3 = mean(LIVE_COUNT))


vis = left_join(vis,fence, by = "STREAM_YR")%>%select(-c(STREAM_YR, PEAK_VISUAL))

##______________________________________________________________________________________________
