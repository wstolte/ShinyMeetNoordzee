#######################################################################
####                                                               ####
####          Script by willem.stolte@deltares.nl                  ####
####    Reads MWTL csv and saves file for presentation purpose     ####
####                                                               ####
####                copyright Deltares                             ####
####                                                               ####
#######################################################################

require(scales)
require(plyr)
require(reshape2)
require(ggplot2)
require(readr)

## script eats waterbase data (live.waterbase.nl)
## and produces .Rdata file for use in Shiny app

submap <- read_csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2names.csv")#, header = T, stringsAsFactors=FALSE)
locmap <- read_csv2("d:/Tools_Scripts/Mapping tables/RWS2DELWAQ2locations_new.csv",
                    col_types = list(
#                       GEBIED = col_factor(),
#                       GEBOMSCH = col_factor(),
#                       LOCATIE = col_factor(),
#                       LOCOMSCH = col_factor(),
                      X_WGS = col_double(),
                      Y_WGS = col_double(),
                      X_RD = col_double(), 
                      Y_RD = col_double(),
                      Delwaq_ED = col_character(),
                      IMARES_st = col_character()
                    ))
save(locmap, file = "map.rdata")



# locmod = c("Huibertgat_oost", "IMARES_st_2", "IMARES_st_3b", "Bocht_van_Watum", "IMARES_st_4b", "IMARES_st_5", "Groote_Gat_noord")
rws_dat           <- read_csv2("collected-data.csv",
                               col_types = list(
                                 #                                  locatie = col_factor(),
                                 #                                  waarnemingssoort = col_factor(),
                                 bepalingsgrenscode = col_skip(),
                                 #                                  waarde = col_double(),
                                 #                                  eenheid = col_factor(),
                                 hoedanigheid = col_skip(),
                                 anamet = col_skip(),
                                 ogi = col_skip(),
                                 vat = col_skip(),
                                 bemhgt = col_skip(),
                                 refvlk = col_skip(),
                                 EPSG = col_skip(),
                                 orgaan = col_skip()
                               ))
rws_dat$locatie <- as.factor(rws_dat$locatie) 
rws_dat$datetime   <- as.POSIXct(
  paste(rws_dat$datum, rws_dat$tijd),
  format = "%Y-%m-%d %H:%M")
rws_dat$variable   <- mapvalues(as.character(rws_dat$waarnemingssoort), from = submap$RWS_wns, to = submap$NL_name, warn_missing = F)
# rws_dat$location   <- mapvalues(as.character(rws_dat$locoms), from = locmap$locoms, to = locmap$Delwaq_ED, warn_missing = F)
# rws_dat$variable   <- mapvalues(as.character(rws_dat$variable), from = submap$Delwaq, to = submap$Delwaq_long_name, warn_missing = F)
rws_dat$month      <- format(rws_dat$datetime, format = "%m")
rws_dat$year <- as.numeric(format(rws_dat$datetime, "%Y"))
rws_dat$season <- ifelse(rws_dat$month %in% c("10", "11", "12", "01", "02"), "winter", "summer")
# rws_dat <- subset(rws_dat, rws_dat$kwccod < 50 )
# save(rws_dat, file = "d:/Tools_Scripts/R/ShinyMeetVeersemeer/data/MWTL_Veersemeer_bewerkt.Rdata")

#filter for high PO4 and NH4 measurements
rws_dat <- subset(rws_dat, rws_dat$waarde < 2 | rws_dat$variable != "opgelost fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 4 | rws_dat$variable != "totaal opgelost fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 3 | rws_dat$variable != "totaal fosfaat")
rws_dat <- subset(rws_dat, rws_dat$waarde < 2 | rws_dat$variable != "ammonium")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 75 | rws_dat$variable != "chlorophyll-a")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 3 | rws_dat$variable != "N pg")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 30 | rws_dat$variable != "dissolved org C")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 500 | rws_dat$variable != "suspended solids")
# rws_dat <- subset(rws_dat, rws_dat$waarde < 1000 )
# rws_dat <- subset(rws_dat, rws_dat$waarde >= 0 )
# rws_dat <- subset(rws_dat, rws_dat$kwccod == 0 )
# 
# save(rws_dat, file = "d:/Tools_Scripts/R/ShinyMeetVeersemeer/data/MWTL_Veersemeer_filtered.Rdata")
save(rws_dat, file = "R/DATA/MWTL_Noordzee_bewerkt.Rdata")


