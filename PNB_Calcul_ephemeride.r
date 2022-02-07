library(suncalc)
library(lubridate)

date <- as.Date(strptime("01-10-2015", "%d-%m-%Y"))

date <- seq(from = as.Date(strptime("01-07-2015", "%d-%m-%Y")),
            to = as.Date(strptime("31-12-2022", "%d-%m-%Y")),
            by = 1)

class(date)
# ?getSunlightTimes
ephem <- getSunlightTimes(date = date,
                          lat = -20.882057,
                          lon = 55.450675,
                          keep = c("sunrise",
                                   "sunset",
                                   "dusk",
                                   "nauticalDusk",
                                   "night"),
                          tz = Sys.timezone())

# To clean the hours in the table
# class(ephem$dusk)
# ephem$dusk2 <- format(as.POSIXct(ephem$dusk), format = '%H:%M')

eph <- apply(ephem[, c("sunset",
                       "dusk",
                       "nauticalDusk",
                       "night",
                       "sunrise")],
             MARGIN = 2,
             FUN = function(w) format(as.POSIXct(w), format = "%H:%M"))

ephem2 <- cbind(ephem[, c("date", "lat", "lon")],
                eph)
head(ephem2)
summary(ephem2)

# write.table(ephem2,
#             file = "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_ephemeride_2015-2022.txt",
#             row.names = F)

# Calculating delay betw 5am and sunrise in minutes ####
########################################################

head(ephem2)
date_sunrise <- as.POSIXct(paste(ephem2$date,
                                 ephem2$sunrise,
                                 sep = " "),
                           format = "%Y-%m-%d %H:%M")
date_sunrise
head(ephem2$sunrise)
head(date_sunrise)

hour(date_sunrise)
minute(date_sunrise)

ephem2$sunrise_min <- hour(date_sunrise) * 60 + minute(date_sunrise)
head(ephem2)
tail(ephem2)

# -----> delay btw sunrise and 5 AM = 300 min
delay <- ephem2$sunrise_min - 300
summary(delay)
summary(ephem2$sunrise_min)

# Calculating delay betw 4am and sunrise in minutes ####
########################################################
# -----> delay btw sunrise and 4 AM = 240 min
delay2 <- ephem2$sunrise_min - 240
summary(delay2)
summary(ephem2$sunrise_min)