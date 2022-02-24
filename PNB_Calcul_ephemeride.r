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

head(ephem2$sunset)
date_sunset <- as.POSIXct(paste(ephem2$date,
                                ephem2$sunset,
                                sep = " "),
                          format = "%Y-%m-%d %H:%M")
ephem2$sunset_min <- hour(date_sunset) * 60 + minute(date_sunset)


# -----> Period : 01 july 2015 to 30 april 2016 ####
####################################################

summary(ephem2$date)
jul2015_apr2016 <- ephem2[ephem2$date <= "2016-04-30",]
summary(jul2015_apr2016$date)

# -----> delay betw 1 AM (60 min) and the sunrise
d_sunrise_1am <- jul2015_apr2016$sunrise_min - 60
summary(d_sunrise_1am)

# -----> delay betw 5 AM (300 min) and the sunrise
d_sunrise_5am <- jul2015_apr2016$sunrise_min - 300
summary(d_sunrise_5am)

# -----> delay btw 8 PM (1200 min) and the sunset
d_sunset_8pm <- 1200 - jul2015_apr2016$sunset_min
summary(d_sunset_8pm)

# -----> Period : 01 july 2016 to 30 april 2017 ####
####################################################
summary(ephem2$date)
jul2016_apr2017 <- ephem2[ephem2$date <= "2017-04-30" & ephem2$date >= "2016-07-01",]
summary(jul2016_apr2017$date)

# -----> delay betw 2 AM (120 min) and the sunrise
d_sunrise_2am <- jul2016_apr2017$sunrise_min - 120
summary(d_sunrise_2am)

# -----> delay betw 4 AM (240 min) and the sunrise
d_sunrise_4am <- jul2016_apr2017$sunrise_min - 240
summary(d_sunrise_4am)


# Calculating delay betw 4am and sunrise in minutes ####
########################################################
# -----> delay btw sunrise and 4 AM = 240 min
delay2 <- ephem2$sunrise_min - 240
summary(delay2)
summary(ephem2$sunrise_min)