library(suncalc)
library(lubridate)

date <- as.Date(strptime('01-10-2015', '%d-%m-%Y'))

date <- seq(from = as.Date(strptime('01-07-2015', '%d-%m-%Y')), to = as.Date(strptime('31-12-2022', '%d-%m-%Y')), by = 1)

class(date)

ephem <- getSunlightTimes(date = date,
                          lat = -20.882057,
                          lon = 55.450675,
                          keep = c('sunset', 'dusk', 'nauticalDusk', 'night'),
                          tz = Sys.timezone())

# To clean the hours in the table
# class(ephem$dusk)
# ephem$dusk2 <- format(as.POSIXct(ephem$dusk), format = '%H:%M')

eph <- apply(ephem[, c('sunset', 'dusk', 'nauticalDusk', 'night')],
             2,
             function(w) format(as.POSIXct(w), format = '%H:%M'))

ephem2 <- cbind(ephem[, c('date', 'lat', 'lon')], eph)


write.table(ephem2,
            file = "PNB_ephemeride_2015-2022.txt",
            row.names = F)
