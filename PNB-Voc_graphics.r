# -------------------------------------------------- #
#### Graphic production for vocalization dynamic ####
# ------------------------------------------------ #

#### Vocalization per night ####
# -----------------------------

voc <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_voc_night.txt',
                  sep = '\t',
                  dec = ',',
                  head = T)

summary(voc)
voc$Minuts <- as.factor(voc$Minuts)
voc$Minuts <- relevel(voc$Minuts, '60-120')

# png("G:/Mon Drive/Projet_Publis/PNB_COLONIE/Figures/PNB_daily_voc2.tiff",
# res = 300,
# width = 35,
# height = 25,
# pointsize = 12,
# unit = "cm",
# bg = "transparent")

barplot(voc$mean,
        names.arg = voc$Minuts,
        ylim = c(0, 30),
        las = 1,
        density = NULL,
        border = NA,
        col = rgb(1, 1, 1),
        xlab = 'Time after the sunset (min)',
        xaxt = 'n',
        ylab = 'Mean proportion of Masacarene Petrel vocalization (%)',
        # cex.lab = 1.5,
        cex.axis = 1.5)
grid(NA, 6)
b <- barplot(voc$mean,
        names.arg = voc$Minuts,
        ylim = c(0, 30),
        angle = 45,
        density = NULL,
        border = NA,
        col = rgb(0, 0, 0, 0.35),
        add = T,
        yaxt = 'n',
        xaxt = 'n')
arrows(b[,1],
       voc$mean - voc$sd,
       b[,1],
       voc$mean + voc$sd,
       length = 0.05,
       angle = 90,
       code = 3)
axis(side = 1,
     at = b[,1],
     tick = F,
     labels = as.character(voc$Minuts),
     cex.lab = 1.8)

# dev.off()
# 
# library(ggplot2)
# ggplot(voc,
#        aes(x = Minuts, y = mean)) +
#   geom_bar(stat = 'identity',
#            fill=rgb(0.1,0.4,0.5,0.7)) +
#   geom_errorbar(aes(x = Minuts, ymin = mean - sd, ymax = mean + sd),
#                 width = 0.1,
#                 colour = 'black',
#                 alpha = 0.4,
#                 size = 0.8) +
#   expand_limits(y = c(0, 30))

#### Vocalization per night ####
# -----------------------------

voc <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_voc_month.txt',
                  sep = '\t',
                  dec = ',',
                  head = T)

summary(voc)
voc$month2 <- factor(voc$month,
                       ordered = F)

voc$rate <- round(voc$rate, digits = 2)
barplot(voc$rate,
        names.arg = voc$month2,
        ylim = c(0,70))
arrows(b[,1],
       voc$rate,
       b[,1],
       voc$rate + voc$sd,
       length = 0.05,
       angle = 90,
       code = 2)

# png("G:/Mon Drive/Projet_Publis/PNB_COLONIE/Figures/PNB_daily_voc2.tiff",
# res = 300,
# width = 35,
# height = 25,
# pointsize = 12,
# unit = "cm",
# bg = "transparent")

barplot(voc$rate,
        names.arg = voc$month2,
        ylim = c(0, 180),
        las = 1,
        density = NULL,
        border = NA,
        col = rgb(1, 1, 1),
        # xlab = 'Time after the sunset (min)',
        xaxt = 'n',
        # ylab = 'Mean proportion of Masacarene Petrel vocalization (%)',
        # cex.lab = 1.5,
        cex.axis = 1.5)
grid(NA, 4)
b <- barplot(voc$rate,
             names.arg = voc$month2,
             # ylim = c(0, 30),
             angle = 45,
             density = NULL,
             border = NA,
             col = rgb(0, 0, 0, 0.35),
             add = T,
             yaxt = 'n',
             xaxt = 'n')
arrows(b[,1],
       voc$rate,
       b[,1],
       voc$rate + voc$sd,
       length = 0.05,
       angle = 90,
       code = 2)
axis(side = 1,
     at = b[,1],
     tick = F,
     labels = as.character(voc$month2),
     cex.lab = 1.8)

# dev.off()