# pnb <- read.table('C:/Users/Etudiant/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_adult_biometrics.txt', sep = '\t', dec = ',', h = T)

pnb <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_adult_biometricsgrounded_vs_colonies.txt',
                  sep = '\t',
                  dec = ',',
                  h = T)
pnb

library(tidyverse)
library(ggpubr)
library(rstatix)
library(lubridate)


pnb$date <- strptime(as.character(pnb$date), '%d/%m/%Y')

Sys.setlocale('LC_ALL', 'English')
pnb$month <- month(pnb$date, label = T, abbr = F)
pnb$year <- year(pnb$date)

for (i in c('TA', 'AP', 'LC', 'HC', 'CR', 'weight')){
print(pnb %>% group_by(origin) %>% get_summary_stats(i, type = 'common'))}

for (i in c('TA', 'AP', 'LC', 'HC', 'CR', 'weight')){
  print(ggboxplot(pnb, x = 'origin',
            y = i))}


# Data distribution
  # Weight
hist(pnb$weight, breaks = 20)
shapiro.test(pnb$weight) # normality test --> OK

hist(pnb$weight[pnb$origin == 'colony'], breaks = 20)
shapiro.test(pnb$weight[pnb$origin == 'colony']) # normality test --> NOT OK

hist(pnb$weight[pnb$origin == 'grounded'], breaks = 20)
shapiro.test(pnb$weight[pnb$origin == 'grounded']) # normality test --> NOT OK

t.test(pnb$weight)
kruskal.test(pnb$weight ~ as.factor(pnb$origin))

  # AP

hist(pnb$AP, breaks = 20)
shapiro.test(pnb$AP) # normality test --> NOT OK

hist(pnb$AP[pnb$origin == 'colony'], breaks = 20)
shapiro.test(pnb$AP[pnb$origin == 'colony']) # normality test --> OK

hist(pnb$AP[pnb$origin == 'grounded'], breaks = 20)
shapiro.test(pnb$AP[pnb$origin == 'grounded']) # normality test --> OK

t.test(pnb$AP)
kruskal.test(pnb$AP ~ as.factor(pnb$origin))

  # TA

hist(pnb$TA, breaks = 20)
shapiro.test(pnb$TA) # normality test --> NOT OK

hist(pnb$TA[pnb$origin == 'colony'], breaks = 20)
hist(pnb$TA[pnb$origin == 'colony' & pnb$TA > 31])

boxplot(pnb$TA[pnb$origin == 'colony'])
boxplot(pnb$TA[pnb$origin == 'colony' & pnb$TA > 31])

shapiro.test(pnb$TA[pnb$origin == 'colony']) # normality test --> NOT OK --> OUTLIER ?
shapiro.test(pnb$TA[pnb$origin == 'colony' & pnb$TA > 31]) # normality test --> OK without ONE OUTLIER

hist(pnb$TA[pnb$origin == 'grounded'], breaks = 20)
shapiro.test(pnb$TA[pnb$origin == 'grounded']) # normality test --> OK

t.test(pnb$TA)
kruskal.test(pnb$TA ~ as.factor(pnb$origin))

# Growth curve per group
# Gompertz equation
# x = -20 * ln(320/y - 1) + 60

y <- seq(0, 350, 1)
x <- -20 * log((320/y) - 1) + 60

plot(x,
     y,
     type = 'l',
     bty = 'n',
     ylim = c(0, 350))

points(rep(jitter(150), length(pnb$AP[pnb$origin == 'colony'])),
       pnb$AP[pnb$origin == 'colony'],
       col = 'darkgreen')
points(175,
       mean(pnb$AP[pnb$origin == 'colony'], na.rm = T),
       col = 'darkgreen')
points(rep(jitter(150), length(pnb$AP[pnb$origin == 'grounded'])),
       pnb$AP[pnb$origin == 'grounded'],
       col = 'darkred')
points(175,
       mean(pnb$AP[pnb$origin == 'grounded'], na.rm = T),
       col = 'darkred')
#######################
# 
# krus <- kruskal.test(pnb$LC ~ as.factor(pnb$origin))
# krus
# 
# krus <- kruskal.test(pnb$HC ~ as.factor(pnb$origin))
# krus
# 
# krus <- kruskal.test(pnb$CR ~ as.factor(pnb$origin))
# krus
# 
