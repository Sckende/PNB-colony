# pnb <- read.table('C:/Users/Etudiant/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_adult_biometrics.txt', sep = '\t', dec = ',', h = T)

pnb <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/1-PNB_Methodo_colony/DATA/PNB_biometrics_grounded_vs_colonies.txt',
                  sep = '\t',
                  dec = ',',
                  h = T)
pnb
summary(pnb)

table(pnb$age, pnb$origin)
pnb$age[pnb$age == 'ad'] <- 'Ad'


library(tidyverse)
library(ggpubr)
library(rstatix)
library(lubridate)

# Adulte selection for biometrics comparaison
pnb.ad <- pnb[pnb$age == 'Ad',]

# Completion of month and year variables
pnb.ad$date <- strptime(as.character(pnb.ad$date), '%d/%m/%Y')

Sys.setlocale('LC_ALL', 'English')
pnb.ad$month <- month(pnb.ad$date, label = T, abbr = F)
pnb.ad$year <- year(pnb.ad$date)

for (i in c('TA', 'AP', 'LC', 'HC', 'CR', 'weight')){
print(pnb.ad %>% group_by(origin) %>% get_summary_stats(i, type = 'common'))}

for (i in c('TA', 'AP', 'LC', 'HC', 'CR', 'weight')){
  print(ggboxplot(pnb.ad, x = 'origin',
            y = i))}


# Data distribution
  # Weight
hist(pnb.ad$weight, breaks = 20)
shapiro.test(pnb.ad$weight) # normality test --> OK

hist(pnb.ad$weight[pnb.ad$origin == 'colony'], breaks = 20)
shapiro.test(pnb.ad$weight[pnb.ad$origin == 'colony']) # normality test --> NOT OK

hist(pnb.ad$weight[pnb.ad$origin == 'grounded'], breaks = 20)
shapiro.test(pnb.ad$weight[pnb.ad$origin == 'grounded']) # normality test --> NOT OK

kruskal.test(pnb.ad$weight ~ as.factor(pnb.ad$origin)) # Significant difference btw grounded and colonies

  # AP

hist(pnb.ad$AP, breaks = 20)
shapiro.test(pnb.ad$AP) # normality test --> NOT OK

hist(pnb.ad$AP[pnb.ad$origin == 'colony'], breaks = 20)
shapiro.test(pnb.ad$AP[pnb.ad$origin == 'colony']) # normality test --> OK

hist(pnb.ad$AP[pnb.ad$origin == 'grounded'], breaks = 20)
shapiro.test(pnb.ad$AP[pnb.ad$origin == 'grounded']) # normality test --> OK

t.test(pnb.ad$AP ~ pnb.ad$origin) # Significant difference btw grounded and colonies

  # TA

hist(pnb.ad$TA, breaks = 20) # Possibly one outlier
hist(pnb.ad$TA[pnb.ad$origin == 'colony' & pnb.ad$TA > 31], breaks = 20)

boxplot(pnb.ad$TA[pnb.ad$origin == 'colony'])
boxplot(pnb.ad$TA[pnb.ad$origin == 'colony' & pnb.ad$TA > 31])

# shapiro.test(pnb$TA[pnb$origin == 'colony']) # normality test --> NOT OK --> OUTLIER ?
shapiro.test(pnb.ad$TA[pnb.ad$origin == 'colony' & pnb.ad$TA > 31]) # normality test --> OK without ONE OUTLIER

hist(pnb.ad$TA[pnb.ad$origin == 'grounded'], breaks = 20)
shapiro.test(pnb.ad$TA[pnb.ad$origin == 'grounded']) # normality test --> OK

t.test(pnb.ad$TA[pnb.ad$TA > 31]~ as.factor(pnb.ad$origin[pnb.ad$TA > 31])) # Significant difference btw grounded and colonies

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
