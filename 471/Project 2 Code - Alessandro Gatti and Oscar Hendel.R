install.packages("urca")
library(urca)

#set a working directory
setwd("C:/Users/aless/Documents/471/Data")

#read in data
spread <- read.csv("crack.csv", skip=5)

#rename columns to LH, CN, and SM respectively
names(spread)[names(spread) == "PX_LAST"] <- "LH"
names(spread)[names(spread) == "PX_LAST.1"] <- "CN"
names(spread)[names(spread) == "PX_LAST.2"] <- "SM"

#compute summary statistics for "LH", "CN", and "SM"
summary(spread$LH)
summary(spread$CN)
summary(spread$SM)

#draw respective histograms
hist(spread$LH, main="Histogram of LH Series", xlab="LH", ylab="Frequency")
hist(spread$CN, main="Histogram of CN Series", xlab="CN", ylab="Frequency")
hist(spread$SM, main="Histogram of SM Series", xlab="SM", ylab="Frequency")

#regress "LH" on "CN"
LH_on_CN <- lm(LH ~ CN, data=spread)
summary(LH_on_CN)

#regress "LH" on "SM"
LH_on_SM <- lm(LH ~ SM, data=spread)
summary(LH_on_SM)

#regress "LH" on both "CN" and "SM"
LH_on_CN_SM <- lm(LH ~ CN + SM, data=spread)
summary(LH_on_CN_SM)

#form first difference series for "LH", "CN", and "SM"
spread$LH_firstdiff <- c(NA, diff(spread$LH))
spread$CN_firstdiff <- c(NA, diff(spread$CN))
spread$SM_firstdiff <- c(NA, diff(spread$SM))

#conduct phillips-perron unit root test on all series
##LH
pp_LH <- ur.pp(spread$LH)
summary(pp_LH)
pp_LH_diff <- ur.pp(spread$LH_firstdiff)
summary(pp_LH_diff)

##CN
pp_CN <- ur.pp(spread$CN)
summary(pp_CN)
pp_CN_diff <- ur.pp(spread$CN_firstdiff)
summary(pp_CN_diff)

##SM
pp_SM <- ur.pp(spread$SM)
summary(pp_SM)
pp_SM_diff <- ur.pp(spread$SM_firstdiff)
summary(pp_SM_diff)

#conduct the Johansen Test
sub_spread <- spread[, c("LH", "CN", "SM")]
ss_matrix <- as.matrix(sub_spread)
johansen_test <- ca.jo(ss_matrix, type="trace", ecdet="const", K=2)
summary(johansen_test)
