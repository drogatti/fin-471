install.packages("tseries")
library(tseries)

#set a working directory
setwd("C:/Users/aless/Documents/471/Data")

#read in data
spread <- read.csv("crack.csv", skip=5)

#rename columns to LH, CN, and SM respectively
names(spread)[names(spread) == "PX_LAST"] <- "LH"
names(spread)[names(spread) == "PX_LAST.1"] <- "CN"
names(spread)[names(spread) == "PX_LAST.2"] <- "SM"

#show df and labels
names(spread)
print(spread)

#create new variable that is diff between LH and SM
spread$DF <- spread$LH - spread$SM

#summary statistics of DF column
summary(spread$DF)

#ADF unit root test on LH column
adf.test(spread$LH)

#plot the series LH
plot(spread$LH)