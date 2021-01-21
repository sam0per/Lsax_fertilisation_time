rm(list=ls())

data <- read.csv("/Users/samuelperini/Desktop/Fert_Lsax.csv")
head(data)
str(data)

data$tot <- data$egg + data$dev

#install.packages("aod")
library(aod)

#treatment model
betabin(cbind(dev,egg)~class,~1,data=data[data$tot>0,],link="logit")
AIC(betabin(cbind(dev,egg)~class,~1,data=data[data$tot>0,],link="logit"))

#null model
betabin(cbind(dev,egg)~1,~1,data=data[data$tot>0,],link="logit")
AIC(betabin(cbind(dev,egg)~1,~1,data=data[data$tot>0,],link="logit"))
