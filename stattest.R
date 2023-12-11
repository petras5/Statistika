library(readxl)
library(tidyverse)
library(ggplot2)
library("dplyr")

data <- read_excel("Tablica_podatci1.xlsx")

if(!require('nortest')) {
  install.packages('nortest')
  library('nortest')
}

##linearna regresija graf s pouzdanim intervalom
log<-data$logBDP
ocekivana<-data$brojdob

ggplot(data, aes(log,ocekivana))+
  geom_point()+
  geom_smooth(method="lm",se=TRUE)

regr<-lm(ocekivana~log, data)

summary(regr)

##qq grafovi
ggplot(data, aes(sample=ocekivana))+
  geom_qq()+
  geom_qq_line(col="lightsalmon", lwd=2)

qplot(sample=log)
ggplot(data, aes(sample=log))+
  geom_qq()+
  geom_qq_line(col="blue", lwd=2)

##lillieforsov test

lillie.test(log)

lillie.test(ocekivana)

r<-regr$residuals
rb<-unname(r, force = FALSE)

lillie.test(rb)

