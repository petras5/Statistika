library(readxl)
library(tidyverse)
library(modeldata)
library(ggplot2)

data <- read_excel("Tablica_podatci1.xlsx")

bedepe<-data$vrijednost
log<-data$logBDP
ocekivana<-data$brojdob

##graf sa vracenom supst z=e^x
regr=lm(ocekivana~log)

summary(regr)

pc=predict(regr,interval="confidence")

h<-pc[,1]
hb<-unname(h,force = FALSE)

f<-pc[,2]
fb<-unname(f, force = FALSE)

g<-pc[,3]
gb<-unname(g, force = FALSE)


ggplot()+
  geom_point(data,mapping=aes(x=bedepe,y=ocekivana),color="black")+
  geom_line(data,mapping=aes(x=bedepe,y=fb),color="blue",lwd=1)+
  geom_line(data,mapping=aes(x=bedepe,y=gb),color="blue",lwd=1)+
  geom_line(data,mapping=aes(x=bedepe,y=hb),color="darkorange",lwd=1)

r<-regr$residuals
rb<-unname(r, force = FALSE)


naz<-sqrt(hb)

stanrez<-rb/naz

ggplot()+
  geom_point(data,mapping=aes(x=ocekivana,y=rb),color="black")+
  geom_line(data,mapping=aes(x=ocekivana,y=0),color="blue",lwd=1.3)

qplot(sample=stanrez)
ggplot(data, aes(sample=stanrez))+
  geom_qq()+
  geom_qq_line(col="lightsalmon", lwd=1)

