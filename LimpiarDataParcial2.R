library(dplyr)
library(highcharter)
library(tidyverse)
library(readr)
library(formattable)
library(lubridate)
library(knitr)
library(stringr)

setwd("~/Documents/8vo_semestre/Data_Product/Parcial_2/Parcial2")

df<-read_delim("c1.csv",",",escape_double=FALSE,trim_ws=TRUE)
df$Fecha<-dmy(df$Fecha)
df$Camion_5<-as.numeric(str_remove(df$Camion_5,pattern = "Q")) 
df$Pickup<-as.numeric(str_remove(df$Pickup,pattern = "Q"))
df$Moto<-as.numeric(str_remove(df$Moto,pattern = "Q")) 
df$factura<-as.numeric(str_remove(df$factura,pattern = "Q")) 
df$directoCamion_5<-as.numeric(str_remove(df$directoCamion_5,pattern = "Q")) 
df$fijoCamion_5<-as.numeric(str_remove(df$fijoCamion_5,pattern = "Q")) 
df$directoPickup<-as.numeric(str_remove(df$directoPickup,pattern = "Q")) 
df$fijoPickup<-as.numeric(str_remove(df$fijoPickup,pattern = "Q")) 
df$directoMoto<-as.numeric(str_remove(df$directoMoto,pattern = "Q")) 
df$fijoMoto<-as.numeric(str_remove(df$fijoMoto,pattern = "Q"))

tidy_df<-df %>% 
  pivot_longer(c(`Camion_5`, `Pickup`,`Moto` ), names_to = "Transporte", values_to = "CostoTotal") %>% 
  pivot_longer(c(`5-30`, `30-45`,`45-75`,`75-120`,`120+`), names_to = "TiempoRecorrido", values_to = "n") %>% 
  pivot_longer(c(`directoCamion_5`, `directoPickup`,`directoMoto` ), names_to = "Directo", values_to="CostoDirecto") %>%
  pivot_longer(c(`fijoCamion_5`, `fijoPickup`,`fijoMoto` ), names_to = "Fijo", values_to="CostoFijo") %>% 
  filter(n>0) %>% 
  filter(CostoTotal>0) %>% 
  filter(CostoDirecto>0) %>% 
  filter(CostoFijo>0)

tidy_df<-tidy_df %>% 
  select(Fecha,ID,Transporte,Cod,CostoTotal,CostoDirecto,CostoFijo,origen,Lat,Long,factura,height,TiempoRecorrido)

tidy_df[which(tidy_df$Transporte=='Camion_5'),'Transporte'] <- 'Camion'

write.csv(tidy_df,file = "c2.csv")

c2 <- read_csv("c2.csv")

ggplot(c2, aes(x = Transporte, y = factura)) + 
  geom_point()


library(lubridate)
c2$mes <- month(c2$Fecha)

c2$factura
c2[,"mes"==1]

which(c2$mes==1)
sum(c2[which(c2$mes==1),]$factura)


c3<-c2[which(c2$mes==1),]
hist(c3$factura)
hist(c2[which(c2$mes==1),]$factura)



