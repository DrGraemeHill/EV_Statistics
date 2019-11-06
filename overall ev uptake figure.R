#figure 1, EV uptake over the entire country

#data is taken from
#veh0150 "Vehicles registered for the first time by body type"
#
#december 2018 EV registrations from SMMT (which gives data for all 2018)
#https://www.smmt.co.uk/2019/01/december-ev-registrations-2/
#
#october 2019 EV registrations taken from
#https://www.smmt.co.uk/vehicle-data/car-registrations/
#but this is likely a live page and will probabably change


library(ggplot2)
library(dplyr)
library(tidyr)

#Veh-150 data


cars<-data.frame(all_cars=c(1996.3,1907.4,2010.8,2225.1,2438.3,2602.1,2665.3,2509.3,2341.5),
                 year=2010:2018,
                 ulev=c(0.3,1.2,2.5,3.7,14.4,28.0,39.6,50.7,59.6))

ulev_up_to_october<-28259+25892
allcars_up_to_october<-2005522

ulev_up_to_october_2018<-12555+35317
allcars_up_to_october_2018<-2064419

ulev_2018<-44437+15474
allcars_2018<-2367147

#in 2018 up to october data represented:
#87% of all cars sold
car_scale_2019<-allcars_up_to_october_2018/allcars_2018
#and 80% of all ULEVs sold
ulev_scale_2019<-ulev_up_to_october_2018/ulev_2018

#these factors will be used to scale the 2019 up to october data into predicted 2019 data
ulev_2019<-ulev_up_to_october/ulev_scale_2019
allcars_2019<-allcars_up_to_october/car_scale_2019

cars<-rbind(cars,c(allcars_2019/1000,2019,ulev_2019/1000))

cars<-cars%>%mutate(all_cars=round(all_cars,1),ulev=round(ulev,1))%>%
  mutate(market_share=round(100*ulev/all_cars,1))



p<-cars%>%pivot_longer(cols=c(all_cars,ulev,market_share))%>%
  filter(name!="market_share")%>%
  ggplot(aes(x=year,y=value,group=name))+geom_point()+geom_line()








