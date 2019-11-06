#diffusion curve


library(dplyr)
library(ggplot2)

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

#fit an exponential model

d<-data.frame(x=c(cars$year,2040)-2009,y=c(cars$market_share,100))
start <- list(alpha = 1, beta = 0.1, theta = 0)
model <- nls(y ~ alpha * exp(beta * x) + theta , data = d,start=start)

d_predict<-data.frame(x=c(2010:2040)-2009)

p<-predict(model,newdata=d_predict)

ev_uptake<-data.frame(Uptake=cars$market_share,Year=cars$year,type="Previous Sales",stringsAsFactors = F)
ev_uptake<-rbind(ev_uptake,data.frame(Uptake=100,Year=2040,type="Goal"))
ev_uptake<-rbind(ev_uptake,data.frame(Uptake=p,Year=2010:2040,type="Needed Sales"))

predicted_sales<-ev_uptake%>%filter(type=="Needed Sales")

p<-ev_uptake%>%filter(type!="Needed Sales")%>%
  ggplot(aes(x=Year,y=Uptake,group=type,colour=type))+
  geom_line(data=predicted_sales,aes(x=Year,y=Uptake,lty="Predicted"),col="black",size=1)+
  geom_point(size=2)+
  labs(y="Market Share (%)")+
  theme (axis.title = element_text(size=12,face="bold"),
         legend.title = element_blank(),
         legend.text = element_text(size=12,face="bold"))

plot(p)
  






