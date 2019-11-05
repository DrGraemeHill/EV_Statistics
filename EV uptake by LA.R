#looking at EV uptake by LA

library(dplyr)
library(ggplot2)


#load the data
pay_by_la<-read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/Gross Annual Pay by LA.txt", stringsAsFactors=FALSE)
uptake <- read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/ULEV uptake by LA.txt", stringsAsFactors=FALSE)
pop <- read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/population by LA.txt", stringsAsFactors=FALSE)

pop<-pop%>%select(code,name,age,pop)%>%
  filter(age == "All ages")



#filter both to just la
pay_by_la<-pay_by_la%>%filter(substr(code,1,2)=="E0")%>%mutate(mean=as.numeric(mean),
                                                               median=as.numeric(median),
                                                               perc_75=as.numeric(perc_75))

uptake<-uptake%>%filter(substr(code,1,2)=="E0")%>%
  mutate(uptake=as.numeric(X2019.Q2))%>%
  select(code,name,uptake)


uptake<-uptake%>%left_join(pay_by_la,by="code")%>%
  left_join(pop,by="code")



