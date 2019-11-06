#looking at EV uptake by LA


library(ggplot2)
library(rgeos)
library(rgdal)
library(raster)
library(leaflet)
library(dplyr)
#load the LA

zones<-readOGR(dsn="P:\\WCTM\\4.Demand\\Zoning System\\Full UK Zones\\LAD\\Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")




#load the data
pay_by_la<-read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/Gross Annual Pay by LA.txt", stringsAsFactors=FALSE)
uptake <- read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/ULEV uptake by LA.txt", stringsAsFactors=FALSE)
pop <- read.delim("C:/Users/HILLG2/OneDrive - Jacobs/EV Slides for Local Authority/EV_Statistics/population by LA.txt", stringsAsFactors=FALSE)

pop<-pop%>%dplyr::select(code,name,age,pop)%>%
  filter(age == "All ages")



#filter both to just la
pay_by_la<-pay_by_la%>%filter(substr(code,1,2)=="E0")%>%mutate(mean=as.numeric(mean),
                                                               median=as.numeric(median),
                                                               perc_75=as.numeric(perc_75))

uptake<-uptake%>%filter(substr(code,1,2)=="E0")%>%
  mutate(uptake=as.numeric(X2019.Q2))%>%
  dplyr::select(code,name,uptake)


uptake<-uptake%>%left_join(pay_by_la,by="code")%>%
  left_join(pop,by="code")%>%mutate(ev_ration=uptake/pop)


zones@data<-zones@data%>%left_join(uptake,by=c("lad17cd"="code"))

pal<-colorNumeric("viridis",domain=range(zones@data$ev_ration,na.rm=T),n=10)

plot(zones,col=pal(zones@data$ev_ration),border=pal(zones@data$ev_ration))

s<-which(!is.na(zones@data$ev_ration))
l<-leaflet(zones[s,]) %>% #setView(lng = -3, lat = 54.6, zoom = 9) %>%
  addTiles()%>%
  addPolygons(layerId=~ zones@data$code,weight=0.5,fillOpacity = 0.8,fillColor = ~pal((log(zones@data$ev_ration[s]))))%>%
  addLegend(pal = pal, bins=5,
            values = log(zones@data$ev_ration[s]),
            opacity = 1,title="EVs per Capita",
            labels=c(1,10,100))

