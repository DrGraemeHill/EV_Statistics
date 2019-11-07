#looking at EV uptake by LA


library(ggplot2)
library(rgeos)
library(rgdal)
library(raster)
library(leaflet)
library(dplyr)
library(tidyr)

#set up the coordinate system
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

#load the LA

#zones<-readOGR(dsn="P:\\WCTM\\4.Demand\\Zoning System\\Full UK Zones\\LAD\\Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")
zones<-readOGR(dsn="C:\\Users\\HILLG2\\Documents\\WCTM\\4.Demand\\Zoning System\\Full UK Zones\\LAD\\Local_Authority_Districts_December_2017_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")



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
  left_join(pop,by="code")%>%mutate(ev_ratio=uptake/pop)


zones@data<-zones@data%>%left_join(uptake,by=c("lad17cd"="code"))


pal<-colorNumeric("viridis",domain=c(-0.5,2))

s<-which(!is.na(zones@data$ev_ratio))
l<-leaflet(zones[s,]) %>% #setView(lng = -3, lat = 54.6, zoom = 9) %>%
  addTiles()%>%
  addPolygons(layerId=~ zones@data$code,weight=0.5,fillOpacity = 0.9,fillColor = ~pal(log10(zones@data$ev_ratio[s])))%>%
  addLegend(colors=pal(seq(-0.5,2,by=0.25)),
            opacity = 1,title=c("New EV Registrations </br> per 1000 People"),
            labels=c("","",1,"","","",10,"","","",100))


#load the ncpr
ncpr<-read.csv("national-charge-point-registry.csv",header=T,
               stringsAsFactors = F)

#reduce it to lat,long and total power per charger
ncpr<-ncpr%>%mutate(total_power=rowSums(select(.,ends_with("RatedOutputKW")),na.rm = T))%>%
  select(latitude,longitude,total_power)%>%filter(!is.na(latitude) & !is.na(longitude) & latitude>0)

#create the spatial points dataframe in the correct projection
charge_points<-SpatialPointsDataFrame(cbind(ncpr$longitude,ncpr$latitude),data=ncpr,
                                      proj4string = CRS(latlong))

zones<-spTransform(zones,CRS(latlong))

#put the index of each zone the charge point is in
ncpr$zone_index<-over(as(charge_points,"SpatialPoints"),as(zones,"SpatialPolygons"))

ncpr<-ncpr%>%group_by(zone_index)%>%summarise(installed_capacity=sum(total_power,na.rm=T))%>%
  filter(!is.na(zone_index))

zones$installed_capacity<-0.1
zones$installed_capacity[ncpr$zone_index]<-ncpr$installed_capacity

#z<-spTransform(zones,CRS(ukgrid))
#z<-gArea(z,byid=T)/1e6
zones$area<-gArea(spTransform(zones,CRS(ukgrid)),byid=T)/1e6
#use watts per km2
zones@data$capdensity<-1000*zones@data$installed_capacity/zones@data$area
pal<-colorNumeric("viridis",domain=c(-2,5))

#s<-which(!is.na(zones@data$ev_ratio))
l<-leaflet(zones) %>% #setView(lng = -3, lat = 54.6, zoom = 9) %>%
  addTiles()%>%
  addPolygons(layerId=~ zones@data$code,weight=0.5,fillOpacity = 0.9,fillColor = ~pal(log10(zones@data$capdensity)))%>%
  addLegend(colors=pal(seq(-2,5)),
            opacity = 1,title=c("Installed Capacity </br> W/km^2"),
            labels=c(0.01,0.1,1,10,100,1000,10000,100000))
d<-zones@data%>%select(Median=median,ev_ratio,capdensity)

p<-d%>%
  ggplot(aes(x=Median,y=ev_ratio))+
  geom_point()+
  scale_y_continuous(limits=c(0,10))+
  labs(y="EV Uptake per 1000 People",
       x="Median Annual Income")+
  theme(axis.title = element_text(size=14,face="bold"))+
  theme(axis.text=element_text(size=12))


plot(p)


