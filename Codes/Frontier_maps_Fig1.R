This script was used to create the maps of the paper
#Libraries needed


library(ggplot2)
library(tidyverse)
library(fiftystater)
library(tmap)
library(ggplot2)
library(maps)
library(tidyverse)  # Modern data science workflow
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)


Read the final data which is already updloaded on the repository


NHW_Final <- read_csv("~20210706_NHW_Final.csv")
NHB_Final <- read_csv("~20210706_NHB_Final.csv")
Hispanic_Final <- read_csv("20210706_Hispanic_Final.csv")
Hispanic_Final <- read_csv("20210706_Hispanic_Final.csv")
fifty_states<-read_csv("fifty_states.csv") # this can be extracted from fifty states R library 


Lets create centeriod of the states to show state observations


#Centroid
centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
c<-data.frame("district of columbia",-77.02,38.94, "DC")
colnames(c)<-c("region","long","lat", "abb")

centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]
centroids<-rbind(centroids,c)



The following code will create a US map for infection rates of NHW population
Data for NHW maps


x<-NHW_Final
x$id <- tolower(x$State)
map.df <- merge(fifty_states,x, by="id", all.x=TRUE)
map.df <- map.df[order(map.df$order),]

x<-merge(x,statelatlong,by.x="State",by.y="City",all.x=TRUE)
x%>%glimpse()
summary(x$`NHW % known Cases`)
summary(x$`NHW % known Death`)







ggplot(map.df, aes(x=long,y=lat,group=group)) + theme_bw()+
  geom_polygon(aes(fill=`NHW % known Cases`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Infection rate",colours=rev(heat.colors(2)),limits = c(0,85),breaks=c(0,20,40,60,80),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )

Similary for NHW mortality rates, the map can be obtained using




ggplot(map.df, aes(x=long,y=lat,group=group))+theme_bw()+
  geom_polygon(aes(fill=`NHW % known Death`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Mortality rate",colours=rev(heat.colors(2)),limits = c(10,98),breaks=c(0,25,50,75,95),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )



NHB data for  maps of infection and mortality
Lets create the data for maps


x<-NHB_Final
x$id <- tolower(x$State)
map.df <- merge(fifty_states,x, by="id", all.x=TRUE)
map.df <- map.df[order(map.df$order),]
statelatlong%>%glimpse()
x<-merge(x,statelatlong,by.x="State",by.y="City",all.x=TRUE)
x%>%glimpse()
summary(x$`NHB % known Cases`)
summary(x$`NHB % known Death`)



NHB Infection rate




ggplot(map.df, aes(x=long,y=lat,group=group))+ theme_bw()+
  geom_polygon(aes(fill=`NHB % known Cases`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Infection rate",colours=rev(heat.colors(2)),limits = c(0,85),breaks=c(0,20,40,60,80),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )




NHB mortality rate




ggplot(map.df, aes(x=long,y=lat,group=group))+ theme_bw()+
  geom_polygon(aes(fill=`NHB % known Death`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Mortality rate",colours=rev(heat.colors(2)),limits = c(0,95),breaks=c(0,25,50,75,95),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )


For Hispanic maps of infection and mortality
Lets create the data for maps


x<-Hispanic_Final
x$id <- tolower(x$State)
map.df <- merge(fifty_states,x, by="id", all.x=TRUE)
map.df <- map.df[order(map.df$order),]
statelatlong%>%glimpse()
x<-merge(x,statelatlong,by.x="State",by.y="City",all.x=TRUE)
x%>%glimpse()
summary(x$`Hispanic % known Cases`)
summary(x$`Hispanic % known Death`)

Hispanic Infection rate




ggplot(map.df, aes(x=long,y=lat,group=group))+ theme_bw()+
  geom_polygon(aes(fill=`Hispanic % known Cases`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Infection rate",colours=rev(heat.colors(2)),limits = c(0,85),breaks=c(0,20,40,60,80),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )



Hispanic mortality rate




ggplot(map.df, aes(x=long,y=lat,group=group))+ theme_bw()+
  geom_polygon(aes(fill=`Hispanic % known Death`))+
  geom_path(size = 0.1)+ 
  scale_fill_gradientn(name="Mortality rate",colours=rev(heat.colors(2)),limits = c(0,95),breaks=c(0,25,50,75,95),na.value="grey90")+
  coord_map()+
  geom_text(data=centroids, aes(x=long,y=lat, group=NA, label=abb), 
            size=2.8, vjust=0.8, hjust=0.8)+xlab("Longitude")+ylab("Latitude")+theme(
              # LAIDES APPEARANCE
              plot.title = element_text(size=14, face= "bold", colour= "black" ),
              axis.title.x = element_text(size=14, face="bold", colour = "black"),    
              axis.title.y = element_text(size=14, face="bold", colour = "black"),    
              axis.text.x = element_text(size=12, face="bold", colour = "black"), 
              # axis.text.y = element_text(size=12,  colour = "black"), # unbold
              axis.text.y = element_text(size=12, face="bold", colour = "black"), # bold
            )


Finally combine them all to get figure#1