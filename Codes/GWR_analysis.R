#Read important libraries
library(tidyverse)  # Modern data science workflow
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(spData)
library(tmap)

# Change the presentation of decimal numbers to 4 and avoid scientific notation
options(prompt="> ", digits=4, scipen=999)

setwd("insert your pathway here")

#US map data from library spData

data("us_states")
plot(us_states)
class(us_states)
states_sp <- sf::as_Spatial(us_states)
States <- sf::as_Spatial(states_sf)
sf::st_crs(us_states)
sf::st_crs(states_sf)
sf::st_crs(States_sp)

#Transform to NAD83

States_sp<- spTransform(States, CRS("+init=epsg:26978"))
plot(States_sp)


#Convert data to spatial data before applying GWR


#Merge my data
NHW_Final_1<-NHW_Final%>%dplyr::select(State ,`NHW % known Cases`,`NHW % known Death`,log_NHW_Case,log_NHW_Death,Obesity,Diabetes,Poverty)%>%tidyr::drop_na()
St_NHW<-merge(States_sp, NHW_Final_1, by.x="state_name", by.y="State")
St_NHW2 <- sf::st_as_sf(St_NHW)
#remove na in column log case
St_NHW <- St_NHW[!is.na(St_NHW@data$log_NHW_Case),]
St_NHW2<- St_NHW2[!is.na(St_NHW2$log_NHW_Case),]



model <- lm(log_NHW_Case ~ Obesity + Diabetes+Poverty, data = St_NHW)
summary(model)
#7.1 Model diagnostics
par(mfrow=c(2,2))
plot(model)


#8 Map residuals to see if there is any spatial relationship of residuals

resids <- residuals(model)

map.resids <- cbind(St_NHW, resids)
head(map.resids)
names(map.resids)[11] <- "resids"
names(map.resids)[6] <- "resids"
head(map.resids)

map.resids2 <- cbind(St_NHW2, resids)
glimpse(map.resids2)
par(mfrow=c(1,1))
qtm(map.resids, fill = "resids")
qtm(map.resids2, fill = "resids")
#If there is a geographic pattern in the residuals, 
#it is possible that an unobserved variable may be influencing the dependent variable

#getting bandwith for GWR regression
GWRbandwidth <- gwr.sel(log_NHW_Case ~ Obesity + Diabetes+Poverty, data = St_NHW, adapt = T)
GWRbandwidth2 <- gwr.sel(log_NHW_Case ~ Obesity + Diabetes+Poverty, data = St_NHW2, adapt = T)
# Run the GWR model

gwr.model = gwr(log_NHW_Death ~ Obesity + Diabetes+Poverty, data = St_NHW,
                adapt=GWRbandwidth,
                hatmatrix=TRUE,
                se.fit=TRUE) 
gwr.model
#9.3 Create results dataframe
results <-as.data.frame(gwr.model$SDF)
names(results)
#Map
#bind

gwr.map <- cbind(St_NHW, as.matrix(results))
gwr.map2 <- st_as_sf(gwr.map)

#ploting local R2 for all eithnic groups models on map
qtm(gwr.map, fill = "localR2", text = "state_abbv",borders = "black")

tmap_mode('plot')
tm_shape(gwr.map)+
  tm_polygons("localR2", palette = "YlOrBr", border.col  ="black",lwd=0.3, breaks = c(0.80,0.82, 0.84,0.86,0.88,0.90 ,0.92))+
  tm_text("state_abbv")+
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.8,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

#plotting coefficients of individual variables
#Spatial distribution of White_Obesity
map1 <- tm_shape(gwr.map2) + 
  tm_fill("Obesity",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
map1

#Coefficients of Obesity
map2 <- tm_shape(gwr.map2) +
  tm_fill("Obesity.1",
          n = 5,
          style = "quantile",
          title = "BMI Coefficient") +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)

map2
#Spatial distribution of Diabetes

map3 <- tm_shape(gwr.map) +
  tm_fill("Diabetes",
          n = 5,
          style = "quantile") +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
map3
#Spatial distribution of Unemployed


#Coefficients of Diabetes
map4 <- tm_shape(gwr.map) +
  tm_fill("Diabetes.1",
          n = 5,
          style = "quantile",
          title = "Diab Coefficient") +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
map4 



#Spatial distribution of poverty

map5 <- tm_shape(gwr.map) +
  tm_fill("Poverty",
          n = 5,
          style = "quantile") +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
map5
#Coefficients of Poverty
map6 <- tm_shape(gwr.map) +
  tm_fill("Poverty.1",
          n = 5,
          style = "quantile",
          title = "Poverty Coefficient") +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6)
map6 
#10 Use Grid Extra

# creates a clear grid
grid.newpage()

# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(3,2)))

# prints a map object into a defined cell   
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =3))

#same code was used for all ethnic groups.
