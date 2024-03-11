#############################################
#Aim:Tidy the data sent from 3-PG model(provided by Volo)
#############################################
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(cowplot)
# the github version of ggmap, which recently pulled in a small fix I had
# for a bug--from Eric C. Anderson
# devtools::install_github("dkahle/ggmap")
#
library(maps)
library(mapdata)
library(sf)
#----------------------------
#(0)load the data
#----------------------------
#data period:1990-2022
data.path<-"D:/data/Upscale_project_data/From_3PG_model/"
df<-read.csv2(paste0(data.path,"npp_simulation_3PG.csv"),sep = ",")
df$grid_id<-as.integer(df$grid_id)

#
f_NPP_test <- raster::raster(paste0(data.path,'/npp_anomalies/fasy_2003.tif'))
#YP:adding grid_id
f_NPP_test$grid_id<-c(1:ncell(f_NPP_test))
tt<-raster::as.data.frame(f_NPP_test, xy = TRUE)
df_CRS<-projection(f_NPP_test)

#merge two datasets
df_new<-left_join(df,tt[,c("x","y","grid_id")],by="grid_id")


#----------------------------
#(1)starting to plot
#----------------------------
t_data<-raster::rasterFromXYZ(df_new[,c("x","y","npp_mean")],res = c(1000,1000),
                              crs = df_CRS)
p_plot<-ggplot() +
  geom_point(data=raster::as.data.frame(f_NPP_test, xy = TRUE)%>%
               filter(!is.na(npp_mean)),
             aes(x=x,y=y),shape=20,col="black",size=0.6)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  coord_equal() +
  theme_map() +
  ggtitle("NPP")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
