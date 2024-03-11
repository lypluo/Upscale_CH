#############################################
#Tidy the Available water capacity (provided by Katrin)
#############################################
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(cowplot)

#----------------------------
#(0)load the data and test code
#----------------------------
data.path<-"D:/data/Upscale_project_data/From_AWC_model/"
f_AWC_test <- raster::raster(paste0(data.path,'/awc_mm_1m_25m.tif'))
raster::plot(f_AWC_test)

##################################
#processing the results from NDVI
##################################
#----------------------------
#(1)simply present the results
#----------------------------
AWC_1_25<-raster::raster(paste0(data.path,
                                 '/awc_mm_1m_25m.tif'))
AWC_2_25<-raster::raster(paste0(data.path,
                               '/awc_mm_2m_25m.tif'))
AWC_mrd_25<-raster::raster(paste0(data.path,
                                 'awc_mm_mrd_25m.tif'))

#
df.AWC_1_25<-raster::as.data.frame(AWC_1_25,xy=TRUE)%>%
  mutate(AWC_1_25=awc_mm_1m_25m,
         awc_mm_1m_25m=NULL)
df.AWC_2_25<-raster::as.data.frame(AWC_2_25,xy=TRUE)%>%
  mutate(AWC_2_25=awc_mm_2m_25m,
         awc_mm_2m_25m=NULL)
df.AWC_mrd_25<-raster::as.data.frame(AWC_mrd_25,xy=TRUE)%>%
  mutate(AWC_mrd_25=awc_mm_mrd_25m,
         awc_mm_mrd_25m=NULL)
#1)AWC mean
t_AWC_1_25<-raster::rasterFromXYZ(df.AWC_1_25[,c("x","y","AWC_1_25")])
p_plot_AWC_1_25<-ggplot() +
  geom_point(data=raster::as.data.frame(t_AWC_1_25, xy = TRUE)%>%
               filter(!is.na(AWC_1_25)),
             aes(x=x,y=y,col=AWC_1_25),shape=20,size=1)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("soil available water capacity (AWC) between 1-25m ")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
