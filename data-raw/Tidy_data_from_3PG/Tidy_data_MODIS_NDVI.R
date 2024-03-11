#############################################
#Tidy the MODIS VIs (provided by Achilleas)
#############################################
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(cowplot)

#----------------------------
#(0)load the data and test code
#----------------------------
data.path<-"D:/data/Upscale_project_data/From_MODIS/MODIS_NDVI_and_NDWI/"
f_NDVI_test <- raster::raster(paste0(data.path,'/MODIS_NDVI_MEAN_2000_2023_Forest_Only.tif'))

#
raster::plot(f_NDVI_test)

raster::as.data.frame(f_NDVI_test, xy = TRUE) %>%

  dplyr::filter(!is.na(MODIS_NDVI_MEAN_2000_2023_Forest_Only)) %>%

  ggplot(aes(MODIS_NDVI_MEAN_2000_2023_Forest_Only))+

  geom_histogram()

##################################
#processing the results from NDVI
##################################
#----------------------------
#(1)simply present the results
#----------------------------
NDVI_mean<-raster::raster(paste0(data.path,
                  '/MODIS_NDVI_MEAN_2000_2023_Forest_Only.tif'))
NDVI_SD<-raster::raster(paste0(data.path,
                                    '/MODIS_NDVI_SD_2000_2023_Forest_Only.tif'))
NDWI_mean<-raster::raster(paste0(data.path,
                                 '/MODIS_NDWI_MEAN_2000_2023_Forest_Only.tif'))
NDWI_SD<-raster::raster(paste0(data.path,
                               '/MODIS_NDWI_SD_2000_2023_Forest_Only.tif'))
NDVI_yearly_mean<-raster::raster(paste0(data.path,
                                 '/MODIS_Yearly_NDVI_MEAN_2000_2023_Forest_Only.tif'))
NDWI_yearly_mean<-raster::raster(paste0(data.path,
                                        '/MODIS_Yearly_NDWI_MEAN_2000_2023_Forest_Only.tif'))
#
df.NDVI_mean<-raster::as.data.frame(NDVI_mean,xy=TRUE)%>%
  mutate(NDVI_mean=MODIS_NDVI_MEAN_2000_2023_Forest_Only/10000,
         MODIS_NDVI_MEAN_2000_2023_Forest_Only=NULL)
df.NDVI_sd<-raster::as.data.frame(NDVI_SD,xy=TRUE)%>%
  mutate(NDVI_sd=MODIS_NDVI_SD_2000_2023_Forest_Only/10000,
         MODIS_NDVI_SD_2000_2023_Forest_Only=NULL)
df.NDWI_mean<-raster::as.data.frame(NDWI_mean,xy=TRUE)%>%
  mutate(NDWI_mean=MODIS_NDWI_MEAN_2000_2023_Forest_Only/10000,
         MODIS_NDWI_MEAN_2000_2023_Forest_Only=NULL)
df.NDWI_sd<-raster::as.data.frame(NDWI_SD,xy=TRUE)%>%
  mutate(NDWI_sd=MODIS_NDWI_SD_2000_2023_Forest_Only/10000,
         MODIS_NDWI_SD_2000_2023_Forest_Only=NULL)
#additional:
df.NDVI_yearly_mean<-raster::as.data.frame(NDVI_yearly_mean,xy=TRUE)%>%
  mutate(NDVI_mean=MODIS_Yearly_NDVI_MEAN_2000_2023_Forest_Only/10000,
         MODIS_Yearly_NDVI_MEAN_2000_2023_Forest_Only=NULL)
df.NDWI_yearly_mean<-raster::as.data.frame(NDWI_yearly_mean,xy=TRUE)%>%
  mutate(NDWI_mean=MODIS_Yearly_NDWI_MEAN_2000_2023_Forest_Only/10000,
         MODIS_Yearly_NDWI_MEAN_2000_2023_Forest_Only=NULL)

#1)NDVI mean
t_NDVI_mean<-raster::rasterFromXYZ(df.NDVI_mean[,c("x","y","NDVI_mean")])
p_plot_NDVI_mean<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDVI_mean, xy = TRUE)%>%
               filter(!is.na(NDVI_mean)),
             aes(x=x,y=y,col=NDVI_mean),shape=20,size=1)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Mean of MODIS NDVI between 2000-2023")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
#2)NDVI sd
t_NDVI_sd<-raster::rasterFromXYZ(df.NDVI_sd[,c("x","y","NDVI_sd")])
p_plot_NDVI_sd<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDVI_sd, xy = TRUE)%>%
               filter(!is.na(NDVI_sd)),
             aes(x=x,y=y,col=NDVI_sd),shape=20,size=1)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("SD of MODIS NDVI between 2000-2023")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )

#3)NDWI mean
t_NDWI_mean<-raster::rasterFromXYZ(df.NDWI_mean[,c("x","y","NDWI_mean")])
p_plot_NDWI_mean<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDWI_mean, xy = TRUE)%>%
               filter(!is.na(NDWI_mean)),
             aes(x=x,y=y,col=NDWI_mean),shape=20)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Mean of MODIS NDWI between 2000-2023")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
#4)NDWI sd
t_NDWI_sd<-raster::rasterFromXYZ(df.NDWI_sd[,c("x","y","NDWI_sd")])
p_plot_NDWI_sd<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDWI_sd, xy = TRUE)%>%
               filter(!is.na(NDWI_sd)),
             aes(x=x,y=y,col=NDWI_sd),shape=20)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("SD of MODIS NDWI between 2000-2023")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
##additional:
t_NDVI_yearly_mean<-raster::rasterFromXYZ(df.NDVI_yearly_mean[,c("x","y","NDVI_mean")])
p_plot_NDVI_yearly_mean<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDVI_yearly_mean, xy = TRUE)%>%
               filter(!is.na(NDVI_mean)),
             aes(x=x,y=y,col=NDVI_mean),shape=20,size=1)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Yearly Mean of MODIS NDVI between 2000-2023 (Forests)")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )

t_NDWI_yearly_mean<-raster::rasterFromXYZ(df.NDWI_yearly_mean[,c("x","y","NDWI_mean")])
p_plot_NDWI_yearly_mean<-ggplot() +
  geom_point(data=raster::as.data.frame(t_NDWI_yearly_mean, xy = TRUE)%>%
               filter(!is.na(NDWI_mean)),
             aes(x=x,y=y,col=NDWI_mean),shape=20,size=1)+
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  # scale_fill_viridis_d(direction = 1,option = "C") +
  scale_color_viridis_c(option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Yearly Mean of MODIS NDWI between 2000-2023 (Forests)")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm")
  )
#
plot_agg<-plot_grid(p_plot_NDVI_mean,p_plot_NDWI_mean,
                    p_plot_NDVI_sd,p_plot_NDWI_sd,align = "hv",
                    nrow=2)
#
save.path<-"./manuscript/MODIS_results/"
ggsave(file=paste0(save.path,"MODIS_NDVI_NDWI_2000_2023.png"),
       plot_agg,height = 7,width = 13)
