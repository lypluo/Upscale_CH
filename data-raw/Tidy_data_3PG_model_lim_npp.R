#############################################
#Tidy the npp from 3-PG model(provided by Volo)
#############################################
#NPP link:
#limited Env:https://www.envidat.ch/#/metadata/environmental-constraints-on-tree-growth
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(cowplot)
#----------------------------
#(0)load the data and test code
#----------------------------
#code refer to Volo
data.path<-"D:/data/Upscale_project_data/From_3PG_model/"
f_NPP_test <- raster::raster(paste0(data.path,'/npp_anomalies/piab_2003.tif'))

#
raster::plot(f_NPP_test)

raster::as.data.frame(f_NPP_test, xy = TRUE) %>%

  dplyr::filter(!is.na(piab_2003)) %>%

  ggplot(aes(piab_2003))+

  geom_histogram()


##
list.files(paste0(data.path,'npp_anomalies/'), full.names = TRUE)[2:5] %>%

  raster::stack() %>%

  raster::plot()

list.files(paste0(data.path,'npp_anomalies/'), full.names = TRUE)[2:5] %>%

  raster::stack() %>%

  raster::as.data.frame(., xy = TRUE) %>%

  dplyr::filter(!is.na(fasy_1960)) %>%

  head()

##################################
#processing the results from npp anomalies
##################################
df.a_npp<-list.files(paste0(data.path,'npp_anomalies/'), full.names = TRUE)
#only selecting the limiting factors in 1991-2018
df.a_npp_1960_2018<-df.a_npp[grep(".tif",df.a_npp)]
#species:
species<-c("fasy","piab")
species_names<-c("Fagus_sylvatica","Picea_abies")
##for each speices
df.a_npp_1960_2018_agg<-c()
for (i in 1:length(species)) {
  df.temp<-c()
  df.temp<-df.a_npp_1960_2018[grep(species[i],df.a_npp_1960_2018)]
  df.a_npp_1960_2018_agg[[i]]<-df.temp
  rm(df.temp)
}
names(df.a_npp_1960_2018_agg)<-species_names
####
#----------
#-->stack the data from 1960-2018 for each species
#----------
##calculate the Theil–Sen slope and test its significance
library(trend) #calculate the Theil_sen slope
cal_senslope<-function(df){
  #df<-c(1,2,4,5,5,6,7,8,10,11)

  trend_stats<-trend::sens.slope(df)
  #in trend r package, the significance of sens.slope has been
  #defaullty test by Mann–Kendall tests-->namely mk.test below:
  #mk.test(df1$npp_anomaly)
  stats_out<-data.frame("slope"=trend_stats$estimates,
                        "p.value"=trend_stats$p.value)
  return(stats_out)
}

df_tidy<-list()
##calculating the Theil-sen slope and conducting MK.test
for (i in 1:length(df.a_npp_1960_2018_agg)) {
  #
  species_proc_name<-names(df.a_npp_1960_2018_agg)[i]
  species_short<-ifelse(species_proc_name=="Fagus_sylvatica","fasy","piab")
  temp.path<-data.frame("path"=df.a_npp_1960_2018_agg[[i]],
  #refer to https://www.geeksforgeeks.org/extract-numbers-from-character-string-vector-in-r/
  "year"=as.numeric(gsub(".*?([0-9]+).*", "\\1", substr(df.a_npp_1960_2018_agg[[i]],
     str_length(df.a_npp_1960_2018_agg[[i]])-8,str_length(df.a_npp_1960_2018_agg[[i]])))))

  #
  df_species_temp<-c()
  #stack all the years' data
  df.proc<-temp.path[,"path"]%>%
    raster::stack()%>%
    # raster::plot()
    raster::as.data.frame(., xy = TRUE)
  df.proc_final<-df.proc%>%
    rownames_to_column('id') %>%  # creates an ID number
    pivot_longer(starts_with(species_short),
                 names_to = "species_year",values_to = "npp_anomaly")%>%
    mutate(year=as.numeric(substr(species_year,6,9)))
  #calculate the Theil_sen slope and MK test
  df.out<-df.proc_final%>%
    filter(!is.na(npp_anomaly))%>%
    group_by(x,y)%>%
    summarise(slope=cal_senslope(npp_anomaly)[,"slope"],
              p.value=cal_senslope(npp_anomaly)[,"p.value"])
 ##
  df_tidy[[i]]<-df.out
}
names(df_tidy)<-species_names
#save the data:
save.path<-"./data/3PG/"
# save(df_tidy,file = paste0(save.path,"species_npp_anomaly_trend_and_sig.RDA"))

##making the plots###
load(paste0(save.path,"species_npp_anomaly_trend_and_sig.RDA"))
library(cowplot)
#----------
#A. plotting the limiting factor for each species
#----------
simple_plot_map<-function(df,species_proc_name){
  # df<-df_tidy[[1]]
  # species_proc_name<-names(df_tidy)[1]
  #
  df<-df %>%
    mutate(lim_GS_categroy=case_when(lim_var_GS=="tmp"~1,
                                     lim_var_GS=="vpd"~2,
                                     lim_var_GS=="sw"~3))%>%
    mutate(lim_GS_categroy=factor(lim_GS_categroy,levels=c(1,2,3)))
  #
  t_plot1<-raster::rasterFromXYZ(df[,c("x","y","lim_val_mean")])
  t_plot2<-raster::rasterFromXYZ(df[,c("x","y","lim_GS_categroy")])
  # t_plot<-raster::rasterFromXYZ(df[,c("x","y","lim_val_mean","lim_GS_categroy")])

  # par(mfrow=c(2,1))
  # raster::plot(t_plot1,main=species_proc_name)
  # #using the ggplot2
  #-->refer:https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
  p_plot1<-ggplot() +
    geom_tile(data=raster::as.data.frame(t_plot1, xy = TRUE),
              aes(x=x, y=y, fill=lim_val_mean), alpha=0.8) +
    # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
    #              fill=NA, color="grey50", size=0.25) +
    scale_fill_viridis_c(begin = 0,end = 1) +
    coord_equal() +
    theme_map() +
    ggtitle(species_proc_name)+
    theme(legend.position="right",
          legend.key.width=unit(0.8, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = unit(c(0,0,0,0), "cm")
          )

  n=length(unique(df$lim_GS_categroy))
  # par(fig=c(0.55,1,0,1),new=T)
  #
  df %>%
    group_by(lim_var_GS)%>%
    count()

  if(n==2){
    # raster::plot(t_plot2,col=c("orange","green3"),
    #              main=species_proc_name,legend=FALSE)
    # legend("topright", legend = c("tmp", "vpd"),
    #        fill = c("orange", "green3"))
    # #using ggplot2
    p_plot2<-ggplot() +
      geom_tile(data=raster::as.data.frame(t_plot2, xy = TRUE)%>%
                  filter(!is.na(lim_GS_categroy))%>%
                  mutate(lim_GS_var=factor(lim_GS_categroy,levels = c("1","2","3"))),
                aes(x=x, y=y, fill=lim_GS_var), alpha=0.8) +
      # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
      #              fill=NA, color="grey50", size=0.25) +
      scale_fill_manual(values = c("1"="orange","2"="green3","3"="skyblue"),
                        labels=c("tmp","vpd","sw")) +
      coord_equal() +
      theme_map() +
      ggtitle(species_proc_name)+
      theme(legend.position="right",
            legend.key.width=unit(0.8, "cm"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "white", colour = NA),
            plot.margin = unit(c(0,0,0,0), "cm")
      )
  }
  if(n==3){
    # raster::plot(t_plot2,col=c("orange","green3","skyblue"),
    #              main=species_proc_name,legend=FALSE)
    # legend("topright", legend = c("tmp", "vpd","sw"),
    #        fill = c("orange", "green3","skyblue"))
    #using ggplot2
    p_plot2<-ggplot() +
      geom_tile(data=raster::as.data.frame(t_plot2, xy = TRUE)%>%
                  filter(!is.na(lim_GS_categroy))%>%
                  mutate(lim_GS_var=factor(lim_GS_categroy,levels = c("1","2","3"))),
                aes(x=x, y=y, fill=lim_GS_var), alpha=0.8) +
      # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
      #              fill=NA, color="grey50", size=0.25) +
      scale_fill_manual(values = c("1"="orange","2"="green3","3"="skyblue"),
                        labels=c("tmp","vpd","sw")) +
      coord_equal() +
      theme_map() +
      ggtitle(species_proc_name)+
      theme(legend.position="right",
            legend.key.width=unit(0.8, "cm"),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "white", colour = NA),
            plot.margin = unit(c(0,0,0,0), "cm")
      )

  }
p_merge<-plot_grid(p_plot1,p_plot2,nrow=2)
#
return(p_merge)
}
##plotting the results:
plot_all<-c()
for (i in 1:length(df_tidy)) {
  plot_merge<-simple_plot_map(df_tidy[[i]],names(df_tidy)[i])
  plot_all[[i]]<-plot_merge
  rm(plot_merge)
}
##merge all the plots:
save.path<-"./manuscript/3PG_results/"
p_all<-plot_grid(plot_all[[1]],plot_all[[2]],plot_all[[3]],plot_all[[4]],
          plot_all[[5]],plot_all[[6]],plot_all[[7]],
          align = "hv")
ggsave(file=paste0(save.path,"limiting_factor_each_species.png"),
       p_all,height = 15,width = 15)

#------------------------------------------------
#-->aggregating info of limiting factors for all species
#-----------------------------------------------
#logic: for each specie-->keep the pixes with lim_val_mean<0.6,
#>=0.6 lim_val_mean set as 0-->set the kept pixel values to 1
#add summary the 7 speices for each pixel-->the pixel value is higher,stress level is higher
df_tidy_recal<-c()
#------set the pixel values to 0 or 1
for (i in 1:length(df_tidy)) {
  df_temp<-df_tidy[[i]]
  species_proc_name<-names(df_tidy)[i]

  #
  df.proc<-df_temp%>%
    filter(!is.na(lim_val_mean))%>%
    #recalculate the limiting values for each pixel
    mutate(lim_val_recal=ifelse(lim_val_mean>=0.6,0,1))
  df_tidy_recal[[i]]<-df.proc
}
names(df_tidy_recal)<-names(df_tidy)
#--------adding values for different speceis------
plot_map_fun<-function(df){
  # df<-df_tidy_recal[[1]]
  p_plot<-ggplot() +
    geom_tile(data=raster::as.data.frame(df, xy = TRUE)%>%
                filter(!is.na(lim_val_recal))%>%
                mutate(lim_val_recal=factor(lim_val_recal,levels = paste0(0:1))),
              aes(x=x, y=y, fill=lim_val_recal), alpha=0.8) +
    # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
    #              fill=NA, color="grey50", size=0.25) +
    scale_fill_viridis_d()+
    coord_equal() +
    theme_map() +
    ggtitle(names(df_tidy_recal)[i])+
    theme(legend.position="right",
          legend.key.width=unit(0.8, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = unit(c(0,0,0,0), "cm"))
  return(p_plot)
}

####
p_species_agg<-c()
#
df_tidy_agg<-df_tidy_recal[[1]]%>%
  select(x,y,lim_val_recal)%>%
  mutate(V1=lim_val_recal)
p_species_agg[[1]]<-plot_map_fun(df_tidy_recal[[1]])
for (i in 2:length(df_tidy_recal)) {
  p_species_agg[[i]]<-plot_map_fun(df_tidy_recal[[i]])
  raster_add<-df_tidy_recal[[i]]%>%select(x,y,lim_val_recal)
  #
  names(raster_add)<-c("x","y",paste0("V",i))
  df_tidy_agg<-full_join(df_tidy_agg,raster_add,id=c("x","y"))
}
#show the filter map(value<0.6) for each species
p_species<-plot_grid(p_species_agg[[1]],p_species_agg[[2]],p_species_agg[[3]],
          p_species_agg[[4]],p_species_agg[[5]],p_species_agg[[6]],
          p_species_agg[[7]],nrow = 3
          )

#summary the values:
df_tidy_agg$V_sum<-rowSums(df_tidy_agg[,paste0("V",1:7)],na.rm=T)
df_tidy_agg<-df_tidy_agg%>%
  mutate(lim_val_sum=V_sum)
#---------
#plotting
#---------
t_plot_agg<-raster::rasterFromXYZ(df_tidy_agg[,c("x","y","lim_val_sum")])
#using ggplot2
p_agg1<-ggplot() +
  geom_tile(data=raster::as.data.frame(t_plot_agg, xy = TRUE)%>%
              filter(!is.na(lim_val_sum))%>%
              mutate(lim_val_sum=factor(lim_val_sum,levels = paste0(0:7))),
            aes(x=x, y=y, fill=lim_val_sum), alpha=0.8) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis_d(direction = -1,option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Env limiting vs non-limiting area")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))

p_agg2<-ggplot() +
  geom_tile(data=raster::as.data.frame(t_plot_agg, xy = TRUE)%>%
              filter(!is.na(lim_val_sum))%>%
              mutate(lim_val_sum=factor(lim_val_sum,levels = paste0(0:7)))%>%
              mutate(lim_val_flag=ifelse(as.numeric(lim_val_sum)<=2,"<=2",">2")),
            aes(x=x, y=y, fill=lim_val_flag), alpha=0.8) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis_d(direction = -1,option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Env limiting vs non-limiting area")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))

p_agg3<-ggplot() +
  geom_tile(data=raster::as.data.frame(t_plot_agg, xy = TRUE)%>%
              filter(!is.na(lim_val_sum))%>%
              mutate(lim_val_sum=factor(lim_val_sum,levels = paste0(0:7)))%>%
              mutate(lim_val_flag=ifelse(as.numeric(lim_val_sum)<=3,"<=3",">3")),
            aes(x=x, y=y, fill=lim_val_flag), alpha=0.8) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis_d(direction = -1,option = "D")+
  coord_equal() +
  theme_map() +
  ggtitle("Env limiting vs non-limiting area")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))
#
plot_agg<-plot_grid(p_agg1,p_agg2,p_agg3,nrow=3)
#
save.path<-"./manuscript/3PG_results/"
ggsave(file=paste0(save.path,"limiting_factor_agg_species.png"),
       plot_agg,height = 15,width = 8)

