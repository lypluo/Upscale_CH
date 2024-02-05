#############################################
#Tidy the data from 3-PG model(provided by Volo)
#############################################
#NPP link:
#limited Env:https://www.envidat.ch/#/metadata/environmental-constraints-on-tree-growth
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
#----------------------------
#(0)load the data and test code
#----------------------------
#code refer to Volo
data.path<-"D:/data/Upscale_project_data/From_3PG_model/"
f_NPP_test <- raster::raster(paste0(data.path,'/npp_anomalies/piab_2003.tif'))

f_lim_env <- raster::raster(paste0(data.path,'/lim_factors_average/Fagus sylvatica_1991_2018_f_vpd_5.tif'))

#
raster::plot(f_NPP_test)

raster::as.data.frame(f_NPP_test, xy = TRUE) %>%

  dplyr::filter(!is.na(piab_2003)) %>%

  ggplot(aes(piab_2003))+

  geom_histogram()

#
raster::as.data.frame(f_lim_env, xy = TRUE) %>%
  dplyr::filter(!is.na(`Fagus_sylvatica_1991_2018_f_vpd_5`)) %>%

  ggplot(aes(`Fagus_sylvatica_1991_2018_f_vpd_5`))+

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
#processing the results from the limited environmental drivers
##################################
df.lim<-list.files(paste0(data.path,'lim_factors_average/'), full.names = TRUE)
#only selecting the limiting factors in 1991-2018
df.lim_1991_2018<-df.lim[grep("1991_2018",df.lim)]
#species:
species<-c("Abies alba","Acer pseudoplatanus","Fagus sylvatica","Larix decidua",
          "Picea abies","Pinus sylvestris","Quercus robur")
species_names<-c("Abies_alba","Acer_pseudoplatanus","Fagus_sylvatica","Larix_decidua",
              "Picea_abies","Pinus_sylvestris","Quercus_robur")
##for each speices
df.lim_1991_2018_agg<-c()
for (i in 1:length(species)) {
  df.temp<-c()
  df.temp<-df.lim_1991_2018[grep(species[i],df.lim_1991_2018)]
  df.lim_1991_2018_agg[[i]]<-df.temp
  rm(df.temp)
}
names(df.lim_1991_2018_agg)<-species_names
####
#----------
#-->identify the limited variables for each month(May-Sep) for each species
#----------
df_tidy<-list()
for (i in 1:length(df.lim_1991_2018_agg)) {
  #
  species_proc_name<-names(df.lim_1991_2018_agg)[i]
  temp.path<-data.frame("path"=df.lim_1991_2018_agg[[i]],
  #refer to https://www.geeksforgeeks.org/extract-numbers-from-character-string-vector-in-r/
  "month"=as.numeric(gsub(".*?([0-9]+).*", "\\1", substr(df.lim_1991_2018_agg[[i]],
     str_length(df.lim_1991_2018_agg[[i]])-5,str_length(df.lim_1991_2018_agg[[i]])))))

  #select the growing season(May-Sep, following Volo's definitation):
  temp.path_sel<-temp.path %>%
    filter(month>=5 & month<=9)
  #
  df_species_temp<-c()
  for (j in 5:9) {
    df.exe<-temp.path %>%
      filter(month==j)
    df.proc<-df.exe[,1]%>%
      raster::stack() %>%
      # raster::plot()
      raster::as.data.frame(., xy = TRUE)
    #identify the dominant variables
    df.proc_final<-df.proc%>%
      # df.proc[!is.na(df.proc[,3]),]%>%
      #https://stackoverflow.com/questions/63881552/how-to-just-keep-the-minimum-value-in-a-row-across-multiple-columns-and-make-all
      # mutate(lim_val= purrr::pmap(across(starts_with(species_proc_name)), min))
      rownames_to_column('id') %>%  # creates an ID number
      pivot_longer(starts_with(species_proc_name),
                                   names_to = "lim_longvar",values_to = "lim_val")%>%
      group_by(id)%>%
      slice(which.min(lim_val))%>%
      mutate(specie_name=species_proc_name,
             #select the limiting variables-->spearate the limiting factor by "_"
             lim_var=unlist(str_split(substr(lim_longvar,str_length(lim_longvar)-5,str_length(lim_longvar)),"_",3))[2])
    #put the data between May-Sep together:
    df_species_temp<-bind_rows(df_species_temp,df.proc_final)
    rm(df.proc_final)
  }
  ##merge the data in the growing season(May-Sep):
  #each pixel have 5 points--5 months
  #-->identify the limiting factors in growing season
  tt1<-df_species_temp %>%
    group_by(x,y,specie_name)%>%
    count(lim_var)%>%
    slice(which.max(n))%>%
    mutate(lim_var_GS=lim_var,lim_var=NULL,
           n=NULL)
  tt2<-left_join(df_species_temp,tt1,by=c("x","y","specie_name"))
  #identify the annually limiting factors and the limited values
  t_final<-tt2%>%
    filter(lim_var_GS==lim_var)%>%
    group_by(x,y,specie_name)%>%
    dplyr::summarise(lim_val_mean=mean(lim_val),
                     lim_var_GS=unique(lim_var_GS))
  df_tidy[[i]]<-t_final
}
names(df_tidy)<-species_names
#save the data:
save.path<-"./data/3PG/"
# save(df_tidy,file = paste0(save.path,"species_GS_limiting_vars.RDA"))

##making the plots###
# load(paste0(save.path,"species_GS_limiting_vars.RDA"))
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
df_tidy_agg<-df_tidy_recal[[1]]%>%
  select(x,y,lim_val_recal)%>%
  mutate(V1=lim_val_recal)
for (i in 2:length(df_tidy_recal)) {
  raster_add<-df_tidy_recal[[i]]%>%select(x,y,lim_val_recal)
  #test
  ggplot() +
    geom_tile(data=raster::as.data.frame(raster_add, xy = TRUE)%>%
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
  #
  names(raster_add)<-c("x","y",paste0("V",i))
  df_tidy_agg<-bind_rows(df_tidy_agg,raster_add)
}
#summary the values:
df_tidy_agg$V_sum<-rowSums(df_tidy_agg[,paste0("V",1:7)],na.rm=T)
df_tidy_agg<-df_tidy_agg%>%
  mutate(lim_val_sum=V_sum)
#---------
#plotting
#---------
t_plot_agg<-raster::rasterFromXYZ(df_tidy_agg[,c("x","y","lim_val_sum")])
#using ggplot2
p_agg<-ggplot() +
  geom_tile(data=raster::as.data.frame(t_plot_agg, xy = TRUE)%>%
              filter(!is.na(lim_val_sum))%>%
              mutate(lim_val_sum=factor(lim_val_sum,levels = paste0(0:7))),
            aes(x=x, y=y, fill=lim_val_sum), alpha=0.8) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis_d()+
  coord_equal() +
  theme_map() +
  ggtitle("Env limiting vs non-limiting area")+
  theme(legend.position="right",
        legend.key.width=unit(0.8, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = unit(c(0,0,0,0), "cm"))
