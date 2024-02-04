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

#----------------------------
#(1)processing the results from the limited environmental drivers
#----------------------------
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
#working the
#-->identify the limited variables for each month(May-Sep) for each species
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
    #selected na values and identify the dominant variables
    df.proc_final<-df.proc[!is.na(df.proc[,3]),]%>%
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
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
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
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)
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
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA)
      )

  }
p_merge<-plot_grid(p_plot1,p_plot2,align = "hv")
#
return(p_merge)
}
##plotting the results:
for (i in 1:length(df_tidy)) {
  simple_plot_map(df_tidy[[i]],names(df_tidy)[i])
}


