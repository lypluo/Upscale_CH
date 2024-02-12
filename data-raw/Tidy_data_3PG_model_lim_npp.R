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

##calculating the Theil-sen slope and conducting MK.test for different periods
cal_sen_periods<-function(df,period){
  # df<-df.a_npp_1960_2018_agg
  # period<-c(1960,2018)

  df_tidy_period<-list()
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
      mutate(year=as.numeric(substr(species_year,6,9)))%>%
      filter(year>=period[1] & year<=period[2])
    #calculate the Theil_sen slope and MK test
    df.out<-df.proc_final%>%
      filter(!is.na(npp_anomaly))%>%
      group_by(x,y)%>%
      summarise(slope=cal_senslope(npp_anomaly)[,"slope"],
                p.value=cal_senslope(npp_anomaly)[,"p.value"])
    ##
    df_tidy_period[[i]]<-df.out
  }
  names(df_tidy_period)<-species_names
  #
  return(df_tidy_period)
}
##get the sen's slope in differnt periods
df_tidy_1960_2018<-cal_sen_periods(df.a_npp_1960_2018_agg,c(1960,2018))
df_tidy_1960_1989<-cal_sen_periods(df.a_npp_1960_2018_agg,c(1960,1989))
df_tidy_1990_2018<-cal_sen_periods(df.a_npp_1960_2018_agg,c(1990,2018))
#
df_all<-list(df_tidy_1990_2018,df_tidy_1960_1989,df_tidy_1990_2018)
names(df_all)<-c("1990-2018","1960-1989","1990-2018")

#save the data:
save.path<-"./data/3PG/"
# save(df_all,file = paste0(save.path,"species_npp_anomaly_trend_and_sig.RDA"))

##making the plots###
#load(paste0(save.path,"species_npp_anomaly_trend_and_sig.RDA"))
library(cowplot)
#function:
plot_map<-function(df,species_proc_name,period){
  # df<-df_tidy_1960_2018[[2]]
  # species_proc_name<-names(df_tidy_1960_2018)[2]
  # period<-c(1960,2018)
  #
  df<-df %>%
  #if p.value>=0.05, the trend of varation of npp anomaly is not significant
  #if p.value<0.05, ...is signifcant
  mutate(sig=ifelse(p.value>=0.05,0,1))%>%
    mutate(sig=factor(sig,levels=c(0,1)))
  #
  t_data1<-raster::rasterFromXYZ(df[,c("x","y","slope")])
  t_data2<-raster::rasterFromXYZ(df[,c("x","y","sig")])

  # par(mfrow=c(2,1))
  # raster::plot(t_plot1,main=species_proc_name)
  # #using the ggplot2
  #-->refer:https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
  p_plot<-ggplot() +
    geom_point(data=raster::as.data.frame(t_data2, xy = TRUE)%>%
                 filter(!is.na(sig))%>%
                 mutate(sig_flag=ifelse(sig==0,".","x"))%>%
                 filter(sig_flag=="x"),
               aes(x=x,y=y),shape=20,col="black",size=0.6)+
    geom_tile(data=raster::as.data.frame(t_data1, xy = TRUE)%>%
                filter(!is.na(slope))%>%
                mutate(slope_flag=case_when(slope< -0.25 ~ "< -0.25",
                                            slope>= -0.25 & slope <=0 ~"-0.25 - 0",
                                            slope>0 & slope <=0.25~"0-0.25",
                                            slope>0.25 & slope <=0.5~"0.25-0.5",
                                            slope>0.5~">0.5"))%>%
                mutate(slope_flag=factor(slope_flag,
                                         levels = c("< -0.25","-0.25 - 0","0-0.25","0.25-0.5",">0.5"))),
              aes(x=x, y=y, fill=slope_flag), alpha=0.6) +
    # geom_polygon(data=OR, aes(x=long, y=lat, group=group),
    #              fill=NA, color="grey50", size=0.25) +
    # scale_fill_viridis_d(direction = 1,option = "C") +
    coord_equal() +
    theme_map() +
    ggtitle(paste0(species_proc_name,paste0(":",period[1],"-",period[2])))+
    theme(legend.position="right",
          legend.key.width=unit(0.8, "cm"),
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = unit(c(0,0,0,0), "cm")
    )
  #
  return(p_plot)
}

#----------
#A. plotting the trend of npp and its significance for each species(1960-2018)
#----------
##plotting the results:
plot_all_1960_2018<-c()
for (i in 1:length(df_tidy_1960_2018)) {
  plot_temp<-plot_map(df_tidy_1960_2018[[i]],names(df_tidy_1960_2018)[i],c(1960,2018))
  plot_all_1960_2018[[i]]<-plot_temp
  rm(plot_temp)
}
##merge all the plots:
save.path<-"./manuscript/3PG_results/"
p_all1<-plot_grid(plot_all_1960_2018[[1]],plot_all_1960_2018[[2]],
                  align = "hv")

#----------
#B. plotting the trend of npp and its significance for each species(1960-1990)
#----------
plot_all_1960_1989<-c()
for (i in 1:length(df_tidy_1960_1989)) {
  plot_temp<-plot_map(df_tidy_1960_1989[[i]],names(df_tidy_1960_1989)[i],c(1960,1989))
  plot_all_1960_1989[[i]]<-plot_temp
  rm(plot_temp)
}
##merge all the plots:
save.path<-"./manuscript/3PG_results/"
p_all2<-plot_grid(plot_all_1960_1989[[1]],plot_all_1960_1989[[2]],
                  align = "hv")

#----------
#C. plotting the trend of npp and its significance for each species(1990-2018)
#----------
plot_all_1990_2018<-c()
for (i in 1:length(df_tidy_1990_2018)) {
  plot_temp<-plot_map(df_tidy_1990_2018[[i]],names(df_tidy_1990_2018)[i],c(1990,2018))
  plot_all_1990_2018[[i]]<-plot_temp
  rm(plot_temp)
}
##merge all the plots:
save.path<-"./manuscript/3PG_results/"
p_all3<-plot_grid(plot_all_1990_2018[[1]],plot_all_1990_2018[[2]],
                  align = "hv")

##merging the plots
plot_agg<-plot_grid(p_all1,p_all2,p_all3,nrow=3)
##save the plots
ggsave(file=paste0(save.path,"npp_anomaly_trend_each_species.png"),
       plot_agg,height = 15,width = 15)
