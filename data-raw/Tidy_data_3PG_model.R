#############################################
#Tidy the data from 3-PG model(provided by Volo)
#############################################
#NPP link:
#limited Env:https://www.envidat.ch/#/metadata/environmental-constraints-on-tree-growth
library(dplyr)
library(ggplot2)

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
species<-c("Abies_alba","Acer_pseudoplatanus","Fagus_sylvatica","Larix_decidua",
          "Picea_abies","Pinus_sylvestris","Quercus_robur")
##for each speices
df.lim_1991_2018_agg<-c()
for (i in 1:length(species)) {
  df.temp<-c()
  df.temp<-df.lim_1991_2018[grep(species[i],df.lim_1991_2018)]
  df.lim_1991_2018_agg[[i]]<-df.temp
  rm(df.temp)
}
names(df.lim_1991_2018_agg)<-species
##
for (i in length(df.lim_1991_2018_agg)) {
  #
  temp.path<-data.frame("path"=df.lim_1991_2018_agg[[i]],
  #refer to https://www.geeksforgeeks.org/extract-numbers-from-character-string-vector-in-r/
  "month"=as.numeric(gsub(".*?([0-9]+).*", "\\1", substr(df.lim_1991_2018_agg[[i]],90,95))))
  #select the growing season(May-Sep, following Volo's definitation):
  temp.path_sel<-temp.path %>%
    filter(month>=5 & month<=9)
  #
  for (j in 5:9) {
    df.exe<-temp.path %>%
      filter(month==j)
    df.proc<-df.exe[,1]%>%
      raster::stack() %>%
      # raster::plot()
      raster::as.data.frame(., xy = TRUE)
    #selected na values and identify the dominant variables
    df.proc<-df.proc[!is.na(df.proc[,3]),]%>%
      dplyr::rowwise()%>%
      #working to here, refer to:
      #https://stackoverflow.com/questions/63881552/how-to-just-keep-the-minimum-value-in-a-row-across-multiple-columns-and-make-all
      mutate(lim_val=,
             lim_var=)
  }
}


