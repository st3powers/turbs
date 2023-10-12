
# clear everything

rm(list = ls())

# load packages

library(tidyverse)
library(ggplot2)
library(readr)
library(OpenStreetMap)
library(gridExtra)
#######################
  
# read in data
  
data0<-read_csv("edidata_TX.csv")
data<-data0 %>% as.data.frame()
  
sampledata0<-read_csv("sampledata.csv")
sampledata<-sampledata0 %>% as.data.frame()
  
sitenames <- tribble(
  ~site, ~sitename, ~siteabbr,
  "AH1", "arm1", "A1",
  "AH2", "upper", "U",
  "AH3", "lower", "L",
  "AH4", "middle", "M",
  "AH5", "arm2", "A2"
  )
  
sampledata<-merge(sitenames,sampledata,by="site")
  
###############################
  
# boat turbidity, satellite turbidity, and predictions
# ndti_flag=1 indicates the pixel was cloudy, cloud-shadowed, or a nextdoor neighbor of a cloudy or cloud-shadowed pixel
  
data_use<-data %>% filter(ndti_flag==0)
x<-data_use$turbidity_ntu
y<-data_use$ndti
  
logEstimate <- lm(ndti~log(turbidity_ntu),data=data_use)
xvec <- seq(min(data_use$turbidity_ntu),max(data_use$turbidity_ntu),length=1000)
logpred <- predict(logEstimate,newdata=data.frame(turbidity_ntu=xvec))
pred <- data.frame(x = xvec, y = logpred)
  
turb_ndti_plot_nocolor<-ggplot(data_use,aes(x=turbidity_ntu,y=ndti)) +
  geom_point(size=0.1,col=gray(0.3))+
  geom_smooth(method="lm",lty=5,col="black",se=FALSE,lwd=1)+
  geom_line(data = pred, aes(x=x, y=y),lty=1,col="black",lwd=1)+
  xlim(c(0,99))+
  xlab("In-lake Turbidity (NTU)")+
  ylab("Satellite Turbidity (NDTI)")+
  theme_bw()
turb_ndti_plot_nocolor
  
lm_turbndti<-lm(ndti~turbidity_ntu,data=data_use)
  
sink("ndti_models.txt")
summary(logEstimate)
print("################################")
summary(lm_turbndti)
sink()

ggsave(filename = paste("turb_ndti_plot.png",sep=""),
    plot = turb_ndti_plot_nocolor, width = 4.5, height = 4.3, units = "in",dpi=600)
  
########################################
  
# boat turbidity, satellite turbidity, and predictions for subsets of data nearest in time to the sentinel2 acquisition time 

data_use_restricted1<-data %>% filter(ndti_flag==0) %>% filter(hour_dec>10.91)
  
logEstimate <- lm(ndti~log(turbidity_ntu),data=data_use_restricted1)
xvec <- seq(min(data_use_restricted1$turbidity_ntu),max(data_use_restricted1$turbidity_ntu),length=1000)
logpred <- predict(logEstimate,newdata=data.frame(turbidity_ntu=xvec))
pred <- data.frame(x = xvec, y = logpred)
  
turb_ndti_plot_nocolor_restricted1<-ggplot(data_use_restricted1,aes(x=turbidity_ntu,y=ndti)) +
  geom_point(size=0.5,col=gray(0.3))+
  geom_smooth(method="lm",lty=5,col="black",se=FALSE,lwd=1)+
  scale_color_viridis_c()+
  xlab("In-lake Turbidity (NTU)")+
  ylab("Satellite Turbidity (NDTI)")+
  theme_bw()
turb_ndti_plot_nocolor_restricted1
  
lm_turbndti<-lm(ndti~turbidity_ntu,data=data_use_restricted1)
  
sink("ndti_models_retricted1.txt")
summary(logEstimate)
print("################################")
summary(lm_turbndti)
sink()

data_use_restricted2<-data %>% filter(ndti_flag==0) %>% filter(hour_dec>12.077 & hour_dec<12.743)
  
logEstimate <- lm(ndti~log(turbidity_ntu),data=data_use_restricted2)
xvec <- seq(min(data_use_restricted2$turbidity_ntu),max(data_use_restricted2$turbidity_ntu),length=1000)
logpred <- predict(logEstimate,newdata=data.frame(turbidity_ntu=xvec))
pred <- data.frame(x = xvec, y = logpred)
  
turb_ndti_plot_nocolor_restricted2<-ggplot(data_use_restricted2,aes(x=turbidity_ntu,y=ndti)) +
  geom_point(size=0.5,col=gray(0.3))+
  geom_smooth(method="lm",lty=5,col="black",se=FALSE,lwd=1)+
  scale_color_viridis_c()+
  xlab("In-lake Turbidity (NTU)")+
  ylab("Satellite Turbidity (NDTI)")+
  theme_bw()
turb_ndti_plot_nocolor_restricted2
  
lm_turbndti<-lm(ndti~turbidity_ntu,data=data_use_restricted2)

sink("ndti_models_retricted2.txt")
summary(logEstimate)
print("################################")
summary(lm_turbndti)
sink()

png(filename = paste("turb_ndti_restricted.png",sep=""),
    width = 7, height = 3.5, units = "in",res=300)
grid.arrange(turb_ndti_plot_nocolor_restricted1+xlim(14,50)+ylim(-.26,.05),
             turb_ndti_plot_nocolor_restricted2+xlim(14,50)+ylim(-.26,.05),
             nrow=1)
dev.off()
  
######################################################
  
# create basemap
# note that during peer review, Stamen maps including the "stamen-terrain" map option became deprecated in the OpenStreetMap package
# substituting a different map type to generate the figures
  
map0 <- openmap(upperLeft = c(33.775, -98.3), 
                lowerRight = c(33.63, -98.49),
#                type = 'stamen-terrain',zoom=12) # stamen-terrain is deprecated
                type = 'osm',zoom=12)
  
###########################################################
  
# set map options
ptsize<-0.9
ptalpha<-0.5
  
##############################################################
  
# map turbidity along boat path
  
data_use<-data
  
plot_turbidity_ntu<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(map0)) +
  geom_point(data = data_use, 
             size=ptsize,
             alpha=ptalpha,
             aes(x = lon_dec, y = lat_dec,color=turbidity_ntu))+
  scale_color_viridis_c("Turbidity (NTU)",direction=)+
  xlab("")+ylab("")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction="horizontal",
    legend.title.align=0.5,
    legend.key.width = unit(0.6,"cm"),
    legend.position = c(0.28, 0.8))+
  guides(col=guide_colorbar(title.position = "top"))+
  geom_segment(x = -98.375, y = 33.7675, xend = -98.352, yend = 33.7666,lwd=2.5,color=gray(0.3))+
  geom_segment(x = -98.47, y = 33.705, xend = -98.426, yend = 33.705,lwd=1,color=gray(0))+
  annotate("text",x = -98.448, y = 33.712,label="4 km")
plot_turbidity_ntu<-plot_turbidity_ntu+
  geom_point(data=sampledata %>% dplyr::select(siteabbr,lat,lon) %>% unique(),
             aes(y=lat,x=lon),shape=21,size=3,color="white",stroke=1)+
  geom_text(data=sampledata %>% dplyr::select(siteabbr,lat,lon) %>% unique(),
              aes(y=lat-0.005,x=lon+0.007,label=siteabbr))
plot_turbidity_ntu

###################################################################

# map time along boat path
  
plot_hourdec<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(map0)) +
  geom_point(data = data_use, 
             size=ptsize,
             alpha=ptalpha,
             aes(x = lon_dec, y = lat_dec,color=hour_dec))+
  scale_color_viridis_c("Hour of day",direction=)+
  xlab("")+ylab("")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction="horizontal",
    legend.title.align=0.5,
    legend.key.width = unit(0.6,"cm"),
    legend.position = c(0.28, 0.8))+
  guides(col=guide_colorbar(title.position = "top"))+
  geom_segment(x = -98.375, y = 33.7675, xend = -98.352, yend = 33.7666,lwd=2.5,color=gray(0.3))+
  geom_segment(x = -98.47, y = 33.705, xend = -98.426, yend = 33.705,lwd=1,color=gray(0))+
  annotate("text",x = -98.448, y = 33.712,label="4 km")

plot_hourdec<-plot_hourdec+
  geom_point(data=sampledata %>% dplyr::select(siteabbr,lat,lon) %>% unique(),
             aes(y=lat,x=lon),shape=21,size=3,color="white",stroke=1)+
  geom_text(data=sampledata %>% dplyr::select(siteabbr,lat,lon) %>% unique(),
            aes(y=lat-0.005,x=lon+0.007,label=siteabbr))

plot_hourdec

########################################################################

# map ndti along boat path, unflagged pixels only

data_use<-data %>% filter(ndti_flag==0)
plot_ndti_filtered<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(map0)) +
  geom_point(data = data_use, 
             size=ptsize,
             alpha=ptalpha,
             aes(x = lon_dec, y = lat_dec,color=ndti))+
  scale_color_viridis_c("ndti",direction=)+
  xlab("")+ylab("")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction="horizontal",
    legend.title.align=0.5,
    legend.key.width = unit(0.6,"cm"),
    legend.position = c(0.28, 0.8))+
  guides(col=guide_colorbar(title.position = "top"))

plot_ndti_filtered

#############################################################################

# map turbidity along boat path, unflagged pixels only

data_use<-data %>% filter(ndti_flag==0)
plot_turb_filtered<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(map0)) +
  geom_point(data = data_use, 
             size=ptsize,
             alpha=ptalpha,
             aes(x = lon_dec, y = lat_dec,color=turbidity_ntu))+
  scale_color_viridis_c("Turbidity (NTU)",direction=)+
  xlab("")+ylab("")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction="horizontal",
    legend.title.align=0.5,
    legend.key.width = unit(0.6,"cm"),
    legend.position = c(0.28, 0.8))+
  guides(col=guide_colorbar(title.position = "top"))

plot_turb_filtered
  
png(filename = paste("turb_map.png",sep=""),
    width = 4, height = 3, units = "in",res=300)
plot_turbidity_ntu
dev.off()
  
png(filename = paste("hourofday.png",sep=""),
    width = 4, height = 3, units = "in",res=300)
plot_hourdec
dev.off()

png(filename = paste("turb_ndti_map_filtered.png",sep=""),
    width = 8, height = 3.5, units = "in",res=600)
grid.arrange(plot_ndti_filtered, plot_turb_filtered,nrow=1)
dev.off()
  
#####################################################################################

# map Sentinel 2 pixel classes along boat path
  
data_use<-data
    
class_plot<-OpenStreetMap::autoplot.OpenStreetMap(OpenStreetMap::openproj(map0)) +
  facet_wrap(~class,ncol=2)+
  geom_point(data = data_use, 
             size=ptsize,
             aes(x = lon_dec, y = lat_dec))+
  xlab("")+ylab("")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
      )+
  theme(legend.position = "none")
  
class_plot

ggsave(filename = paste("class_map.png",sep=""),
       plot = class_plot, width = 6, height = 7.5, units = "in",dpi=600)

##################################################################################################

# do counts of pixel classes

data_notflagged<-data %>% filter(ndti_flag==0)
prop_notflagged <- length(data_notflagged[,1])/length(data[,1]) 
prop_notflagged

prop_water<-length(which(data$class=="water"))/length(data[,1])
prop_clouds<-length(which(data$class=="clouds"))/length(data[,1]) 
prop_cirrus<-length(which(data$class=="cirrus"))/length(data[,1]) 
prop_shadows<-length(which(data$class %in% c("shadows","shadows2")))/length(data[,1]) 
prop_neighbors<-length(which(data$class=="neighbors"))/length(data[,1])

sum(prop_clouds,prop_cirrus,prop_shadows,prop_neighbors)
sum(prop_water,prop_clouds,prop_cirrus,prop_shadows,prop_neighbors)

prop_water+prop_neighbors

##########################################################################################################

# merge sample data (secchi, tss, turbidimeter) with sensor turbidity data to create validation data frames

samplesensor_merged<-merge(sampledata,data)
samplesensor_merged$lat_diff<-samplesensor_merged$lat_dec-samplesensor_merged$lat
samplesensor_merged$lon_diff<-samplesensor_merged$lon_dec-samplesensor_merged$lon
samplesensor_merged<-samplesensor_merged %>% filter(abs(lat_diff)<0.0003,abs(lon_diff)<0.0003)

validationdata<-samplesensor_merged %>% dplyr::select(site,sitename,siteabbr,lat,lon,variable,value_discrete=value,value_sensor=turbidity_ntu) %>% 
  group_by(site,sitename,siteabbr,lat,lon,variable,value_discrete) %>%
  dplyr::summarize(value_sensor=mean(value_sensor,na.rm=TRUE)) %>%
  as.data.frame()

validationdata1<-validationdata %>% select(site,sitename,siteabbr,lat,lon,variable,value=value_discrete)
validationdata2<-validationdata %>% select(site,sitename,siteabbr,lat,lon,variable,value=value_sensor)
validationdata2$variable<-"turbidity_sensor"
validationdata2<-unique(validationdata2)
validationdata<-rbind(validationdata1,validationdata2)

validationdata_wide<-validationdata %>% pivot_wider(names_from=variable,values_from=value)

###################################################################################################################

# plot validation data, fit models, and predict

plot_turbsensor_turbsample<-validationdata_wide %>%
  ggplot(aes(y=turbidity_sensor,x=turbidity,label=siteabbr))+
  geom_smooth(method="lm",lty=1,col="dark gray",se=FALSE)+
  geom_point(shape=21,size=8,fill="white")+
  geom_text()+
  ylab("Sensor Turbidity (NTU)")+
  xlab("Sample Turbidity (NTU)")+
  expand_limits(x = c(0,72), y = c(0,72))+
  theme_bw()

lm1<-lm(turbidity_sensor~turbidity,data=validationdata_wide)

plot_tss_turb<-validationdata_wide %>%
  ggplot(aes(x=turbidity,y=tss,label=siteabbr))+
  geom_smooth(method="lm",lty=1,col="dark gray",se=FALSE)+
  geom_point(shape=21,size=8,fill="white")+
  geom_text()+
  ylab("Total Suspended Solids (mg/L)")+
  xlab("Sample Turbidity (NTU)")+
  expand_limits(x = c(0,72), y = c(0,72))+
  theme_bw()

lm2<-lm(tss~turbidity,data=validationdata_wide)

plot_tss_turbsensor<- validationdata_wide %>%
  ggplot(aes(x=turbidity_sensor,y=tss,label=siteabbr))+
  geom_smooth(method="lm",lty=1,col="dark gray",se=FALSE)+
  geom_point(shape=21,size=8,fill="white")+
  geom_text()+
  ylab("Total Suspended Solids (mg/L)")+
  xlab("Sensor Turbidity (NTU)")+
  expand_limits(x = c(0,72), y = c(0,72))+
  theme_bw()

lm3<-lm(tss~turbidity,data=validationdata_wide)

plot_om_turb<-validationdata_wide %>%
  ggplot(aes(y=100*afdm/tss,x=turbidity,label=siteabbr))+
  geom_point(shape=21,size=8,fill="white")+
  geom_text()+
  ylab("Suspended Organic Matter (% of TSS)")+
  xlab("Sample Turbidity (NTU)")+
  expand_limits(x = c(0,72), y = c(0,72))+
  theme_bw()

lm4<-lm(100*afdm/tss~turbidity,data=validationdata_wide)

linlog <- lm(secchi~log(turbidity),data=validationdata_wide)
xvec <- seq(min(validationdata_wide$turbidity),max(validationdata_wide$turbidity),length=1000)
logpred <- predict(linlog,newdata=data.frame(turbidity=xvec))
pred <- data.frame(x = xvec, y = logpred)
pred$siteabbr<-"nothing"

x<-validationdata_wide$turbidity
y<-validationdata_wide$secchi
decay<-nls(y ~ 1/(b + x^c), start=list(b=1,c=1))
xvec <- seq(min(validationdata_wide$turbidity),max(validationdata_wide$turbidity),length=1000)
preds <- predict(decay,newdata=data.frame(x=xvec))
pred<- data.frame(x = xvec, y = preds)
pred$siteabbr<-"nothing"

plot_secchi_turb<-validationdata_wide %>%
  ggplot(aes(y=secchi,x=turbidity,label=siteabbr))+
  geom_line(data = pred, aes(x=x, y=y),lty=1,col="dark gray",lwd=1)+
  geom_point(shape=21,size=8,fill="white")+
  geom_point(shape=21,size=8,fill="white")+
  geom_text()+
  ylab("Secchi Depth (m)")+
  xlab("Sample Turbidity (NTU)")+
  expand_limits(x = c(0,72))+
  theme_bw()

png(filename = "samples_multi.png",
    width = 6.5, height =6.5, units = "in",res=300)
grid.arrange(plot_turbsensor_turbsample,
             plot_tss_turb,#plot_tss_turbsensor,
             plot_secchi_turb,
             plot_om_turb,
             nrow=2)
dev.off()

# output model summaries

sink("lm_models.txt")
print(summary(lm1))
print("###############################")
print(summary(lm2))
print("###############################")
print(summary(lm3))
print("###############################")
print(summary(lm4))
print("###############################")
print(summary(linlog))
print("###############################")
print(summary(decay))
sink()

    