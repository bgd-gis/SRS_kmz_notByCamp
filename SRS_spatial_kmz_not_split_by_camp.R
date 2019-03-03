
#FACILITY ASSESSMENT RANDOM SAMPLING
#SIMPLE RANDOM SPATIAL SAMPLING- NOT DISSAGGREGATED BY CAMP

#REQUIREMENTS:
#1.)DATABASE OF FACILITIES (CSV)
#2.)CAMP BOUNDARY SHAPE FILE

#LOAD LIBRARIES
library(rgdal)
library(sp)
library(plotKML)
library(dplyr)

##################################################
#### LOAD REQUIRED SHAPEFILES ####################
##################################################

#################
# 1.) INPUTS
###############################################################
#IF USING OSM SHELTERS- COVERT THEM TO CENTROID POINTS IN QGIS - AND LOAD
###############################################################
#NEED TO LOAD FILE PATH WHICH CONTAINS SHELTER CENTROID POINTS
# gdb1<- "input file path to folder/gdb/gpkg containing centroid points


#IF YOUR SAMPLING FRAME IS DEFINED BY CAMP OR IF YOU NEED TO SPLIT KMZ FILES USE CAMP LATEST BOUNDARY SHPEFILE
#DONT TRUST DATA SET TO GET CAMP BOUNDARIES- USE ACTUALY CAMP BOUNDARY FILE
gdb2<-#"INPUT PATH TO GDB/FOLDER/GPKG CONTAINNG CAMP BOUNDARY"
  
  #LOAD SHELTER POINTS AND ADMIN BOUNDARIES
  # ogrListLayers(gdb1)
  ogrListLayers(gdb2) #HERE YOU CAN SEE THE NAMES OF THE GEOSPATIAL FILES IN THE GDB2 YOU DEFINED
#LOAD IN CAMP BOUNDARIES
cmp<-readOGR(dsn=gdb2, layer="Camp_Boundary_01102018")
#####################
#SAMPLING FRAME DATA
####################
#LOAD SHELTER FOOT PRINT CENTROIDS 
# hh<-readOGR(dsn=gdb1, layer = "NAME OF SHELTER PRINT CENTROID FILE")
#IN THIS CASE WE ARE WORKING WITH EDUCATION DATA
#PATH TO FACILITY ASSESSMENT CSV
data_path<-"C:\\Users\\zacka\\Documents\\IMPACT\\REACH_Bangladesh\\03_70DRF_UNICEF\\04_Education\\01_Education_Facility_Data\\"
dir(data_path)
file_name<-"Copy of List of Education Facilities.20190217.csv"
facil<-read.csv(paste0(data_path,file_name),stringsAsFactors = FALSE,
                na.strings= c("NA", NA, "", " ","n/a","n\a"))

############################

###############################
#2.) DO SOME SPATIAL MANIPULATIONS
##################################

#DEFINE DATA AS SPATIAL OBJECT
facil$lon<-facil$Longitude
facil$lat<-facil$Latitude

#get rid of any records that dont hav lon and
facil2<-facil[!is.na(facil$lon),]
facil3<-facil2[!is.na(facil2$lat),]

facil_sp<-facil3
facil_sp$Implementing.Partners
coordinates(facil_sp)<-~lon+lat
proj4string(facil_sp)<-proj4string(cmp)

#SPATIAL JOIN WITH CAMP LAYER
jd<-over(facil_sp, cmp)
# jd
dfj<-data.frame(coordinates(facil_sp),facil_sp@data,jd)

####################################
#SAMPLING TIME######################
####################################

#GET THREE REQUIRED SAMPLE SIZES
ss_95<-308
ss_95X1.15<-ss_95*1.15
ss_95X3<-ss_95*3

#SETWD TO STORE SAMPLE
setwd("ENTER PATH")
#GET RANDOM NUMBER FOR SEED
seed_val<-sample(1:1000000,1)
#MAKE SURE YOU ACTUALLY RECORD RANDOM SEED NUMBER SO THAT YOU CAN DUPLICATE
set.seed(267328)#747641
sr1<-sample(nrow(dfj),ss_95X3)
sampx3<-dfj[sr1,]

seed_val2<- sample(1:1000000, 1)
set.seed(707015)
sr1.15<-sample(nrow(sampx3), ss_95X1.15)
sampx1.15<-sampx3[sr1.15,]
# write.csv(sampx1.15, "Facility_sample_15_percent_buffer.csv")
sampx3_remaining<-sampx3[-sr1.15,]
nrow(sampx3_remaining)

sampx1.15$New_Camp_N<-as.character(sampx1.15$New_Camp_N)
sampx1.15$camp_name_final<-ifelse(is.na(sampx1.15$New_Camp_N),
                                  sampx1.15$Camp,sampx1.15$New_Camp_N)

sampx1.15$camp_name_final2<-str_replace_all(sampx1.15$camp_name_final,
                                            c("Camp "='C_', 
                                              "Nayapara "="N_",
                                              "Kutupalong "="K_",
                                              "Extension"="ext",
                                              "Choukhali"="Ch",
                                              "/Chakmarkul"="",
                                              "/Unchiprang"="",
                                              "/Nayapara EXP"=""))
head(sampx1.15)

sampx1.15_sp<-sampx1.15
sampx1.15_sp$ID<-1:nrow(sampx1.15_sp)
coordinates(sampx1.15_sp)<-~lon+lat
proj4string(sampx1.15_sp)<-proj4string(cmp)
getwd()

kml(obj=sampx1.15_sp, folder.name="Education_Assessment", 
    file.name=paste0("Facility_assessment_Round1_354.kml"),
    kmz=FALSE,altitude=0,plot.labpt=TRUE,
    labels=paste0(as.character(sampx1.15_sp$ID),"_",sampx1.15_sp$camp_name_final2,"_",
                  sampx1.15_sp$Facility.Name),
    LabelScale=0.5)

sampx1.15_sp %>% head()





