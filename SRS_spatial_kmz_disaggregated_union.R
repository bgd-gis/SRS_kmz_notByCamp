#HH SAMPLING 
#SIMPLE RANDOM SPATIAL SAMPLING-DISSAGREGATED BY CAMP
#WILL USE OSM SHELTER FOOTPRINT

#REQUIREMENTS:
#1.)SHELTER FOOTPRINT
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
gdb<-"C:\\Users\\zacka\\Documents\\IMPACT\\REACH_Bangladesh\\01_general_gis\\03_shapefiles\\01_boundaries\\01_admin_boundaries"#"INPUT PATH TO GDB/FOLDER/GPKG CONTAINNG CAMP BOUNDARY"
  #LOAD SHELTER POINTS AND ADMIN BOUNDARIES
# ogrListLayers(gdb1)
ogrListLayers(gdb) #HERE YOU CAN SEE THE NAMES OF THE GEOSPATIAL FILES IN THE GDB2 YOU DEFINED
#LOAD IN CAMP BOUNDARIES
unions<-readOGR(dsn=gdb, layer="BGD_Teknaf_Ukhia_Unions")

##########***
hh_centroid_gdb<-"C:/Users/zacka/Documents/IMPACT/REACH_Bangladesh/02_70DIY_ECHO/05_MSNA/01_data/01_raw_data/ISCG_HC_MSNA_Questionnaire_Final_V2_2018_Unclean_compiled_AllData.csv"
# ogrListLayers(hh_centroid_gdb)
# hh<-readOGR(dsn=hh_centroid_gdb, layer="shelters_r3_centroid")
#SPATIAL TRANSFOR TO SAME PROJECTION
hh<-read.csv(hh_centroid_gdb,stringsAsFactors = FALSE, na.strings= c(NA,"<NA>","n/a",""," "))
unique(hh$survey_consent)
hh2<-hh[hh$survey_consent=="yes",]
unique(hh2$survey_consent)
hh2sp<-hh2
coordinates(hh2sp)<-~X_gps_reading_longitude+X_gps_reading_latitude
proj4string(hh2sp)<-proj4string(unions)

#SPATIAL JOIN
jd<-over(hh2sp, unions)
dfj<-data.frame(coordinates(hh2sp),jd)

#LOAD IN SAMPLING FRAME^****
# sample_frame_path<-"C:/Users/zacka/Documents/IMPACT/REACH_Bangladesh/03_70DRF_UNICEF/04_Education/"
# sf<-read.csv(paste0(sample_frame_path,"kmz_sample_request_template - sample_variation1(1).csv"), stringsAsFactors = FALSE, na.strings =c("n/a",""," ", NA, "NA","<NA>"))
# 
# sf2<-sf[!is.na(sf$points_needed),]
# sf2$Camp_Name
# hh_ss<-merge(dfj, sf2, by.x="New_Camp_N", by.y="Camp_Name", all.x=TRUE, all.y=FALSE)
# hh_ss2<-hh_ss[!is.na(hh_ss$points_needed),]
# hh_ss2$New_Camp_N<-as.character(hh_ss2$New_Camp_N)

##########***

list_hh_by_union<-split(dfj,dfj$adm4_en)
lapply(list_hh_by_union, nrow)
sf$number_points_w_buffer
length(list_hh_by_union)
setwd( "C:\\Users\\zacka\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DIY - ECHO\\MSNA\\GIS_HC_MSNA\\FGD_Points")
#get random seed- but then store it for later under set
seed_val<-sample(1:1000000,1)
set.seed(543149)#385840
seed_list<-list()
list_r1_samples<-list()
for ( i in 1: length(list_hh_by_union)){
  seed_val<-sample(1:1000000,1)
  #store seeds in a list so that you can repeat this later
  seed_list[[i]]<-seed_val
  set.seed(seed_val)
  sr1<-sample(nrow(list_hh_by_union[[i]]),20)
  sr1_samps<-list_hh_by_union[[i]][sr1,]
  list_r1_samples[[i]]<-sr1_samps
  #get number of samples and union name for name naming purposes
  num_samples<-20 #NORMALLY READ IN CSV AND EXTRACT NUMBER FROM ROW WITHIN BOUNDARY
  union_name<-unique(sr1_samps[,"adm4_en"])
  sr1_sampsa<-sr1_samps
  sr1_sampsa$CID<-1:nrow(sr1_sampsa)
  sr1_sampsb<-data.frame(sr1_sampsa$X_gps_reading_longitude, sr1_sampsa$X_gps_reading_latitude, 
                         sr1_sampsa$CID,sr1_sampsa$adm4_en)
  un_name<-unique(sr1_sampsb$adm4_en)
  colnames(sr1_sampsb)<-c("lon","lat","CID","union_label")
  sampsp<-sr1_sampsb
  coordinates(sampsp)<-~lon+lat
  proj4string(sampsp)<-proj4string(unions)
  #write kml
  kml(obj=sampsp, folder.name=paste0("FGD_draw1_",union_name,"_",num_samples), 
      file.name=paste0("FGD_draw1_",union_name,"_",num_samples,".kml"),
      kmz=FALSE,altitude=0,plot.labpt=TRUE,labels=paste0(sampsp$CID,union_name,"_fgd"),LabelScale=0.5)
  print(list_r1_samples[[i]] %>% nrow())
}
unique(hh_ss2$New_Camp_N)
unique(dfj$New_Camp_N)
#take the seed list and save it into random seeds used
dput(seed_list)
seed.df<-do.call("rbind",seed_list)
seed_list
#save seed_list here so that you can repeat this and get same radomness
rn_seeds_used<-c("659152", "83130", "191899", "88587", "432505", "423963", "463387", 
                 "347852", "826091")
length(rn_seeds_used)


#samples left?
samps_used<-do.call("rbind",list_r1_samples)
#pool that was taken from
rownames(df_ss2)
rownames(samps_used)
remain_index2<-which(rownames(df_ss2)%in%rownames(samps_used)==FALSE)
remain_total2<-df_ss2[remain_index2,]
remain_total2 %>% head()

rem2sp<-remain_total2
coordinates(rem2sp)<-~coords.x1+coords.x2
proj4string(rem2sp)<-proj4string(cmp)
gdb_rem
writeOGR(obj =rem2sp,layer ="rem_after_2nd",dsn = gdb_rem, driver = "ESRI Shapefile")














  
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





