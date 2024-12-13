# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Questions - OF18, OF19, OF31, OF32, OF33, OF37

#Lake Density within 2km of AA - OF18
OF18<-FWA_lakes %>%
  st_intersection(FWetlands2km) %>%
  mutate(LkArea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  #mutate(LareaHa=as.numeric(st_area(.))*0.001) %>%
  #write_sf(RoadsD,file.path(spatialOutDir,'RoadsD.gpkg'),overwrite=TRUE)
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(Lake_ha=sum(LkArea_Ha),Buff_area=first(Buffarea_Ha)) %>%
  mutate(LkDensity=Lake_ha/(Buff_area*0.01)) %>%
  mutate(OF18_1=if_else(LkDensity<0.02,1,0)) %>%
  mutate(OF18_2=if_else(LkDensity >=0.02 & LkDensity < 0.08,1,0)) %>%
  mutate(OF18_3=if_else(LkDensity >=0.08 & LkDensity < 0.18,1,0)) %>%
  mutate(OF18_4=if_else(LkDensity >=0.18 & LkDensity <= 0.34,1,0)) %>%
  mutate(OF18_5=if_else(LkDensity >0.34,1,0)) %>%
  dplyr::select(WTLND_ID,OF18_1,OF18_2,OF18_3,OF18_4,OF18_5)

WriteXLS(OF18,file.path(dataOutDir,'OF18.xlsx'))

#Lake and Wetland Density within 2km of AA - OF19
WetIn<- Wetlands %>%
  dplyr::select(WTLND_ID,wet_id) %>%
  st_cast("POLYGON") %>%
  vect()

LakeIn<-FWA_lakes %>%
  #dplyr::select(lake_id) %>%
  mutate(LkArea=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(LkArea>0) %>%
  dplyr::select(LkArea) %>%
  st_cast("POLYGON") %>%
  vect()

#LkWet<-terra::union(LakeIn,WetIn) %>%
#  sf::st_as_sf()
#mapview(LkWet)+ mapview(FWA_lakes)+ mapview(Wetlands)
#write_sf(LkWet, file.path(spatialOutDir,"LkWet.gpkg"))
LkWet<-st_read(file.path(spatialOutDir,"LkWet.gpkg")) %>%
  dplyr::select(-c(WTLND_ID))

OF19<-LkWet %>%
  st_intersection(FWetlands2km) %>%
  mutate(LkWetArea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  #mutate(LareaHa=as.numeric(st_area(.))*0.001) %>%
  #write_sf(RoadsD,file.path(spatialOutDir,'RoadsD.gpkg'),overwrite=TRUE)
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(LakeWet_ha=sum(LkWetArea_Ha),Buff_area=first(Buffarea_Ha)) %>%
  mutate(LkWetDensity=LakeWet_ha/(Buff_area))  %>%
  mutate(OF19_1=if_else(LkWetDensity<0.02,1,0)) %>%
  mutate(OF19_2=if_else(LkWetDensity >=0.02 & LkWetDensity < 0.08,1,0)) %>%
  mutate(OF19_3=if_else(LkWetDensity >=0.08 & LkWetDensity < 0.18,1,0)) %>%
  mutate(OF19_4=if_else(LkWetDensity >=0.18 & LkWetDensity <= 0.34,1,0)) %>%
  mutate(OF19_5=if_else(LkWetDensity >0.34,1,0)) %>%
  dplyr::select(WTLND_ID,OF19_1,OF19_2,OF19_3,OF19_4,OF19_5)
WriteXLS(OF19,file.path(dataOutDir,'OF19.xlsx'))

#Road Density within 2km of AA - OF31
OF31<-roads %>%
  dplyr::select(RoadUse) %>%
  st_intersection(FWetlands2km) %>%
  #mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  mutate(rd_km=as.numeric(st_length(.))*0.001) %>%
  #write_sf(RoadsD,file.path(spatialOutDir,'RoadsD.gpkg'),overwrite=TRUE)
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(rd_km=sum(rd_km),B_area=first(Buffarea_Ha)) %>%
  full_join(WTLND_ID_List) %>%
  replace(is.na(.), 0) %>%
  mutate(Buff_area=if_else(B_area>0,(B_area-wetland_area_Ha),0)) %>%
  mutate(rdDensity_km_km2=rd_km/(Buff_area*0.01)) %>%
  mutate(OF31_1=if_else(rdDensity_km_km2<0.12,1,0)) %>%
  mutate(OF31_2=if_else(rdDensity_km_km2 >=0.12 & rdDensity_km_km2< 0.3,1,0)) %>%
  mutate(OF31_3=if_else(rdDensity_km_km2 >=0.3,1,0)) %>%
  dplyr::select(WTLND_ID,OF31_1,OF31_2,OF31_3)
WriteXLS(OF31,file.path(dataOutDir,'OF31.xlsx'))

#Intactness within 2km of AA - OF32
#Percent Disturbed in WAU - all non-road disturbance categories - OF41
disturbTbl<-terra::freq(Disturb)
#Reclassify disturbance to disturb and non-disturb
m<-c(0,0,1,
     1,99,2)
rclmat<-matrix(m,ncol=3, byrow=TRUE)
DisturbB<-classify(Disturb,rclmat,include.lowest=TRUE)
terra::freq(DisturbB)
Intact_2km.1<- terra::extract(DisturbB,vect(FWetlands2km),fun=table,na.rm=T,bind=TRUE)
colnames(Intact_2km.1)<-c('wet_id','intact_ha','disturbed_ha')
Intact_2km<-Intact_2km.1 %>%
  mutate(wet_id=as.integer(wet_id)) %>%
  left_join(FWetlands2km) %>%
  sf::st_drop_geometry()

OF32 <- Intact_2km %>%
  mutate(pc_intact=round(intact_ha/(disturbed_ha+intact_ha)*100,2)) %>%
  mutate(OF32_1=if_else(pc_intact<5,1,0)) %>%
  mutate(OF32_2=if_else(pc_intact >=5 & pc_intact< 30,1,0)) %>%
  mutate(OF32_3=if_else(pc_intact >=30 & pc_intact< 60,1,0)) %>%
  mutate(OF32_4=if_else(pc_intact >=60 & pc_intact< 90,1,0)) %>%
  mutate(OF32_5=if_else(pc_intact >=90,1,0)) %>%
  dplyr::select(WTLND_ID,OF32_1,OF32_2,OF32_3,OF32_4,OF32_5)
WriteXLS(OF32,file.path(dataOutDir,'OF32.xlsx'))

#Mature and Old within 2km - OF33
table(Old_GrowthSSP$TAP_CLASSIFICATION_LABEL)
colnames(WTLND_ID_List)<-c('WTLND_ID')
OF33<-Old_GrowthSSP %>%
  dplyr::select(TAP_CLASSIFICATION_LABEL) %>%
  mutate(OGMature=if_else(TAP_CLASSIFICATION_LABEL %in% c('Mature','Old'),1,0)) %>%
  mutate(EarlyMid=if_else(TAP_CLASSIFICATION_LABEL %in% c('Early','Mid'),1,0)) %>%
  mutate(OtherSeral=if_else((OGMature==0 & EarlyMid==0),1,0)) %>%
  #dplyr::filter(TAP_CLASSIFICATION_LABEL %in% c('Mature','Old')) %>%
  st_intersection(FWetlands2km) %>%
  mutate(OGMArea_Ha=OGMature*(as.numeric(st_area(.)*0.0001))) %>%
  mutate(EMidArea_Ha=EarlyMid*(as.numeric(st_area(.)*0.0001))) %>%
  mutate(ForestArea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(OGM_ha=sum(OGMArea_Ha),EMid_ha=sum(EMidArea_Ha),Other_ha=sum(OtherSeral),
                   Forest_ha=sum(ForestArea_Ha),Buff_area=first(Buffarea_Ha)) %>%
  #Need to ensure all wetlands are coded, including those outside of the crown forest area
  full_join(WTLND_ID_List) %>%
  replace(is.na(.), 0) %>%
  #Private land is not classed within the OG file, those units are set to 0, therefore OF33_1=1
  mutate(OGMDensityByForest=if_else(Forest_ha>0,OGM_ha/(Forest_ha*0.01),0)) %>%
  mutate(OGMDensityBy2kmTotal=if_else(Forest_ha>0,OGM_ha/(Buff_area*0.01),0)) %>%
  mutate(OF33_1=if_else(OGMDensityByForest<5,1,0)) %>%
  mutate(OF33_2=if_else(OGMDensityByForest >=5 & OGMDensityByForest< 30,1,0)) %>%
  mutate(OF33_3=if_else(OGMDensityByForest >=30 & OGMDensityByForest< 60,1,0)) %>%
  mutate(OF33_4=if_else(OGMDensityByForest >=60 & OGMDensityByForest< 90,1,0)) %>%
  mutate(OF33_5=if_else(OGMDensityByForest >=90,1,0)) %>%
  dplyr::select(WTLND_ID,OF33_1,OF33_2,OF33_3,OF33_4,OF33_5)

WriteXLS(OF33,file.path(dataOutDir,'OF33.xlsx'))

#Number of land cover types within 2k - OF37
LandCoverTbl<-terra::freq(LandCover)
#Count only non water units
#Drop 'Fresh Water' and 'Marine Water'
m<-c(0,0,1,
     1,99,2)
rclmat<-matrix(m,ncol=3, byrow=TRUE)
DisturbB<-classify(Disturb,rclmat,include.lowest=TRUE)
terra::freq(DisturbB)

LandCover_2kmE<- terra::extract(LandCover,vect(FWetlands2km),fun=table,na.rm=T,bind=TRUE)
LandCover_2km<-LandCover_2kmE %>%
  mutate(wet_id=as.integer(ID)) %>%
  dplyr::select(-c('ID','Fresh Water','Marine Water')) %>%
  mutate(LandCoverCount=(rowSums(.!=0))-1)

OF37 <- FWetlands2km %>%
  left_join(LandCover_2km) %>%
  st_drop_geometry()  %>%
  mutate(OF37_1=if_else(LandCoverCount<4,1,0)) %>%
  mutate(OF37_2=if_else(LandCoverCount==4 | LandCoverCount==5,1,0)) %>%
  mutate(OF37_3=if_else(LandCoverCount==6 | LandCoverCount==7,1,0)) %>%
  mutate(OF37_4=if_else(LandCoverCount>7 & LandCoverCount<=11,1,0)) %>%
  mutate(OF37_5=if_else(LandCoverCount>11,1,0)) %>%
  dplyr::select(WTLND_ID,OF37_1,OF37_2,OF37_3,OF37_4,OF37_5)
WriteXLS(OF37,file.path(dataOutDir,'OF37.xlsx'))


