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

#Questions - OF05, OF07, OF12, OF41, OF42, OF43
#Summarize data for eah FWA_ASS_WSHD - then assign to wetland within the FWA_ASS_WSHD

#Relative Elevation in Watershed - OF05
#Get the min and max elevation of the units ASS_WS
DEM_ASS_WS_max<- terra::extract(DEM.tp,FWA_ASS_WS,fun=max,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::rename(DEM_max=DEM_GD) %>%
  dplyr::select(ASS_WS_id,DEM_max)
DEM_ASS_WS_min<- terra::extract(DEM.tp,FWA_ASS_WS,fun=min,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::rename(DEM_min=DEM_GD) %>%
  dplyr::select(ASS_WS_id,DEM_min)
DEM_ASS_WS<- merge(DEM_ASS_WS_max,DEM_ASS_WS_min, by='ASS_WS_id') %>%
  mutate(DEM_range=DEM_max-DEM_min)

#Get wetland's elevation
wetDEM<- terra::extract(DEM.tp,FieldWet.pts,fun=mean,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::rename(wetDEM=DEM_GD) %>%
  dplyr::select(WTLND_ID,wetDEM) %>%
  left_join(wet_WS)

#Calculate the relative elevation of the surveyed wetlands
OF05 <- wetDEM %>%
  left_join(DEM_ASS_WS) %>%
  mutate(OF5_1=(wetDEM-DEM_min)/DEM_range) %>%
  dplyr::select(WTLND_ID,OF5_1)

WriteXLS(OF05,file.path(dataOutDir,'OF05.xlsx'))

#Aspect in wetland- OF07
#first get aspect of WSAU as a surrogate for contributing area
Aspect8<-terra::terrain(DEM.tp, v='aspect', unit='degrees',neighbors=8)
#writeRaster(Aspect, filename=file.path(spatialOutDir,paste0('Aspect_',WetlandAreaShort,'.tif')), overwrite=TRUE)
#writeRaster(Aspect8, filename=file.path(spatialOutDir,paste0('Aspect8_',WetlandAreaShort,'.tif')), overwrite=TRUE)

WS_DEM_asp<-terra::extract(rast(file.path(spatialOutDir,paste0('Aspect8_',WetlandAreaShort,'.tif'))),
                           FWA_ASS_WS,fun=mean,na.rm=T,bind=TRUE)
OF07<-wet_WS %>%
  left_join(as.data.frame(WS_DEM_asp)) %>%
  mutate(OF7_1=if_else(aspect>315 | aspect <45,1,0)) %>%
  mutate(OF7_2=if_else((aspect>=135 & aspect <=225),1,0)) %>%
  mutate(OF7_3=if_else((OF7_1 | OF7_2) == 0,1,0)) %>%
  dplyr::select(WTLND_ID,OF7_1,OF7_2,OF7_3)

WriteXLS(OF07,file.path(dataOutDir,'OF07.xlsx'))

#Wetland Density in WAU - sum of wetland area/WAU area - OF43
ASS_WS_Wetlands<- Wetlands %>%
  st_filter(FWA_ASS_WS, .predicates=st_intersects) %>%
  dplyr::select(WTLND_ID,wet_area_Ha=area_Ha)
#write_sf(ASS_WS_Wetlands, file.path(spatialOutDir,"ASS_WS_Wetlands.gpkg"))

WetlandDensity_ASS_WS<-Wetlands %>%
  st_intersection(FWA_ASS_WS) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,wet_area_Ha=area_Ha,ASS_WS_id,WS_area_Ha) %>%
  group_by(ASS_WS_id) %>%
  dplyr::summarize(wet_areaSUM=sum(wet_area_Ha), WS_area=first(WS_area_Ha)) %>%
  mutate(wetland_density=wet_areaSUM/WS_area)
#write_sf(WetlandDensity_ASS_WS, file.path(spatialOutDir,"WetlandDensity_ASS_WS.gpkg"))

#Assign wetland density to each of the surveyed wetlands
OF43 <- wet_WS %>%
  left_join(WetlandDensity_ASS_WS) %>%
  mutate(OF43_1=if_else(wetland_density<0.03,1,0)) %>%
  mutate(OF43_2=if_else(wetland_density >=0.03 & wetland_density<= 0.04,1,0)) %>%
  mutate(OF43_3=if_else(wetland_density >0.04 & wetland_density<=0.07,1,0)) %>%
  mutate(OF43_4=if_else(wetland_density >0.07 & wetland_density<= 0.13,1,0)) %>%
  mutate(OF43_5=if_else(wetland_density >0.13,1,0)) %>%
  dplyr::select(WTLND_ID,OF43_1,OF43_2,OF43_3,OF43_4,OF43_5)

WriteXLS(OF43,file.path(dataOutDir,'OF43.xlsx'))

#Road Density in WAU - all road - class 1, 2 and 3 - OF42
RoadsD<-roads %>%
  dplyr::select(RoadUse) %>%
  st_intersection(FWA_ASS_WS) %>%
  #mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  mutate(rd_km=as.numeric(st_length(.))*0.001) %>%
  #write_sf(RoadsD,file.path(spatialOutDir,'RoadsD.gpkg'),overwrite=TRUE)
  st_drop_geometry() %>%
  group_by(ASS_WS_id) %>%
  dplyr::summarize(rd_km=sum(rd_km),WS_area=first(WS_area_Ha)) %>%
  #multiply by 0.01 to convert ha to square km
  mutate(rdDensity_km_km2=rd_km/(WS_area*0.01))

OF42 <- wet_WS %>%
  left_join(RoadsD) %>%
  mutate(OF42_1=if_else(rdDensity_km_km2<0.4,1,0)) %>%
  mutate(OF42_2=if_else(rdDensity_km_km2 >=0.4 & rdDensity_km_km2<=1.2,1,0)) %>%
  mutate(OF42_3=if_else(rdDensity_km_km2 >1.2,1,0)) %>%
  dplyr::select(WTLND_ID,OF42_1,OF42_2,OF42_3)
WriteXLS(OF42,file.path(dataOutDir,'OF42.xlsx'))

#Percent Disturbed in WAU - all non-road disturbance categories - OF41
disturbTbl<-terra::freq(Disturb)
#Reclassify disturbance to disturb and non-disturb
m<-c(0,0,1,
     1,99,2)
rclmat<-matrix(m,ncol=3, byrow=TRUE)
DisturbB<-classify(Disturb,rclmat,include.lowest=TRUE)
terra::freq(DisturbB)
Disturb_ASS_WS<- terra::extract(DisturbB,vect(FWA_ASS_WS),fun=table,na.rm=T,bind=TRUE)
colnames(Disturb_ASS_WS)<-c('ASS_WS_id','intact_ha','disturbed_ha')
Disturb_ASS_WS<-Disturb_ASS_WS %>%
  mutate(ASS_WS_id=as.integer(ASS_WS_id))

OF41 <- wet_WS %>%
  left_join(Disturb_ASS_WS) %>%
  mutate(pc_disturb=round(disturbed_ha/(disturbed_ha+intact_ha)*100,2)) %>%
  #dplyr::filter(area_Ha==maxArea) %>%
  mutate(OF41_1=if_else(pc_disturb<10,1,0)) %>%
  mutate(OF41_2=if_else(pc_disturb >=10 & pc_disturb<=20,1,0)) %>%
  mutate(OF41_3=if_else(pc_disturb >20 & pc_disturb<=30,1,0)) %>%
  mutate(OF41_4=if_else(pc_disturb >30 & pc_disturb<=40,1,0)) %>%
  mutate(OF41_5=if_else(pc_disturb >40,1,0)) %>%
  dplyr::select(WTLND_ID,OF41_1,OF41_2,OF41_3,OF41_4,OF41_5)
WriteXLS(OF41,file.path(dataOutDir,'OF41.xlsx'))

#OF12 - Percent Unvegetated in WAU - all buildings, roads, parking lots, other pavement, or bare soil
unVeg<-rast(file.path(spatialOutDir,'unVeg.tif'))

unVeg_ASS_WS<- terra::extract(unVeg,vect(FWA_ASS_WS),fun=table,na.rm=T,bind=TRUE)
colnames(unVeg_ASS_WS)<-c('ASS_WS_id','veg_ha','unveg_ha')
unVeg_ASS_WS<-unVeg_ASS_WS %>%
  mutate(ASS_WS_id=as.integer(ASS_WS_id))

OF12 <- wet_WS %>%
  left_join(unVeg_ASS_WS) %>%
  mutate(pc_unveg=round(unveg_ha/(unveg_ha+veg_ha)*100,2)) %>%
  mutate(OF12_1=if_else(pc_unveg<10,1,0)) %>%
  mutate(OF12_2=if_else(pc_unveg >=10 & pc_unveg< 25,1,0)) %>%
  mutate(OF12_3=if_else(pc_unveg >=25,1,0)) %>%
  dplyr::select(WTLND_ID,OF12_1,OF12_2,OF12_3)
WriteXLS(OF12,file.path(dataOutDir,'OF12.xlsx'))


