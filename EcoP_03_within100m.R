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

#Questions - OF30, OF36, OF38, OF39, OF40

#Road Density within 100m of AA - OF30
OF30<-roads %>%
  dplyr::select(RoadUse) %>%
  st_intersection(FWetlands100m) %>%
  mutate(rd_km=as.numeric(st_length(.))*0.001) %>%
  #write_sf(RoadsD,file.path(spatialOutDir,'RoadsD.gpkg'),overwrite=TRUE)
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(rd_km=sum(rd_km),B_area=first(area_Ha)) %>%
  full_join(WTLND_ID_List) %>%
  replace(is.na(.), 0) %>%
  #Make road density the buffer area, ie do not include wetland area
  mutate(Buff_area=if_else(B_area>0,B_area-wetland_area_Ha,0)) %>%
  mutate(rdDensity_km_km2=if_else(Buff_area>0,rd_km/(Buff_area*0.01),0)) %>%
  mutate(OF30_1=if_else(rdDensity_km_km2<0.12,1,0)) %>%
  mutate(OF30_2=if_else(rdDensity_km_km2 >=0.12 & rdDensity_km_km2< 0.3,1,0)) %>%
  mutate(OF30_3=if_else(rdDensity_km_km2 >=0.3,1,0)) %>%
  dplyr::select(WTLND_ID,OF30_1,OF30_2,OF30_3)
WriteXLS(OF30,file.path(dataOutDir,'OF30.xlsx'))
#mapview(FWetlands)+mapview(FWetlands100m)+mapview(roads)

#Number of land cover types within 100m - OF36
#Value	LTypeCode	LandCover
#0	1	Barren Land
#1	2	Coniferous Forest
#2	7	Cropland
#3	3	Deciduous Forest
#4	9	Estuary
#5	9	Fresh Water
#6	6	Grassland
#7	9	Intertidal Zone
#8	9	Marine Water
#9	4	Mixed Forest
#10	2	Riparian - Coniferous Forest
#11	3	Riparian - Deciduous Forest
#12	6	Riparian - Grassland
#13	4	Riparian - Mixed Forest
#14	5	Riparian - Shrubland
#15	5	Shrubland
#16	8	Snow and Ice
#17	9	Subtidal Zone
#18	10	Unclassified
#19	1	Urban and Built-up
#20	9	Wetland

#First prep land cover
LandCoverTbl<-terra::freq(LandCover)
LandCovL<-LandCoverTbl %>%
  dplyr::select(value)
#Use extract to build table of wetlands and cover
LandCover_100mE<- terra::extract(LandCover,vect(FWetlands100m),fun=table,na.rm=T,bind=TRUE) %>%
  mutate(wet_id=as.integer(ID))
#multiply table by 0.002 to translate 20mx20m to hectares
sqm_to_ha<-res(LandCover)[1]*res(LandCover)[2]*0.0001
LandCover_100m <- sqm_to_ha*(LandCover_100mE[LandCovL$value])
LandCover_100m$wet_id=as.numeric(rownames(LandCover_100m))
wet_LandCover_100m <- FWetlands100m %>%
  left_join(LandCover_100m) %>%
  mutate(area_Ha100m=as.numeric(st_area(.)*0.0001)) %>%
  rowwise() %>%
  mutate(areaLandCovL=sum(across(LandCovL$value)),na.rm=T)

LandCoverCount_100m1<-LandCover_100m %>%
  dplyr::select(-c('Fresh Water','Marine Water')) %>%
  mutate(LandCoverCount=(rowSums(.!=0))-1)

OF36 <- FWetlands100m %>%
  left_join(LandCoverCount_100m1) %>%
  st_drop_geometry() %>%
  mutate(OF36_0=if_else(LandCoverCount==0,1,0)) %>%
  mutate(OF36_1=if_else(LandCoverCount==1,1,0)) %>%
  mutate(OF36_2=if_else(LandCoverCount==2,1,0)) %>%
  mutate(OF36_3=if_else(LandCoverCount==3,1,0)) %>%
  mutate(OF36_4=if_else(LandCoverCount>=4,1,0)) %>%
  dplyr::select(WTLND_ID,OF36_1,OF36_2,OF36_3,OF36_4)

WriteXLS(OF36,file.path(dataOutDir,'OF36.xlsx'))

#Deciduous within 100m - OF38
wet_100mIn <- FWetlands100m %>%
  left_join(LandCover_100m) %>%
  mutate(area_Ha100m=as.numeric(st_area(.)*0.0001)) %>%
  rowwise() %>%
  mutate(areaLandCovL=sum(across(LandCovL$value)),na.rm=T) %>%
  mutate(areaNoWater=areaLandCovL-(get('Fresh Water')+get('Marine Water')))

OF38 <- wet_100mIn %>%
  mutate(DecidProp=get('Deciduous Forest')/areaNoWater) %>%
  st_drop_geometry() %>%
  mutate(OF38_1=if_else(DecidProp<0.06,1,0)) %>%
  mutate(OF38_2=if_else(DecidProp >=0.06 & DecidProp< 0.2,1,0)) %>%
  mutate(OF38_3=if_else(DecidProp >=0.2 & DecidProp< 0.4,1,0)) %>%
  mutate(OF38_4=if_else(DecidProp >=0.4 & DecidProp< 0.66,1,0)) %>%
  mutate(OF38_5=if_else(DecidProp >=0.66,1,0)) %>%
  dplyr::select(WTLND_ID,OF38_1,OF38_2,OF38_3,OF38_4,OF38_5)

WriteXLS(OF38,file.path(dataOutDir,'OF38.xlsx'))

#Coniferous within 100m - OF39
OF39 <- wet_100mIn %>%
  mutate(ConifProp=get('Coniferous Forest')/areaNoWater) %>%
  st_drop_geometry() %>%
  mutate(OF39_1=if_else(ConifProp<0.2,1,0)) %>%
  mutate(OF39_2=if_else(ConifProp >=0.2 & ConifProp<= 0.47,1,0)) %>%
  mutate(OF39_3=if_else(ConifProp >0.47 & ConifProp<= 0.69,1,0)) %>%
  mutate(OF39_4=if_else(ConifProp >0.69 & ConifProp<= 0.9,1,0)) %>%
  mutate(OF39_5=if_else(ConifProp >0.9,1,0)) %>%
  dplyr::select(WTLND_ID,OF39_1,OF39_2,OF39_3,OF39_4,OF39_5)
WriteXLS(OF39,file.path(dataOutDir,'OF39.xlsx'))

#Non-tree veg within 100m - OF40
OF40 <- wet_100mIn %>%
  mutate(NonTreeProp=(areaNoWater-(get('Coniferous Forest')+get('Deciduous Forest')+get('Mixed Forest')+
                                    get('Riparian - Coniferous Forest')+get('Riparian - Deciduous Forest')+
                                     get('Riparian - Mixed Forest')))/areaNoWater) %>%
  st_drop_geometry() %>%
  mutate(OF40_1=if_else(NonTreeProp<=0.09,1,0)) %>%
  mutate(OF40_2=if_else(NonTreeProp >0.09 & NonTreeProp<= 0.24,1,0)) %>%
  mutate(OF40_3=if_else(NonTreeProp >0.24 & NonTreeProp<= 0.44,1,0)) %>%
  mutate(OF40_4=if_else(NonTreeProp >0.44 & NonTreeProp<= 0.74,1,0)) %>%
  mutate(OF40_5=if_else(NonTreeProp >0.74,1,0)) %>%
  dplyr::select(WTLND_ID,OF40_1,OF40_2,OF40_3,OF40_4,OF40_5)
WriteXLS(OF40,file.path(dataOutDir,'OF40.xlsx'))


