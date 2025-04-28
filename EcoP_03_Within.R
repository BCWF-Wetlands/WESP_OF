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

#Questions - OF16, OF17, OF23, OF28, OF29
#Distance to Questions - OF01

#FWetlands<-Field2023Data

#OF01 - Distance to Community
#wetland distance to >5 dwellings per square km - in meters
# residence_R - is a raster surface of human density from stats CDN - see WESP_data_prep for layer details
OF01<-terra::extract(residence_R,FWetlands,fun=min,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  mutate(OF1_1=if_else(residence<=100,1,0)) %>%
  mutate(OF1_2=if_else(residence >100 & residence< 501,1,0)) %>%
  mutate(OF1_3=if_else(residence >501 & residence< 1001,1,0)) %>%
  mutate(OF1_4=if_else(residence >1001 & residence< 5001,1,0)) %>%
  mutate(OF1_5=if_else(residence >5001,1,0)) %>%
  dplyr::select(WTLND_ID,OF1_1,OF1_2,OF1_3,OF1_4,OF1_5)

WriteXLS(OF01,file.path(dataOutDir,'OF01.xlsx'))


#Karst - significant, based on 'KARST_DEVELOPMENT_INTENSITY' - OF16
Karstin<-st_read(file.path(spatialOutDirP,'KarstP.gpkg')) %>%
  st_intersection(AOIw)
OF16<-Karstin %>%
  mutate(KarstConfidence=str_extract(substr(KARST_DEVELOPMENT_CONFIDENCE,1,2),'[[:digit:]]+')) %>%
  dplyr::select(KarstConfidence,KARST_LIKELIHOOD,KARST_DEVELOPMENT_INTENSITY) %>%
  dplyr::filter(KARST_DEVELOPMENT_INTENSITY %in% c('H','M')) #drop L
wet_Karst<-WithinFn(OF16,'OF16')


#GeoFP - all polygons are faults - OF17
OF17<-st_read(file.path(spatialOutDirP,'GeoFP.gpkg')) %>%
  st_intersection(AOIw)
wet_GeoF<-WithinFn(OF17,'OF17')

#BGC Protected - OF23
BGCprotectedin<-st_read(file.path(spatialOutDirP,'BGCprotectedP.gpkg')) %>%
  st_intersection(AOIw)
#need to assign dominant BGC to wetland, then what use that % unit protected
OF23<-FWetlands %>%
  st_intersection(BGCprotectedin) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,area_Ha,PCTBCPR) %>%
  group_by(WTLND_ID) %>%
  mutate(maxArea=max(area_Ha)) %>%
  ungroup %>%
  dplyr::filter(area_Ha==maxArea) %>%
  mutate(OF23_1=if_else(PCTBCPR<=1,1,0)) %>%
  mutate(OF23_2=if_else(PCTBCPR >1 & PCTBCPR<= 4,1,0)) %>%
  mutate(OF23_3=if_else(PCTBCPR >4 & PCTBCPR<= 8,1,0)) %>%
  mutate(OF23_4=if_else(PCTBCPR >8 & PCTBCPR<= 12,1,0)) %>%
  mutate(OF23_5=if_else(PCTBCPR >12 & PCTBCPR<= 20,1,0)) %>%
  mutate(OF23_6=if_else(PCTBCPR >20,1,0)) %>%
  dplyr::select(WTLND_ID,OF23_1,OF23_2,OF23_3,OF23_4,OF23_5,OF23_6)
WriteXLS(OF23,file.path(dataOutDir,paste0('OF23.xlsx')))

#OF28 - Site Index within
#Get mean SI -
# set na.rm=F so that NaN SI is picked up in wetlands where it is not typed
wet_SIE<-terra::extract(VRI_SIR,FWetlands,fun=mean,na.rm=F,weights=TRUE) %>%
  dplyr::rename(wetL_id=ID)
OF28<-FWetlands %>%
  mutate(wetL_id=as.numeric(rownames(.))) %>%
  st_drop_geometry() %>%
  mutate(wet_id=as.numeric(wetL_id)) %>%
  left_join(wet_SIE) %>%
  replace(is.na(.), 0) %>%
  mutate(OF28_1=if_else(VRI_SIR==0,0,0)) %>% #set 0 case to 0 for all cases
  mutate(OF28_1=if_else(VRI_SIR >0 & VRI_SIR<=6.3,1,0)) %>%
  mutate(OF28_2=if_else(VRI_SIR >6.3 & VRI_SIR<=9.5,1,0)) %>%
  mutate(OF28_3=if_else(VRI_SIR >9.5 & VRI_SIR<= 13.3,1,0)) %>%
  mutate(OF28_4=if_else(VRI_SIR >13.3 & VRI_SIR<=18.5,1,0)) %>%
  mutate(OF28_5=if_else(VRI_SIR >18.5,1,0)) %>%
  dplyr::select(WTLND_ID,OF28_1,OF28_2,OF28_3,OF28_4,OF28_5)

WriteXLS(OF28,file.path(dataOutDir,'OF28.xlsx'))

#Topographic Position - OF29
Top_pos<-terra::extract(LandForm,FWetlands,fun=max,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,Top_pos=landformP)

#Now assign to wetland
OF29<-wet_WS %>%
  left_join(Top_pos) %>%
  left_join(Landforms_LUT) %>%
  mutate(OF29_1=MesoCode) %>%
  dplyr::select(WTLND_ID,OF29_1)
WriteXLS(OF29,file.path(dataOutDir,'OF29.xlsx'))


##############
