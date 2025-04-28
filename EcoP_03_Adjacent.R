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

#Questions - OF15, OF21, OF22, OF34 OF35

#OF15 - Burned since 2010-
# Fire2010 - bufferr wetland
#read in buffered Field Wetlands - buffer

#Find wetlands that intersect with fires since 2010
OF15<-FWetlands100m %>%
  st_filter(Fire2010, .predicates = st_intersects) %>%
  st_drop_geometry() %>%
  mutate(OF15=1) %>%
  dplyr::select(WTLND_ID,OF15)

WriteXLS(OF15,file.path(dataOutDir,'OF15.xlsx'))

#OF 34 - Land cover uniqueness within 1km and 5km
# land cover buffer around wetland - evaluate most rare element and evaluate if it occurs adjacent to wetland
#
Fn_34 <- function(FWet){
wet_commLandkmE<-terra::extract(LandCover,FWet,fun=table,na.rm=T,weights=TRUE) %>%
  dplyr::rename(wetL_id=ID)
#Pull out the most common landcover type and its area and calculate its proportion
#Make 0 and NA very large so not included in rowMins
wet_commLandkmE[is.na(wet_commLandkmE)]<-1e6
wet_commLandkmE[wet_commLandkmE==0]<-1e6

Wet_UncommLand.1 <- wet_commLandkmE %>%
  dplyr::select(-c('Wetland','Marine Water','Fresh Water'))
LTypes<-colnames(Wet_UncommLand.1)[2:length(Wet_UncommLand.1)]

Wet_UncommLand.2 <- Wet_UncommLand.1 %>%
  mutate(UncommLandA=rowMins(as.matrix((.[LTypes]) )))

df<-FWet %>%
  #mutate(wet_id=as.numeric(wet_id)) %>%
  left_join(Wet_UncommLand.2) %>%
  #get area of AA in same units as rast polygons - 20x20
  mutate(area_400m2=(area_Ha*10000)/400) %>%
  st_drop_geometry() %>%
  #calculate proportion of most dominant cover type
  mutate(OF34=UncommLandA/area_400m2*100) %>%
  dplyr::select(WTLND_ID,OF34) #%>%
  #dplyr::filter(OF34<1.0)
return(df)
}

OF34.1<-Fn_34(FWetlands1km)
OF34.2<- OF34.1 %>%
  dplyr::rename(OF34_1km=OF34)
OF34.3<- OF34.2 %>%
  left_join(Fn_34(FWetlands5km))
OF34<- OF34.3 %>%
  dplyr::rename(OF34_5km=OF34) %>%
  mutate(OF34_1=if_else(OF34_1km<=1,1,0)) %>%
  mutate(OF34_2=if_else(OF34_5km<=1 & OF34_1==0,1,0)) %>%
  mutate(OF34_3=if_else(OF34_1==0 & OF34_2==0,1,0)) %>%
  dplyr::select(WTLND_ID,OF34_1,OF34_2,OF34_3)
WriteXLS(OF34,file.path(dataOutDir,'OF34.xlsx'))

#OF 35 - most common cover land type adjacent to wetland
#Generate a table of land cover types within and adjacent to wetland
wet_commLandE<-terra::extract(LandCover,FWetlands100m,fun=table,na.rm=T,weights=TRUE) %>%
  dplyr::rename(wetL_id=ID)
#Pull out the most common landcover type and its area and calculate its proportion
LTypes<-colnames(wet_commLandE)[2:length(wet_commLandE)]
Wet_commLand.1 <- wet_commLandE %>%
  mutate(commLandC=LTypes[max.col(.[LTypes], ties.method='first')]) %>%
  mutate(commLandA=rowMaxs(as.matrix((.[LTypes]) )))

OF35<-FWetlands100m %>%
  #mutate(wet_id=as.numeric(wet_id)) %>%
  left_join(Wet_commLand.1) %>%
  #get area of AA in same units as rast polygons - 20x20
  mutate(area_400m2=(area_Ha*10000)/400) %>%
  st_drop_geometry() %>%
  #calculate proportion of most dominant cover type
  mutate(OF35_1=commLandA/area_400m2) %>%
  dplyr::select(WTLND_ID,OF35_1)

WriteXLS(OF35,file.path(dataOutDir,'OF35.xlsx'))

#OF22 Protection from Intensive Use
OF22<-FWetlands100m %>%
  sf::st_join(ConservationLands, join = st_intersects) %>%
  dplyr::filter(category %in% c('01_PPA','02_Protected_Other','03_Exclude_1_2_Activities')) %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(category=first(category)) %>%
  mutate(OF22=1) %>%
  st_drop_geometry()

WriteXLS(OF22,file.path(dataOutDir,'OF22.xlsx'))

#OF21 Ecological Designation
SARA_wet<-FWetlands100m %>%
  sf::st_join(SARA, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(COMMON_NAME_ENGLISH=first(COMMON_NAME_ENGLISH)) %>%
  mutate(OF21.SARA=if_else(is.na(COMMON_NAME_ENGLISH),0,1))
CDC_occur_wet<-FWetlands100m %>%
  sf::st_join(CDC_occur, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(ENG_NAME=first(ENG_NAME)) %>%
  mutate(OF21.CDC=if_else(is.na(ENG_NAME),0,1))
ConservationLands_wet<-FWetlands100m %>%
  sf::st_join(ConservationLands, join = st_intersects) %>%
  st_drop_geometry() %>%
  dplyr::filter(category %in% c('01_PPA','02_Protected_Other','03_Exclude_1_2_Activities','04_Managed')) %>%
  group_by(WTLND_ID) %>%
  dplyr::summarize(category=min(category)) %>%
  mutate(OF21.ConsLand=if_else(is.na(category),0,1))

OF21<-FWetlands100m %>%
  left_join(SARA_wet) %>%
  left_join(CDC_occur_wet) %>%
  left_join(ConservationLands_wet) %>%
  replace(is.na(.), 0) %>%
  mutate(OF21=if_else((OF21.SARA+OF21.CDC+OF21.ConsLand)>0,1,0)) %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID, OF21)

WriteXLS(OF21,file.path(dataOutDir,'OF21.xlsx'))

