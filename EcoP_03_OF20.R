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

#Questions - OF20 - species in wetland, watershed assessment unit and within 5km
#Get strata layers
FWA_ASS_WS<-st_read(file.path(spatialOutDir,"FWA_ASS_WS.gpkg"))
#clip out the 5km buffer where it is not in watershed
FWetlands5kmW<-st_read(file.path(spatialOutDir,"FWetlands5km.gpkg")) %>%
  st_intersection(FWA_ASS_WS) %>%
  group_by(WTLND_ID) %>%
  dplyr::summarise() %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001))
write_sf(FWetlands5kmW,file.path(spatialOutDir,'FWetlands5kmW.gpkg'))

  mapview(FWetlands5kmW)+mapview(FWetlands)+mapview(FWA_ASS_WS)

#Fish presence - OF20
WithinFn20<-function(wetStrat,idName,layer_in,lname){
  df<-wetStrat %>%
    st_filter(layer_in, .predicates=st_intersects) %>%
    st_drop_geometry() %>%
    mutate(!! (lname) :=1) %>%
    dplyr::select(!!(idName),!! (lname))

  #WriteXLS(df,file.path(dataOutDir,paste0(lname,'.xlsx')))
  return(df)
}

# get fish within wetland
#wet_Fish<-WithinFn20(FWetlands,Fish_Observe,'Fish_Observe')
wet_Fish_all<-Fish_Observe %>%
  mutate(Chinook=if_else((SPECIES_NAME=='Chinook Salmon'),1,0)) %>%
  mutate(Chum=if_else((SPECIES_NAME=='Chum Salmon'),1,0)) %>%
  mutate(Coho=if_else((SPECIES_NAME=='Coho Salmon'),1,0)) %>%
  mutate(Other=if_else(!SPECIES_NAME %in% c('Chinook Salmon',
                                            'Chum Salmon','Coho Salmon'),1,0)) %>%
  dplyr::select(FISH_OBSERVATION_POINT_ID,SPECIES_NAME,
                Chinook,Chum,Coho,Other,WATERBODY_TYPE)

#First do fish in wetland
wet_Fish_Chinook<-wet_Fish_all %>%
  dplyr::filter(Chinook==1) %>%
  WithinFn20(FWetlands,'WTLND_ID',.,'OF20_1.w') %>%
  st_drop_geometry()
wet_Fish_Chum<-wet_Fish_all %>%
  dplyr::filter(Chum==1) %>%
  WithinFn20(FWetlands,'WTLND_ID',.,'OF20_2.w') %>%
  st_drop_geometry()
wet_Fish_Coho<-wet_Fish_all %>%
  dplyr::filter(Coho==1) %>%
  WithinFn20(FWetlands,'WTLND_ID',.,'OF20_3.w') %>%
  st_drop_geometry()
wet_Fish_Other<-wet_Fish_all %>%
  dplyr::filter(Other==1) %>%
  WithinFn20(FWetlands,'WTLND_ID',.,'OF20_4.w') %>%
  st_drop_geometry()
wet_Fish_Fishless<-FWetlands %>%
  dplyr::filter(!WTLND_ID %in% c(wet_Fish_Chinook$WTLND_ID,wet_Fish_Chum$WTLND_ID,
                                 wet_Fish_Coho$WTLND_ID, wet_Fish_Other$WTLND_ID)) %>%
  mutate(OF20_5.w=1) %>%
  dplyr::select(WTLND_ID,OF20_5.w) %>%
  st_drop_geometry()
OF20.w<-FWetlands %>%
  sf::st_drop_geometry(data_all) %>%
  dplyr::select(WTLND_ID) %>%
  left_join(wet_Fish_Chinook) %>%
  left_join(wet_Fish_Chum) %>%
  left_join(wet_Fish_Coho) %>%
  left_join(wet_Fish_Other) %>%
  left_join(wet_Fish_Fishless) %>%
  replace(is.na(.), 0)

#Second do fish at 5km buffer around wetland
wet_Fish_Chinook5<-wet_Fish_all %>%
  dplyr::filter(Chinook==1) %>%
  WithinFn20(FWetlands5kmW,'WTLND_ID',.,'OF20_1.5') %>%
  st_drop_geometry()
wet_Fish_Chum5<-wet_Fish_all %>%
  dplyr::filter(Chum==1) %>%
  WithinFn20(FWetlands5kmW,'WTLND_ID',.,'OF20_2.5') %>%
  st_drop_geometry()
wet_Fish_Coho5<-wet_Fish_all %>%
  dplyr::filter(Coho==1) %>%
  WithinFn20(FWetlands5kmW,'WTLND_ID',.,'OF20_3.5') %>%
  st_drop_geometry()
wet_Fish_Other5<-wet_Fish_all %>%
  dplyr::filter(Other==1) %>%
  WithinFn20(FWetlands5kmW,'WTLND_ID',.,'OF20_4.5') %>%
  st_drop_geometry()
wet_Fish_Fishless5<-FWetlands5kmW %>%
  dplyr::filter(!WTLND_ID %in% c(wet_Fish_Chinook5$WTLND_ID,wet_Fish_Chum5$WTLND_ID,
                                 wet_Fish_Coho5$WTLND_ID, wet_Fish_Other5$WTLND_ID)) %>%
  mutate(OF20_5.5=1) %>%
  dplyr::select(WTLND_ID,OF20_5.5) %>%
  st_drop_geometry()

OF20.5<-FWetlands %>%
  sf::st_drop_geometry(data_all) %>%
  dplyr::select(WTLND_ID) %>%
  left_join(wet_Fish_Chinook5) %>%
  left_join(wet_Fish_Chum5) %>%
  left_join(wet_Fish_Coho5) %>%
  left_join(wet_Fish_Other5) %>%
  left_join(wet_Fish_Fishless5) %>%
  replace(is.na(.), 0)

#Third do fish within watershed
wet_Fish_ChinookW<-wet_Fish_all %>%
  dplyr::filter(Chinook==1) %>%
  WithinFn20(FWA_ASS_WS, 'ASS_WS_id',.,'OF20_1.ws') %>%
  st_drop_geometry()
wet_Fish_ChumW<-wet_Fish_all %>%
  dplyr::filter(Chum==1) %>%
  WithinFn20(FWA_ASS_WS, 'ASS_WS_id',.,'OF20_2.ws') %>%
  st_drop_geometry()
wet_Fish_CohoW<-wet_Fish_all %>%
  dplyr::filter(Coho==1) %>%
  WithinFn20(FWA_ASS_WS, 'ASS_WS_id',.,'OF20_3.ws') %>%
  st_drop_geometry()
wet_Fish_OtherW<-wet_Fish_all %>%
  dplyr::filter(Other==1) %>%
  WithinFn20(FWA_ASS_WS, 'ASS_WS_id',.,'OF20_4.ws') %>%
  st_drop_geometry()
wet_Fish_FishlessW<-FWA_ASS_WS %>%
  dplyr::filter(!ASS_WS_id %in% c(wet_Fish_ChinookW$ASS_WS_id,wet_Fish_ChumW$ASS_WS_id,
                                 wet_Fish_CohoW$ASS_WS_id, wet_Fish_OtherW$ASS_WS_id)) %>%
  mutate(OF20_5.ws=1) %>%
  dplyr::select(ASS_WS_id,OF20_5.ws) %>%
  st_drop_geometry()

OF20.W<-FWA_ASS_WS %>%
  sf::st_drop_geometry(data_all) %>%
  dplyr::select(ASS_WS_id) %>%
  left_join(wet_Fish_ChinookW) %>%
  left_join(wet_Fish_ChumW) %>%
  left_join(wet_Fish_CohoW) %>%
  left_join(wet_Fish_OtherW) %>%
  left_join(wet_Fish_FishlessW) %>%
  right_join(wet_WS) %>%
  replace(is.na(.), 0)

OF20check<-OF20.W %>%
  dplyr::filter(OF20_4.ws==1)

#Join together to make OF20 indicator
OF20<-FWetlands %>%
  sf::st_drop_geometry(data_all) %>%
  dplyr::select(WTLND_ID) %>%
  left_join(OF20.w) %>%
  left_join(OF20.5) %>%
  left_join(OF20.W) %>%
  mutate(
    OF20_1 := case_when(
      OF20_1.w==1 ~ 3,
      OF20_1.5==1 ~ 2,
      OF20_1.ws==1 ~ 1,
      TRUE ~ 0),
    OF20_2 := case_when(
      OF20_2.w==1 ~ 3,
      OF20_2.5==1 ~ 2,
      OF20_2.ws==1 ~ 1,
      TRUE ~ 0),
    OF20_3 := case_when(
      OF20_3.w==1 ~ 3,
      OF20_3.5==1 ~ 2,
      OF20_3.ws==1 ~ 1,
      TRUE ~ 0)) %>%
  mutate(
    OF20_4 := case_when(
        OF20_1==0 & OF20_2==0 & OF20_3==0 &
          (OF20_4.w==0 | OF20_4.5==0 | OF20_4.ws==0)  ~ 1,
        TRUE ~ 0),
    OF20_5 := case_when(
      OF20_1==0 & OF20_2==0 & OF20_3==0 &  OF20_4==0 ~ 1,
          TRUE ~ 0)) %>%
  replace(is.na(.), 0) %>%
  dplyr::select(WTLND_ID,OF20_1,OF20_2,OF20_3,OF20_4,OF20_5)
WriteXLS(OF20,file.path(dataOutDir,'OF20.xlsx'))


