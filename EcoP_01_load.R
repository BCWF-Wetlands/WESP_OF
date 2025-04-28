# Copyright 2022 Province of British Columbia
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

#Fetch AOI, ASS_WS layer
bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)
ProvRast<-raster(file.path(spatialOutDirP,'ProvRast.tif'))

AOI<- st_read(file.path(spatialInDir,paste0(WetlandAreaShortL[EcoP],"_AOI.gpkg")))
AOIr<-rast(file.path(spatialInDir,'AOIr.tif'))
AOIbuff<-st_read(file.path(spatialInDir,paste0(WetlandAreaShortL[EcoP],"_AOIbuff.gpkg")))

Wetlands<-st_read(file.path(spatialInDir,'Wetlands.gpkg'))
Wetlands.dat<-Wetlands %>% st_drop_geometry()

#Check if Sampled wetlands are in Wetlands data base and that there are no duplicates
# and assign wet_id from Wetlands to Field2024Data
Wetlands_id_missing<-FWetlandsIn %>%
  dplyr::filter(!WTLND_ID %in% Wetlands.dat$WTLND_ID)
#If missing wetlands - process Field data using the WESP_data_prep scripts
Duplicate_Wetland_Co<-data.frame(Duplicates=FWetlandsIn[duplicated(FWetlandsIn$WTLND_ID),]$WTLND_ID)
#If clean then add wet_id from Wetlands to the field data
# and add a wetL_id - for linking
Wetlands_id<-Wetlands %>%
     st_drop_geometry() %>%
     dplyr::select(WTLND_ID,wet_id) %>%
     dplyr::filter(WTLND_ID %in% FWetlandsIn$WTLND_ID)
FWetlands<-FWetlandsIn %>%
  left_join(Wetlands_id, by='WTLND_ID') %>%
  mutate(wetL_id=as.numeric(rownames(.)))

#Re-read Wetlands - if it was modified
Wetlands<-st_read(file.path(spatialInDir,'Wetlands.gpkg'))
WetlandsB<-st_read(file.path(spatialInDir,'WetlandsB.gpkg'))
wetland.pt<-st_read(file.path(spatialInDir,'wetland.pt.gpkg'))

#Read in the clipped data from WESP_data_prep
DEM.tp<-rast(file.path(spatialInDir,paste0('DEMtp_',WetlandAreaShort,'.tif')))
Disturb<-rast(file.path(spatialInDir,'Disturb.tif'))
LandCover<-rast(file.path(spatialInDir,'LandCover.tif'))
FireR<-rast(file.path(spatialInDir,'FireR.tif'))
roadsSR<-rast(file.path(spatialInDir,'roadsSR.tif'))
roadsDist<-rast(file.path(spatialInDir,'roadsDist.tif'))
LandForm<-rast(file.path(spatialInDir,'LandForm.tif'))
Landforms_LUT<-read_xlsx(file.path(dataOutDirP,'Landforms_LUT.xlsx'))
#BedRock<-read_xlsx(file.path(dataOutDir,paste('BedRock.xlsx',sep='')))
dwell_R<-rast(file.path(spatialInDir,'dwell_R.tif'))
residence_R<-rast(file.path(spatialInDir,'residence_R.tif'))
VRI_SIR<-rast(file.path(spatialInDir,'VRI_SIR.tif'))

CDC_occur<-st_read(file.path(spatialInDir,'CDC_occur.gpkg'))
ConservationLands<-st_read(file.path(spatialInDir,'ConservationLands.gpkg')) %>%
  mutate(Cons_id=1:nrow(.))
FWA_ASS_WSin<-st_read(file.path(spatialInDir,"FWA_ASS_WS.gpkg"))
SARA<-st_read(file.path(spatialInDir,'SARA.gpkg'))
Old_GrowthSSP<-st_read(file.path(spatialInDir,'Old_GrowthSSP.gpkg'))
VRI<-st_read(file.path(spatialInDir,'VRI_raw.gpkg'))
roads<-st_read(file.path(spatialInDir,'roads_sf.gpkg'))
Fire2010<-st_read(file.path(spatialInDir,'Fire2010.gpkg'))
F_OWN<-st_read(file.path(spatialInDir,'F_OWN.gpkg'))
BEC<-st_read(file.path(spatialInDir,'BEC.gpkg'))
karst<-st_read(file.path(spatialInDir,'karst.gpkg'))

FWA_lakes<-st_read(file.path(spatialInDir,'FWA_lakes.gpkg'))
FWA_rivers<-st_read(file.path(spatialInDir,'FWA_rivers.gpkg'))
BGCprotected<-st_read(file.path(spatialInDir,'BGCprotected.gpkg'))
GeoF<-st_read(file.path(spatialInDir,'GeoF.gpkg'))
Fish_Observe<-st_read(file.path(spatialInDir,'Fish_Observe.gpkg'))
Old_Growth<-st_read(file.path(spatialInDir,'Old_Growth.gpkg'))
Streams<-st_read(file.path(spatialInDir,'Streams.gpkg'))
Erase_water<-st_read(file.path(spatialInDir,'Erase_water.gpkg'))

