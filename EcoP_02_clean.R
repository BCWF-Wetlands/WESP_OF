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

#Generate AOI based on field wetlands for office questions data clipping
AOIwbuff <- Field2024 %>%
  st_union() %>%
  st_as_sf() %>%
  st_buffer(dist=2000)
#Identify all the FWA assessment watersheds that are within or touch the EcoProvince boundary and dissolve
AOIwWS<- FWA_ASS_WSin %>%
  st_filter(Field2024, .predicates=st_intersects) %>%
  mutate(AOI=1) %>%
  dplyr::group_by(AOI) %>%
  dplyr::summarize()
AOIw<-AOIwWS %>%
  st_union(AOIwbuff) %>%
  st_cast('POLYGON') %>%
  st_as_sf() %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(area_Ha>1250)
#Check output and write
mapview(AOIwbuff)+mapview(AOIwWS)+mapview(AOIw)+mapview(Field2024)
write_sf(AOIw, file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOIw.gpkg")))
AOIw<-st_read(file.path(spatialOutDir,paste0(WetlandAreaShortL[EcoP],"_AOIw.gpkg")))

#pull those assessment watersheds that the Field data belongs to - for answering office questions
FWA_ASS_WS<-AOIwWS %>%
  st_union() %>%
  st_cast('POLYGON') %>%
  st_as_sf() %>%
  st_filter(Field2024, .predicates=st_intersects) %>%
  st_intersection(FWA_ASS_WSin) %>%
  mutate(WS_area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::filter(WS_area_Ha>1) %>%
  mutate(ASS_WS_id=1:nrow(.)) %>%
  dplyr::select(id,ASS_WS_id,WS_area_Ha)

mapview(Field2024)+mapview(FWA_ASS_WS)+mapview(AOIw)
write_sf(FWA_ASS_WS, file.path(spatialOutDir,'FWA_ASS_WS.gpkg'))

#Identify FWA assessment watersheds that a wetland is within - if it is in multiple watersheds then chose the largest overlap
wet_WS<-FWetlands %>%
  st_intersection(FWA_ASS_WS) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  st_drop_geometry() %>%
  group_by(WTLND_ID) %>%
  filter(area_Ha==max(area_Ha)) %>%
  dplyr::select(wet_id,WTLND_ID,ASS_WS_id)

FWetlands100m<-FWetlands %>%
  st_buffer(dist=100) %>%
  mutate(wetland_area_Ha=area_Ha) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  #mutate(wet_id=as.numeric(rownames(.))) %>%
  #dplyr::select(wet_id,WTLND_ID,wetland_area_Ha,area_Ha)
  dplyr::select(wet_id,WTLND_ID,wetland_area_Ha,area_Ha)
write_sf(FWetlands100m, file.path(spatialOutDir,"FWetlands100m.gpkg"))

FWetlands1km<-FWetlands %>%
  st_buffer(dist=1000) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  dplyr::select(wet_id,WTLND_ID,area_Ha)
write_sf(FWetlands1km, file.path(spatialOutDir,"FWetlands1km.gpkg"))

FWetlands5km<-FWetlands %>%
  st_buffer(dist=5000) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::select(wet_id,WTLND_ID,area_Ha)
write_sf(FWetlands5km, file.path(spatialOutDir,"FWetlands5km.gpkg"))

#Generate 2km assessment area
FWetlands2km<-FWetlands %>%
  st_buffer(dist=2000) %>%
  mutate(Buffarea_Ha=as.numeric(st_area(.)*0.0001)) %>%
  dplyr::select(wet_id,WTLND_ID,Buffarea_Ha)
write_sf(FWetlands2km, file.path(spatialOutDir,"FWetlands2km.gpkg"))

#mapview(FWetlands)+mapview(FWetlands2km)+mapview(FWA_lakes)

WTLND_ID_List<-FWetlands %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,wetland_area_Ha=area_Ha)


#For OF03 and OF12 - Percent Unvegetated in WAU - all buildings, roads,
# parking lots, other pavement, or bare soil
#0 not disturbed
#1 for historic Fire <=1990
#2 for recent historic Fire <=2015 & >=1991
#3 for historic cutblocks
#4 rangeland
#5 for recent fire - >2016
#6 Low use Roads
#7 for current cutblocks
#8 for agriculture and rural and recreation
#9 Mod use Roads
#10 ROW, Power, Rail, OGC geophysical
#11 High use Roads
#12 Mining, OGC infrastructure
#13 Urban
disturbTbl<-terra::freq(Disturb)
#Unveg=6,9,10,11,12,13
rdsTbl<-terra::freq(roadsSR)

#Reclassify disturbance to unveg/veg
m<-c(0,8,0,
     9,99,1)
rclmat<-matrix(m,ncol=3, byrow=TRUE)
UnvegV<-classify(Disturb,rclmat,include.lowest=TRUE, others=0)
UnvegV<-subst(UnvegV, NA, 0)
terra::freq(UnvegV)
writeRaster(UnvegV, filename=file.path(spatialOutDir,'UnvegV.tif'), overwrite=TRUE)

#add roads
RdUnveg<-roadsSR>0
RdUnveg<-subst(RdUnveg, NA, 0)
terra::freq(RdUnveg)
writeRaster(RdUnveg, filename=file.path(spatialOutDir,'RdUnveg.tif'), overwrite=TRUE)

unVeg<-(UnvegV+RdUnveg)>0
unVeg<-subst(unVeg, NA, 0)
terra::freq(unVeg)
writeRaster(unVeg, filename=file.path(spatialOutDir,'unVeg.tif'), overwrite=TRUE)

FieldWet.pts<-wetland.pt %>%
  dplyr::filter(WTLND_ID %in% Field2024$WTLND_ID) %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  dplyr::select(WTLND_ID,wet_id)

#Ponded Water
#Identify all ponded water
#LakesR<-fasterize(FWA_lakes, ProvRast, field="water",background=0)
#writeRaster(LakesR, file.path(spatialOutDir,'LakesR.tif'), overwrite=TRUE)

#Identify all ponded water not touching wetlands - move to Office
#put a small buffer around lakes to address slivers, etc
Lakes_file<-file.path(spatialOutDir,'LakesDontTouchD.tif')
  if (!file.exists(Lakes_file)) {
lakesB <- FWA_lakes %>%
  st_buffer(dist=5)
#identify wetlands that do not touch lakes - Terra failed, used raster but slower
lakesWet<-st_intersection(Wetlands, lakesB)
lakesThatDontTouch<-FWA_lakes %>%
  dplyr::filter(!lake_id %in% lakesWet$lake_id) %>%
  st_buffer(dist=0) %>%
  dplyr::select(water)

LakesDontTouchR<-fasterize(lakesThatDontTouch, ProvRast, field="water")
writeRaster(LakesDontTouchR, file.path(spatialOutDir,'LakesDontTouchR.tif'), overwrite=TRUE)
LakesDontTouchD<-raster::gridDistance(LakesDontTouchR,origin=1)
writeRaster(LakesDontTouchD, file.path(spatialOutDir,'LakesDontTouchD.tif'), overwrite=TRUE)

lakesThatDontTouchGT8 <- lakesThatDontTouch %>%
  mutate(areaHa=as.numeric(st_area(.)*0.0001)) %>%
  filter(areaHa>8)

LakesDontTouchGT8R<-fasterize(lakesThatDontTouchGT8, ProvRast, field="water")
LakesDontTouchGT8D<-raster::gridDistance(LakesDontTouchGT8R,origin=1)
#plot(LakesDontTouchD)
writeRaster(LakesDontTouchGT8D, file.path(spatialOutDir,'LakesDontTouchGT8D.tif'), overwrite=TRUE)

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

LkWet<-terra::union(LakeIn,WetIn) %>%
  sf::st_as_sf()
#mapview(LkWet)+ mapview(FWA_lakes)+ mapview(Wetlands)
write_sf(LkWet, file.path(spatialOutDir,"LkWet.gpkg"))

} else {
LakesDontTouchD<-rast(file.path(spatialOutDir,'LakesDontTouchD.tif'))
LakesDontTouchGT8D<-rast(file.path(spatialOutDir,'LakesDontTouchGT8D.tif'))
LkWet<-st_read(file.path(spatialOutDir,"LkWet.gpkg"))
}


message('Breaking')
break

#######################



#Combine human disturbance + fires
#human disturbed from AreaDisturbance_LUT:
#0 not disturbed
#1 for historic Fire <=1990
#2 for recent historic Fire <=2015 & >=1991
#3 for historic cutblocks
#4 rangeland
#5 for recent fire - >2016
#6 Low use Roads
#7 for current cutblocks
#8 for agriculture and rural and recreation
#9 Mod use Roads
#10 ROW, Power, Rail, OGC geophysical
#11 High use Roads
#12 Mining, OGC infrastructure
#13 Urban

#Assign values to roads
#Roads_LUT<-data.frame(Value=c(1,2,3), Rd_Disturb_Code=c(11,9,6))

#roadsDist <- raster::subs(roadsSR, Roads_LUT, by='Value', which='Rd_Disturb_Code')

#Disturb_LUT<-data.frame(DisturbCode=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
#                        DisturbType=c('not disturbed', 'historic fire','recent fire',
#                                      'historic cutblocks','rangeland','current fire',
#                                      'low use roads','current cutblocks',
#                                      'agriculture and rural and recreation',
#                                      'mod use roads', 'ROW, Power, Rail, OGC geophysical',
#                                      'high use roads','mine, OGC infrastructure','urban'))
Disturb_LUT<-data.frame(DisturbCode=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                        DisturbType=c('not disturbed', 'historic fire','recent fire',
                                      'historic cutblocks','rangeland','current fire',
                                      'low use roads','current cutblocks',
                                      'agriculture and rural and recreation',
                                      'mod use roads', 'ROW, Power, Rail, OGC geophysical',
                                      'high use roads','mine, OGC infrastructure','urban'))
saveRDS(Disturb_LUT,file='tmp/Disturb_LUT')

#Make a raster stack of disturbance layers
#disturbStackL <- list(disturbance_sfR,FireR,roadsDist)
disturbStackL <- list(disturbance_sfR,FireR)
disturbStack <- stack(disturbStackL)
LandDisturb<-max(disturbStack, na.rm=TRUE)
#LandDisturbP<-max(disturbance_sfR, FireR, na.rm=TRUE)
writeRaster(LandDisturb,filename=file.path(spatialOutDir,"LandDisturb.tif"), format="GTiff", overwrite=TRUE)
LandDisturb<-raster(file.path(spatialOutDir,"LandDisturb.tif"))


