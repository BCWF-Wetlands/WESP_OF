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

#Distance to Questions - OF02, OF03, OF04

#OF02 - Distance of wetland centre point (AA) to Frequently traveled roads
#Use only high use roads > 1 vehicle/minute - road class 1
roads02<-roads %>%
  dplyr::select(RoadUse) %>%
  dplyr::filter(RoadUse<2)
write_sf(roads02, file.path(spatialOutDir,"roads02.gpkg"))

#Use st_nearest_feature to find high use roads next wetland centre point
NearestRoad<-st_nearest_feature(FieldWet.pts,roads02)
DistToRoad<-as.data.frame(st_distance(FieldWet.pts, roads02$geom[NearestRoad], by_element = TRUE))
colnames(DistToRoad)<-c('DistToRoad')

OF02<-DistToRoad %>%
  mutate(wet_id=as.numeric(rownames(.))) %>%
  left_join(FieldWet.pts) %>%
  st_drop_geometry() %>%
  mutate(DistToRoad=as.numeric(DistToRoad)) %>%
  mutate(OF2_1=if_else(DistToRoad<10,1,0)) %>%
  mutate(OF2_2=if_else(DistToRoad >10.1 & DistToRoad< 25,1,0)) %>%
  mutate(OF2_3=if_else(DistToRoad >25.1 & DistToRoad< 50,1,0)) %>%
  mutate(OF2_4=if_else(DistToRoad >50.1 & DistToRoad< 100,1,0)) %>%
  mutate(OF2_5=if_else(DistToRoad >100.1 & DistToRoad< 500,1,0)) %>%
  mutate(OF2_6=if_else(DistToRoad >501,1,0)) %>%
  dplyr::select(WTLND_ID,OF2_1,OF2_2,OF2_3,OF2_4,OF2_5,OF2_6)

WriteXLS(OF02,file.path(dataOutDir,'OF02.xlsx'))

#Distance to Questions - OF03
#Clean up FWA_lakes
Lakes.1<-st_read(file.path(spatialInDir,'FWA_lakes.gpkg')) %>%
  dplyr::filter(st_is_valid(.)==TRUE) %>%
  st_make_valid() %>%
  st_buffer(dist=0) %>%
  mutate(water=0) %>%
  mutate(areaHa=as.numeric(st_area(.)*0.0001))

#FWA_lakes has some bad geometry, filter it out
Lakes.2<-Lakes.1 %>%
  dplyr::filter(!(is.na(st_dimension(.))))
#Check geometry
clgeo_IsValid(as(Lakes.2,'Spatial'), verbose = FALSE) #TRUE
Lakes.v<-vect(Lakes.2)
writeVector(Lakes.v, file.path(spatialOutDir,'Lakes.v.gpkg'), overwrite=TRUE)

#use touches=TRUE to capture all lakes even those that are not completely in a cell
Lakes.r<-terra::rasterize(Lakes.v,AOIr,field='water',touches=TRUE,background=1)
Lakes.r<-terra::rasterize(Lakes.v,rast(ProvRast),field='water',touches=TRUE,background=1) %>%
  terra::crop(AOIbuff)
writeRaster(Lakes.r, file.path(spatialOutDir,'Lakes.r.tif'), overwrite=TRUE)
Lakes.r<-rast(file.path(spatialOutDir,'Lakes.r.tif'))
#Modify raster values - change 0 to 1 and 1 to 2, then change 2 to 0
# raster then has 0 for lakes and 1 for matrix, set distance target to 0
# cost surface is distance from Lake in cell size
#Lakes.r.1 <- Lakes.r+1
#LakesR <- subst(Lakes.r.1, 2, 0)
LakesDistCost<-terra::costDist(Lakes.r, target=0, overwrite=TRUE)
writeRaster(LakesDistCost, file.path(spatialOutDir,'LakesDistCost.tif'), overwrite=TRUE)

#Get unVeg raster and make it 10 where unVeg occurs
unVeg<-rast(file.path(spatialOutDir,'unVeg.tif'))
unVeg10<-unVeg*10

#Make Lakes raster >1 where unVeg10>1, Lakes stay 0,
# the matrix increases if unVeg10>0
LakesUnVeg<-Lakes.r*(Lakes.r+unVeg10)
writeRaster(LakesUnVeg, file.path(spatialOutDir,'LakesUnVeg.tif'), overwrite=TRUE)

#Cost surface to Lakes over raster which is 1 where OK and >1 if unVeg
LakesUnVegCost<-terra::costDist(LakesUnVeg, target=0)
writeRaster(LakesUnVegCost, file.path(spatialOutDir,'LakesUnVegCost.tif'), overwrite=TRUE)

#First get minimum distance to lakes
OF03.1<-terra::extract(LakesDistCost,FWetlands,fun=min,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,DistToPonded=water)
#Second get minimum distance to lakes across UnVeg cost surface
OF03.2<-terra::extract(LakesUnVegCost,FWetlands,fun=min,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,DistToPondedunVeg=water)
#Drop distance to lake==0 since connected
# identiy where distance to lakes is the same as UnVeg cost surface, ie.
# there is no cost between lake and wetland, therefore not seperated
OF03<-OF03.1 %>%
  left_join(OF03.2) %>%
  mutate(OF3_0=if_else(DistToPonded ==0,1,0)) %>% #was not included - check?
  mutate(OF3_1=if_else(DistToPonded >0 & DistToPonded< 101 & (DistToPondedunVeg==DistToPonded),1,0)) %>%
  mutate(OF3_2=if_else(DistToPonded >0 & DistToPonded< 101 & (DistToPondedunVeg>DistToPonded),1,0)) %>%
  mutate(OF3_3=if_else(DistToPonded >100 & DistToPonded< 1001 & (DistToPondedunVeg==DistToPonded),1,0)) %>%
  mutate(OF3_4=if_else(DistToPonded >100 & DistToPonded< 1001 & (DistToPondedunVeg>DistToPonded),1,0)) %>%
  mutate(OF3_5=if_else(DistToPonded >1000 & DistToPonded< 5001 & (DistToPondedunVeg==DistToPonded),1,0)) %>%
  mutate(OF3_6=if_else(DistToPonded >1000 & DistToPonded< 5001 & (DistToPondedunVeg>DistToPonded),1,0)) %>%
  mutate(OF3_7=if_else(DistToPonded >5000 | DistToPonded==0,1,0))

OF03.a<-OF03 %>%
  dplyr::select(WTLND_ID,OF3_1,OF3_2,OF3_3,OF3_4,OF3_5,OF3_6,OF3_7)

WriteXLS(OF03.a,file.path(dataOutDir,'OF03.xlsx'))

FieldWetPond <- FWetlands %>%
  left_join(OF03) %>%
  dplyr::select(wet_id,WTLND_ID,area_Ha,DistToPonded,DistToPondedunVeg,OF3_0,OF3_1,OF3_2,OF3_3,OF3_4,OF3_5,OF3_6,OF3_7)

write_sf(FieldWetPond, file.path(spatialOutDir,"FieldWetPond.gpkg"))

#OF04 - Distance to Ponded Water GT 8ha
Lakes.vGT8<-subset(Lakes.v,Lakes.v$areaHa>8)
writeVector(Lakes.vGT8, file.path(spatialOutDir,'Lakes.vGT8.gpkg'), overwrite=TRUE)

#use touches=TRUE to capture all lakes even thouse that are not completely in a cell
LakesGT8.r<-terra::rasterize(Lakes.vGT8,AOIr,field='water',touches=TRUE,background=1)
writeRaster(LakesGT8.r, file.path(spatialOutDir,'LakesGT8.r.tif'), overwrite=TRUE)

LakesGT8DistCost<-terra::costDist(LakesGT8.r, target=0, overwrite=TRUE)
writeRaster(LakesGT8DistCost, file.path(spatialOutDir,'LakesGT8DistCost.tif'), overwrite=TRUE)
LakesGT8DistCost<-rast(file.path(spatialOutDir,'LakesGT8DistCost.tif'))

OF04<-terra::extract(LakesGT8DistCost,FWetlands,fun=min,na.rm=T,bind=TRUE) %>%
  sf::st_as_sf() %>%
  st_drop_geometry() %>%
  dplyr::select(WTLND_ID,DistToPondedGT8=water) %>%
  #mutate(OF4_0=if_else(DistToPondedGT8 ==0,1,0)) %>%
  #mutate(OF4_1=if_else(DistToPondedGT8 >0 & DistToPondedGT8< 101,1,0)) %>%
  mutate(OF4_1=if_else(DistToPondedGT8< 101,1,0)) %>%
  mutate(OF4_2=if_else( DistToPondedGT8 >100 & DistToPondedGT8< 1001,1,0)) %>%
  mutate(OF4_3=if_else(DistToPondedGT8 >1000 & DistToPondedGT8< 2001,1,0)) %>%
  mutate(OF4_4=if_else(DistToPondedGT8 >2000 & DistToPondedGT8< 5001,1,0)) %>%
  mutate(OF4_5=if_else(DistToPondedGT8 >5000 & DistToPondedGT8< 10001,1,0)) %>%
  mutate(OF4_6=if_else(DistToPondedGT8 >10000,1,0)) %>%
  dplyr::select(WTLND_ID,OF4_1,OF4_2,OF4_3,OF4_4,OF4_5,OF4_6)

WriteXLS(OF04,file.path(dataOutDir,'OF04.xlsx'))

##########
