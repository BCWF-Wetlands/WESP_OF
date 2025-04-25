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

#Check if lwgeom is connected to sf
#sf_extSoftVersion()["lwgeom"]

#Load generic 2024 Field Data
#run function to convert convert to sf and save to disk
#Depending on data format may need to modify source projection
Field2024Data.1<-gdbFn(field_gdb,layer_nm,out_name)
st_crs(Field2024Data.1)
#check colname to make sure they are labeled consistently
colnames(Field2024Data.1)
#Check Geometry
table(st_is_valid(Field2024Data.1))

#Clean projection and column names
Field2024Data.2<-Field2024Data.1 %>%
  #st_can_transform(Field2024Data.1,AOI)
  #st_set_crs(3857) %>%
  #st_transform(.,3005) %>%
  st_set_crs(3005) %>%
  mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
  mutate(Sampled=1) %>%
  #Clean geometry
  st_make_valid() %>%
  st_buffer(.,0) %>%
  dplyr::rename(WTLND_ID=Wtlnd_C) %>%
  dplyr::rename(YearSampled=YrSmpld)
#Make sure geometry column is labled properly
st_geometry(Field2024Data.2)<-'geom'

#Fix the geometry - make all POLYGONS
Field2024Data<-lapply(1:nrow(Field2024Data.2), function(i) {
  st_cast(Field2024Data.2[i, ], "POLYGON")
}) %>%
  do.call(rbind, .)
#Ensure geometry is clean
table(st_is_valid(Field2024Data))

#Last check sample fields
Check<-Field2024Data %>%
  #dplyr::filter(Sampled==1) %>%
  #mutate(WTLND_ID=Wtlnd_C) %>%
  dplyr::select(WTLND_ID,YearSampled,Sampled)
#Data check
table(Field2024Data.2$YearSampled)

write_sf(Field2024Data,file.path(spatialOutDir,paste0(WetlandAreaDir,"_2024Field.gpkg")))
