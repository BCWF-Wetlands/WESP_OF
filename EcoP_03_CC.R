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

#Questions - OF25, OF26, OF27
#Get field wetland's centre point to feed climr

#Get the centroid of the wetlands
wetlandsXY <- st_centroid(FWetlands) %>%
  dplyr::select(WTLND_ID) %>%
  mutate(wet_id=as.numeric(rownames(.)))

#ClimateBC wants elevation so extract from DEM at wetland centroids
#and transform geometry to the crs expected by ClimateBC
wetlandsXYDEM.1 <- DEM.tp %>%
  terra::extract(wetlandsXY,xy=TRUE)
wetlandsXYDEM <- wetlandsXY %>%
  left_join(wetlandsXYDEM.1, join_by(wet_id==ID)) %>%
  `st_crs<-`(3005) %>%
  st_transform(crs=4326)

#Get x, y coordinates - long and lat
wetpt <- wetlandsXYDEM %>%
  cbind(st_coordinates(wetlandsXYDEM)) %>%
  st_drop_geometry() %>%
  dplyr::select(ID1=wet_id, ID2=WTLND_ID, Lat=Y, Long=X, Elev=DEM_SIM)

write_csv(wetpt, path=file.path(dataOutDir, 'SIMwetpt.csv'))
#After write copy the data into an email and send to windows and ensure
# no blank lines, and use Windows CR
# externally need to take file out and input to climatebc (external process)

OF_25_26_27<-read_csv(file.path(dataOutDir,'CC/SIMwetpt_Normal_1961_1990Y.csv')) %>%
  dplyr::rename(wet_id=ID1) %>%
  dplyr::rename(WTLND_ID=ID2) %>%
  mutate(Rad=Rad_sp+Rad_sm) %>%
  dplyr::select(WTLND_ID, OF25_1=CMD, OF26_1=DD5, OF27_1=Rad)

WriteXLS(OF_25_26_27,file.path(dataOutDir,paste0('OF_25_26_27.xlsx')))

message('Breaking')
break

#######################


#Special section to fix GD radiation errors
### Read GD file and filter out the bad Rad wetlands
wetptFix<-read_csv(file.path(OutDir,'data/GD_Base/CC/GDwetpt5_Normal_1961_1990MSY2.csv')) %>%
  dplyr::select(ID1, ID2, lat=Latitude, Long=Longitude, el=Elevation, Rad_sm) %>%
  dplyr::filter(Rad_sm==-9999.0)

#Manually enter the Lat, Lon, Elv into climateBC and find closes/most similar
# location that generates a climateBC radiation value - appears that climateBC
# cant handle locations on the boundary in south west BC, however it does capture areas within a 1km of the wetland
#GD_24993, use 122.89 Long to get closest place with solar radiation
# Rad_sp=14.7, Rad_sm=20, Rad=34.7
#GD_22088, use 123.4 Long to get closest place with solar radiation
# Rad_sp=14.3, Rad_sm=19.6, Rad=33.9

#Climate BC - now works, however it does not output all climateBC metrics
# most importantly solar radiation is not included
# https://bcgov.github.io/climr/articles/climr_with_rasters.html
remotes::install_github("bcgov/climr")
library(climr)
library(data.table)

list_gcms()
list_gcm_periods()
list_ssps()
list_vars()

#query data
ds_out <- downscale(
  xyz = wetpt, which_refmap = "auto",
  gcms = c("GFDL-ESM4", "EC-Earth3"), # specify two global climate models
  ssps = c("ssp370", "ssp245"), # specify two middle greenhouse gas concentration scenarios
  gcm_periods = c("2021_2040"), # specify a time periods
  max_run = 3, # specify 3 individual runs for each model
  vars = c("DD5", "CMD", "Rad_sm","Rad_sp") #RAD not available
)

OF_25_26_27<-ds_out%>%
  dplyr::rename(wet_id=ID1) %>%
  dplyr::rename(WTLND_ID=ID2) %>%
  mutate(Rad=Rad_sp+Rad_sm) %>%
  dplyr::select(WTLND_ID, OF25_1=CMD, OF26_1=DD5, OF27_1=Rad)

WriteXLS(OF_25_26_27,file.path(dataOutDir,paste0('OF_25_26_27.xlsx')))


