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

devtools::install_github("BCWF-Wetlands/wespr", force=TRUE)
Sys.getenv("GITHUB_PAT")
Sys.unsetenv("GITHUB_PAT")

#Questions - OF25, OF26, OF27
#Get field wetland's centre point to feed climr
# https://bcgov.github.io/climr/articles/climr_with_rasters.html
remotes::install_github("bcgov/climr")
devtools::install_github("bcgov/climr")
library(climr)
library(data.table)


#add id, lon, lat, elev columns
xyzDT <- terra::extract(DEM.tp, Field.pt, xy = TRUE) |>
  as.data.table()
setnames(xyzDT, c("id", "elev", "lon", "lat"))

xyzDT <- terra::extract(DEM.tp, Field.pt, xy = TRUE) %>%
  as.data.table()
setnames(xyzDT, c("id", "elev", "lon", "lat"))


#data("xyzDT")
write_csv(xyzDT,file.path(dataOutDir,paste0(WetlandArea,'_CC.csv')))

#ClimateBC wants elevation so extract from DEM at wetland centroids
#and transform geometry to the crs expected by ClimateBC
DEM<-raster(DEM.tp)
wetlandsXY<-Field.pt
wetlandsXYDEM <- DEM %>%
  raster::extract(wetlandsXY) %>%
  cbind(wetlandsXY) %>%
  st_transform(crs=4326)

#Get x, y coordinates - long and lat
wetpt <- st_coordinates(wetlandsXYDEM)
wetpt <- wetlandsXYDEM %>%
  cbind(wetpt) %>%
  st_drop_geometry() %>%
  dplyr::select(ID1=wet_id, ID2=WTLND_ID, lat=Y, long=X, el='.')

#write file to csv - change line feed from unix style to windows otherwise ClimateBC errors
#MS-DOS csv
write_csv(wetpt, path=file.path(dataOutDir, 'GDwetpt5.csv'))
### Read file back in
OF_25_26_27<-read_csv(file.path(dataOutDir,'CC/GDwetpt5_Normal_1961_1990MSY2.csv')) %>%
  dplyr::rename(wet_id=ID1) %>%
  dplyr::rename(WTLND_ID=ID2) %>%
  mutate(Rad=Rad_sp+Rad_sm) %>%
  dplyr::select(WTLND_ID, OF25_1=CMD, OF26_1=DD5, OF27_1=Rad)

WriteXLS(OF_25_26_27,file.path(dataOutDir,paste0('OF_25_26_27.xlsx')))


##########From ESI and RR

source ('header.R')

#ClimateBC
# ASC file type methods
#Write a smaller sub set as a test
ws_AOI <- readRDS(file = 'tmp/ws') %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")

DEM_cBC <-DEM %>%
  crop(ws_AOI) %>%
  projectRaster(crs=CRS("+init=epsg:4326"))

rgdal::writeGDAL(as(DEM_cBC, "SpatialGridDataFrame"),
                 file.path(spatialOutDir,"DEMt_cBC.asc"),
                 drivername = "AAIGrid", mvFlag=-9999)

#Method 2 make a csv of the lat, lons of each wetland
#Get the centroid of the wetlands
wetlandsXY <- st_centroid(Wetlands)

#ClimateBC wants elevation so extract from DEM at wetland centroids
#and transform geometry to the crs expected by ClimateBC
wetlandsXYDEM <- DEM %>%
  raster::extract(wetlandsXY) %>%
  cbind(wetlandsXY) %>%
  st_transform(crs=4326)

#Get x, y coordinates - long and lat
wetpt <- st_coordinates(wetlandsXYDEM)
wetpt <- wetlandsXYDEM %>%
  cbind(wetpt) %>%
  st_drop_geometry() %>%
  dplyr::select(Id1=Wetland_Co,Id2=wet_id, long=Y, lat=X, elev='.')

#write file to csv - change line feed from unix style to windows otherwise ClimateBC errors
write_csv(wetpt, path=file.path(dataOutDir, 'ESIwetpt.csv'))

#Process file on Windows machine using ClimateBC and read back in
#Annual Variables
ESIclimate<-read_csv(file=file.path(DataDir,'ESIwetpt_Normal_1961_1990Y.csv')) %>%
  dplyr::rename(Wetland_Co=Id1) %>%
  dplyr::select(Wetland_Co, Latitude, Longitude, Elevation, CMD, DD5)
#Seasonal Variables
ESIclimateS<-read_csv(file=file.path(DataDir,'ESIwetpt_Normal_1961_1990S.csv')) %>%
  dplyr::rename(Wetland_Co=Id1) %>%
  mutate(Rad=Rad_sp+Rad_sm) %>%
  dplyr::select(Wetland_Co, Latitude, Longitude, Elevation, Rad_sp, Rad_sm, Rad)

#CMD		Hargreaves climatic moisture deficit (mm)
#DD>5		degree-days above 5°C, growing degree-days
#sum:
#RAD_sp	spring solar radiation (MJ m‐2 d‐1)
#RAD_sm	summer solar radiation (MJ m‐2 d‐1)



vars <- c("CMD","DD5")
climate_norms_hist <- climr_downscale(xyz = xyzDT, which_normal = "auto",
                                      return_normal = TRUE,
                                      historic_period = "2001_2020",
                                      vars = vars,
                                      out_spatial = FALSE) ##specify desired variables to plot


 ds_out_CMD <- climr_downscale(xyz = xyzDT, which_normal = "auto",
                          gcm_models = c("ACCESS-ESM1-5"),
                          ssp = c("ssp370"),
                          gcm_period = c("2021_2040"),
                          #gcm_ts_years = 2020:2060,
                          max_run = 1, # we want 3 individual runs for each model
                          vars = c("CMD"))

ds_out <- climr_downscale(xyz = xyzDT, which_normal = "auto",
                                                                          gcm_models = c("ACCESS-ESM1-5", "EC-Earth3"),
                                                                          ssp = c("ssp370","ssp245"),
                                                                          gcm_period = c("2021_2040", "2041_2060","2061_2080"),
                                                                          #gcm_ts_years = 2020:2060,
                                                                          max_run = 3, # we want 3 individual runs for each model
                                                                          vars = c("CMD","DD5"))
#"RAD_sp","RAD_sm"


dwscl_out <- climr_downscale(xyzDT, which_normal = "auto",
                             gcm_models = list_gcm()[1:3],
                             ssp = list_ssp()[1],
                             gcm_period = list_gcm_period(),
                             max_run = 0L, return_normal = FALSE,
                             vars = "MAT", out_spatial = FALSE)


tempRas <- rast(DEM_AOI)

dwscl_Ras <- sapply(split(dwscl_out, by = c("GCM", "PERIOD")) , function(DT, tempRas) {
  tempRas[DT$id] <- DT$MAT
  return(tempRas)
}, tempRas = tempRas)

## make a stack
dwscl_Ras <- rast(dwscl_Ras)

dwscl_Ras

############

dem_vancouver <- mask(dem_vancouver, vancouver_poly)

## extract points at a coarser scale, but elevation at original scale
lowRes_ras <- rast(DEM_AOI)
lowRes_ras <- project(lowRes_ras, crs(lowRes_ras), res = 0.01)

lowRes_ras[] <- 1
lowRes_ras <- mask(lowRes_ras, vancouver_poly) ## set water back to NA

lowRes_points <- as.points(lowRes_ras)
lowRes_points$id <- 1:nrow(lowRes_points)

xyzDT <- extract(dem_vancouver, lowRes_points, xy = TRUE) |>
  as.data.table()
setnames(xyzDT, c("id", "elev", "lon", "lat"))



