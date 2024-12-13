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

#Test catchment methods
#Pure R solution
#https://vt-hydroinformatics.github.io/rgeowatersheds.html

library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
#library(rayshader)
#library(rgl)
library(bcmaps)
library(bcdata)
whitebox::wbt_init()

#knitr::knit_hooks$set(webgl = hook_webgl) # doesnt work...
theme_set(theme_classic())

#Load test data
#TestAOI <- get_layer("wsc_drainages") %>%
#  dplyr::filter(SUB_SUB_DRAINAGE_AREA_NAME=='Nicola')
#AssessmentWatersheds
waterData <- file.path(ProvData,'WaterFeatures')
FWA_gdb<-file.path(waterData,'FWA_BC.gdb')
FWA_list <- st_layers(FWA_gdb)
AssWshd_GD <- read_sf(FWA_gdb, layer = "FWA_NAMED_WATERSHEDS_POLY") %>%
  #dplyr::filter(WATERSHED_GROUP_CODE %in% c('BULK','MORR')) %>%
  #dplyr::filter(GNIS_ID==21201)
  st_intersection(AOI)
saveRDS(AssWshd,file='tmp/AssWshd')
write_sf(AssWshd, file.path(spatialOutDir,"AssWshd.gpkg"))
TestAOI<-AssWshd
TestAOI<-AOI

mapview(AssWshd)+mapview(Field2023Data)
#Mid point of wetland
ppointSPmid<-Field.pt %>%
  st_intersection(TestAOI) %>%
  dplyr::select(wet_id)
#Establish Pour Points - wetland pour point 'bottom' of wetland
Field_wet<-Field2023Data %>%
  st_intersection(TestAOI) %>%
  dplyr::select(wet_id)

#For each wetland - get its lowest point
DEM_AOI<-DEM.tp %>%
  terra::crop(TestAOI) %>%
  raster()
WetLowFn <- function(wet){
  w1<-DEM_AOI %>%
    raster::mask(wet) %>%
    #raster::crop(wet) %>%
    which.min(.)
  w2<-xyFromCell(DEM_AOI,w1[1])
  return(w2)
}
wetlandsXY<-lapply(1:nrow(Field_wet), function(i) WetLowFn(Field_wet[i,]))
wetpt<-as.data.frame(do.call('rbind',wetlandsXY))
ppointSPLow<-st_as_sf(wetpt, coords = c("x", "y"))
sf::st_crs(ppointSPLow)<-sf::st_crs(Field_wet)

mapview(ppointSPLow) +mapview(ppointSPmid)+mapview(Field_wet)
st_write(ppointSPLow, file.path(spatialOutDir,"ppointSPLow.shp"),delete_dsn=TRUE)
st_write(ppointSPmid, file.path(spatialOutDir,"ppointSPmid.shp"),delete_dsn=TRUE)
ppointSP <-ppointSPLow
st_write(ppointSP, file.path(spatialOutDir,"ppointSP.shp"),delete_dsn=TRUE)

demT<-DEM_AOI
#dem[dem<1500]<-NA
writeRaster(demT, file.path(spatialOutDir,"demT.tif"), overwrite = TRUE)
#Hillshade
wbt_hillshade(dem = file.path(spatialOutDir,"demT.tif"),
              output = file.path(spatialOutDir,"demTHill.tif"),
              azimuth = 115)
#Prepare DEM - https://vt-hydroinformatics.github.io/rgeoraster.html#prepare-dem-for-hydrology-analyses
#Breach Depressions
demT<-file.path(spatialOutDir,"demT.tif")
wbt_breach_depressions_least_cost(
  dem = demT,
  output = file.path(spatialOutDir,"demTbreach.tif"),
  dist = 5,
  fill = TRUE)
#Fill Depressions
demTbreach<-file.path(spatialOutDir,"demTbreach.tif")
wbt_fill_depressions_wang_and_liu(
  dem = demTbreach,
  output = file.path(spatialOutDir,"demTbreachfilled.tif"))

#demTbreachfilled<-raster(file.path(spatialOutDir,"demTbreachfilled.tif"))

#Generate Flow Accumulation - D8 D infinity
#wbt_d8_flow_accumulation(input = file.path(spatialOutDir,"demTbreachfilled.tif"),
#                         output = file.path(spatialOutDir,"D8FA.tif"))
wbt_d8_pointer(dem = file.path(spatialOutDir,"demTbreachfilled.tif"),
               output = file.path(spatialOutDir,"D8pointer.tif"))
wbt_d_inf_flow_accumulation(input = file.path(spatialOutDir,"demTbreachfilled.tif"),
                            output = file.path(spatialOutDir,"Dinf.tif"))
#Dinf<-raster(file.path(spatialOutDir,"Dinf.tif"))

wbt_d_inf_pointer(dem = file.path(spatialOutDir,"demTbreachfilled.tif"),
                  output = file.path(spatialOutDir,"Dinfpointer.tif"))
#Dinfpointer<-raster(file.path(spatialOutDir,"Dinfpointer.tif"))

#Generate raster streams
wbt_extract_streams(flow_accum = file.path(spatialOutDir,"Dinf.tif"),
                    output = file.path(spatialOutDir,"raster_streams.tif"),
                    threshold = 750)
#raster_streams<-raster(file.path(spatialOutDir,"raster_streams.tif"))

#Move pour points to nearest stream cell
wbt_jenson_snap_pour_points(pour_pts = file.path(spatialOutDir,"ppointSP.shp"),
                            streams = file.path(spatialOutDir,"raster_streams.tif"),
                            output =  file.path(spatialOutDir,"snappedpp.shp"),
                            snap_dist = 0.0017) #careful with this! Know the units of your data
snappedpp<-st_read(file.path(spatialOutDir,"snappedpp.shp"))

mapview(snappedpp)+mapview(Field_wet)
#Generate catchment
#wbt_watershed(d8_pntr = file.path(spatialOutDir,"Dinfpointer.tif"),
wbt_watershed(d8_pntr = file.path(spatialOutDir,"D8pointer.tif"),
                            pour_pts = file.path(spatialOutDir,"snappedpp.shp"),
              output = file.path(spatialOutDir,"Wetland_Catchment.tif"),
            verbose_mode=TRUE)
#Shape file of catchments
wetCatchShp <- read_stars(file.path(spatialOutDir,"Wetland_Catchment.tif")) %>%
  st_as_sf(merge = T)
st_write(wetCatchShp, file.path(spatialOutDir,'wetCatch.shp'),delete_dsn=TRUE)


#Next Step....
#identify upstream watersheds, lower wetlands are the combination of their wetland
# + those wetland watersheds above them
# Investigate wetlands that only get a thin string going to them - are they isolated?
# identify flow in, through and out using flow accumulation
# Wetlands who clearly drain to a stream but their catchment is internal?




#Visualization
snappedpp<-st_read(file.path(spatialOutDir,"snappedpp.shp"))
mapview(wetCatchShp)+mapview(Wetlands)+mapview(snappedpp)

tm_shape(raster(file.path(spatialOutDir,"demTHill.tif")))+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()
#Filled DEM
tm_shape(raster(file.path(spatialOutDir,"demTbreachfilled.tif")))+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()
## Difference between filled and original DEM
difference <- raster(file.path(spatialOutDir,"demT.tif")) - raster(file.path(spatialOutDir,"demTbreachfilled.tif"))
difference[difference == 0] <- NA
#Plot difference
tm_shape(raster(file.path(spatialOutDir,"demTHill.tif"))
) +
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_scale_bar()+
  tm_shape(difference)+
  tm_raster(style = "cont",legend.show = TRUE)+
  tm_scale_bar()
#Dinf on hillshade
tm_shape(raster(file.path(spatialOutDir,"demTHill.tif")))+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(log(raster(file.path(spatialOutDir,"Dinf.tif"))))+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE, alpha = .5)+
  tm_scale_bar()
#Streams on hillshade
tm_shape(raster(file.path(spatialOutDir,"raster_streams.tif")))+
  tm_raster(legend.show = TRUE, palette = "Blues")+
  tm_shape(shapefile(file.path(spatialOutDir,"snappedpp.shp")))+
  tm_dots(col = "red")
#Catchment
tm_shape(raster(file.path(spatialOutDir,"demTHill.tif")))+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(raster(file.path(spatialOutDir,"Wetland_Catchment.tif")))+
  tm_raster(legend.show = TRUE, alpha = 0.5, style = "cat")+
  tm_shape(shapefile(file.path(spatialOutDir,"snappedpp.shp")))+
 # (pp)+
  tm_dots(col = "red")
#Catchment as vector
tm_shape(raster(file.path(spatialOutDir,"demTHill.tif")))+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(wetCatchShp)+
  tm_borders(col = "red")










#Accessing GRASS through R to QGIS
# https://gis.stackexchange.com/questions/303786/define-catchment-boundaries-with-gis-using-r

#subcatch(DEM(as a matrix), outpoint (as a vector))
#Tried converting to stack of each then extract DEM and single point but preserve it
#matrix location....

library('topmodel')
ws<-get_layer("wsc_drainages", class = "sf")
TestAOI<-ws %>%
  dplyr::filter(SUB_DRAINAGE_AREA_NAME=='Thompson'&
                  SUB_SUB_DRAINAGE_AREA_NAME=='Nicola')

DEM_AOI<-DEM.tp %>%
  mask(TestAOI) %>%
  crop(TestAOI)
writeRaster(DEM_AOI, filename=file.path(spatialOutDir,'DEM_AOI.tif'), overwrite=TRUE)

DEM_AOI<-raster(file.path(spatialOutDir,'DEM_AOI.tif'))
DEM_AOIt<-rast(DEM_AOI)
DEMtM<-as.matrix(DEM_AOIt, wide=TRUE)
#1363285,505111
#SI_2718
TDEM<-raster::as.matrix(DEM_AOI)

Twet2<-wetland.pt %>%
  filter(WTLND_ID=='SI_3235') %>%
  st_coordinates()

wetM<-wetland.pt %>%
  select(wet_id) %>%
  as("Spatial") %>%
  rasterize(DEM_AOI)

wetS<-stack(DEM_AOI,wetM)
names(wetS)

MwetS<-raster::as.matrix(wetS,rownames.force=TRUE)

m[m[, "three"] == 11,]
Cwet<-as.data.frame(MwetS)
result <- filter(as.data.frame(MwetS), wet_id == 3235)


Cwet<-as.data.frame(MwetS[MwetS[,'wet_id',drop=TRUE]== 3235,])
Cwet<-as.data.frame(MwetS) %>%
  dplyr::select(wet_id=4754)


Tcatch<-subcatch(MwetS,Cwet)



example_points <- as(example_points, "Spatial")


point_location <- raster::xyFromCell(DEM_AOI, which.max(raster::extract(DEM_AOI, Twet)))
#point_location <- raster::xyFromCell(TDEM, which.max(raster::extract(TDEM, Twet)))


mapview(TestAOI)+mapview(Twet)+mapview(point_location)


tt<-raster::extract(TDEM, Twet)



raster_data <- raster("path/to/raster.tif")
point_data <- st_read("path/to/file.shp")
# Get matrix location of point in raster
point_location <- raster::xyFromCell(raster_data, which.max(raster::extract(raster_data, point_data)))
point_row_col <- as.integer(raster::rowFromY(raster_data, point_location$y))
point_col_row <- as.integer(raster::colFromX(raster_data, point_location$x))
# Print row and column index of point in raster
cat("Row: ", point_row_col, "\n")
cat("Column: ", point_col_row, "\n")


# Load DEM and point data as sf object
dem <- raster(file.path(spatialOutDir,'DEM_AOI.tif'))
#point <- st_as_sf(data.frame(ID = 1, x = -100, y = 50), coords = c("x", "y"), crs = st_crs(dem))
point<-Twet
# Extract elevation value from DEM at point location
elev <- extract(dem, point)
# Create catchment area around point location using D8 flow direction algorithm
flow_dir <- terrain(dem, opt = "flowdir")
catchment <- catchment(flow_dir, point)
# Plot DEM with catchment area
plot(dem)
plot(catchment, add = TRUE)
# Write catchment area to a new shapefile
write_sf(catchment, "path/to/catchment.shp") This code loads a DEM and a point location as an sf object, extracts the elevation value at the point location from the DEM, and then creates a catchment area using the D8 flow direction algorithm. The resulting catchment area is then plotted on top of the DEM and written to a new shapefile.

whitebox::install_whitebox()
library('whitebox')

wbt_watershed(d8_pntr = ptr,
              pour_pts = pps,
              output = out)



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



#Accessing GRASS through R to QGIS
library(gdalUtils)
library(RQGIS3)
library(here)
set_env("C:/OSGeo4W64")
qgis_session_info()

gdal_translate(here("data", "cdem_dem_021M.tif"),
               here("temp_files", "dem_temp.tif"),
               projwin = c(-71.2, 47.5, -70.5, 47))
#get_usage(alg = "grass7:r.watershed")
run_qgis(alg = "grass7:r.watershed",
         elevation = here("temp_files", "dem_temp.tif"),
         threshold = 5000,
         drainage = here("temp_files/drain.tif"))
#get_usage(alg = "grass7:r.water.outlet")
bassin_grass <- run_qgis(alg = "grass7:r.water.outlet",
                         input = here("temp_files/drain.tif"),
                         output = here("temp_files/bassin_grass.tif"),
                         coordinates="-70.8837037458,47.0692354224",
                         load_output = TRUE)
