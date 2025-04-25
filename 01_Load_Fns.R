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

#Functions
ClipFn <- function(layernm){
  df<-read_sf(file.path(spatialOutDirP,paste0(layernm,".gpkg"))) %>%
    st_intersection(AOIbuff)
  st_crs(df)<-3005
  #Strip the 'P' off and save to disk
  write_sf(df,file.path(spatialOutDir,paste0(str_sub(layernm,1,str_length(layernm)-1),'.gpkg')),delete_layer=TRUE)
}

#Use get_data_fn to fetch conventional BC data catalogue layers
get_data_fn <- function(BC_DC_layer,layer_name){
  df<-bcdc_get_data(BC_DC_layer) %>%
    mutate(areaHa=as.numeric(st_area(.)*0.0001))
  st_crs(df)<-3005
  write_sf(df,file.path(spatialOutDirP,paste0(layer_name,'.gpkg')),delete_layer=TRUE)
}

ZipFn <- function(BC_DC,extDir){
  DownZip<-paste0(extDir,'.zip')
  download.file(BC_DC,
                destfile = file.path(spatialOutDirP, extDir, DownZip))
  unzip(file.path(spatialOutDirP, extDir, DownZip), exdir = file.path(spatialOutDirP, extDir))
  list.files(file.path(spatialOutDirP, extDir), pattern = ".gdb", full.names = TRUE)[1]
}

gdbFn<-function(gbd_in,layernm,outN){
  df_in <- read_sf(gbd_in, layer = layernm)
  #st_crs(df_in) <- 3005
  write_sf(df_in, file.path(spatialOutDirP,paste0(outN,".gpkg")))
  return(df_in)
}
