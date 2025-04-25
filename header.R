library(sf)
library(dplyr)
library(readr)
library(raster)
library(bcmaps)
#library(rgdal)
library(fasterize)
library(readxl)
library(mapview)
library(WriteXLS)
library(foreign)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(stars)
#library(rgrass7)
library(exactextractr)
library(expss)
library(openxlsx)
library(cleangeo)
library(geos)
library(tidyr)
library(plyr)
library(bcdata)
library(tmap)
library(smoothr)
library(terra)
library(rmapshaper)
library(tibble)
library(stringr)
library(matrixStats)
library(purrr)
library(datawizard)

options(scipen=999)
options(warn = 1)
options(timeout=10000)

#Install climr - climate BC R package
#install.packages("remotes") # to install climr
#Sys.unsetenv("GITHUB_PAT") # need to remove token for climr install to work
#remotes::install_github("bcgov/climr", force=TRUE)
#https://bcgov.github.io/climr/
#library(climr)

DataDir <- 'data'
dir.create(DataDir, showWarnings = FALSE)
spatialDir <- file.path(DataDir,'spatial')
dir.create(spatialDir, showWarnings = FALSE)
OutDir <- 'out'
dir.create(file.path(OutDir), showWarnings = FALSE)
dataOutDir <- file.path(OutDir,'data')
dir.create(file.path(dataOutDir), showWarnings = FALSE)
spatialOutDir <- file.path(DataDir,'spatial')
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dataDirPrep <- file.path('../WESP_data_prep/data')
dataOutDirP <- file.path('../WESP_data_prep/out/data')
spatialOutDirP <- file.path('../WESP_data_prep/out/spatial')
dir.create("tmp", showWarnings = FALSE)
ProvData<- file.path('../../PROVData')

source('01_Load_Fns.R')
#FieldData<-file.path(DataDir,'2023FieldData')
#OutDirWESP <-'../WESP_data_prep/out'
#dataOutDirWESP <- file.path(OutDirWESP,'data')
#spatialOutDirPWESP <- file.path(OutDirWESP,'spatial')
#DataDirWESP <- '../WESP_Sample_Design/data'
#GISLibrary<- file.path('/Users/darkbabine/ProjectLibrary/Library/GISFiles/BC')
#WESPDir <- file.path('../WESP_Sample_Design/data')
#BioDDir <- file.path('../../Biodiversity/data')
#DrawDir <- file.path('../WESP_Sample_Draw/data')
#WESPdata<-file.path('../../WESPdata')



