# Copyright 2018 Province of British Columbia
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

#Load packages, options and set up higher level directories
source("header.R")
#Provincial - run_Prov.R - must be run prior to EcoProv data prep - downloads data to local drive

#set up abbreviations and directory locations for EcoProvinces
WetlandAreaL<-list('SIM_Base',c('Taiga_Planes_Base','Boreal_Plains_Base'),
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaDirL<-c('SIM_Base','Taiga_Boreal_Plains',
                   'Sub_Boreal','GD_Base','GD_Base_Est','Sub_Boreal_PEM','SI_Base')
WetlandAreaShortL<-c('SIM','TBP',
                     'SB','GD','GD_Est','SB_PEM','SI')
EcoPNL<-list("SOUTHERN INTERIOR MOUNTAINS",c("BOREAL PLAINS","TAIGA PLAINS"),
             "SUB-BOREAL INTERIOR","GEORGIA DEPRESSION","GEORGIA DEPRESSION","SUB-BOREAL INTERIOR","SOUTHERN INTERIOR")

#Select an EcoProvince(s)
#one of: 1-SIM, 2-TBP, 3-SB, 4-GD, 5-GD_Est, 6-SB_PEM, 7-SI

EcoP<-1
WetlandArea<-WetlandAreaL[EcoP]
WetlandAreaDir<-WetlandAreaDirL[EcoP]
WetlandAreaShort<-WetlandAreaShortL[EcoP]
EcoPN<-as.character(EcoPNL[EcoP])
#For Plains use:
# EcoPN<-c("BOREAL PLAINS","TAIGA PLAINS")


#Base load
spatialOutDir <- file.path('out','spatial',WetlandAreaDir)
spatialInDir <- file.path(spatialOutDirP,WetlandAreaDir)
dataOutDir <- file.path(OutDir,'data',WetlandAreaDir)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
tempAOIDir<-paste0("tmp/",WetlandAreaDir)
dir.create(tempAOIDir, showWarnings = FALSE)

#Load layers
#Field Layers - should be first checked and cleaned by WESP_data_prep.R
Field_in<-st_read(file.path(spatialInDir,paste0("SIM_Base_2024Field.gpkg")))
FWetlandsIn<-Field_in

#load wetlands processed by WESP_data_prep
source('EcoP_01_load.R')
#Fetch Field Data and AOIw based on wetlands, 2k buffer and their resident ASS_WS
source('EcoP_02_clean.R')

#Call routines to clip Provincial data to AOI
#Attributes within wetland (Karst, Geological Fault, BGC Protected, Topographic Position, Elevation, Aspect, Fish)
#source('EcoP_03_Within.R')

#Distance to attributes
#Attributes adjacent to wetland
#Attributes within 100m
#Attributes within 2km
#Attributes within ASS_WS
#CC attributes

