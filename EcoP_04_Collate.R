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

#lapply to collate all the OF answers in the out/data folder
#gather all the OF spreadsheets into a list and join them together
# First delete Answer file if it exists
Answer_File<-paste0(dataOutDir,"/",WetlandAreaShort,"_OF_Answers.data.xlsx")
if (file.exists(Answer_File)) {
  file.remove(Answer_File)
}
OF_Files<-list.files(path=file.path(dataOutDir),
                     recursive=FALSE, pattern='OF')

OF_AnswersL <- c(lapply(file.path(dataOutDir,
                                       OF_Files), read_xlsx))
OF_Answers.1<-OF_AnswersL %>%
  purrr::reduce(left_join, by = "WTLND_ID") %>%
  select(WTLND_ID,(contains('OF'))) %>%
  st_drop_geometry()

OF_Answers.1.Check<-OF_Answers.1 %>%
     dplyr::select(WTLND_ID, c(paste0('OF30_',(1:3))))

#Pull out manual office questions from FWetlands - will be switching to read from survey 123 file
FWetlands<-st_read(file.path(spatialInDir,paste0(WetlandAreaDir,"_2024Field.gpkg")))

OF_manual<-FWetlands %>%
  st_drop_geometry() %>%
  dplyr::rename(OF6_1=Stream_Intersect) %>%
  dplyr::rename(OF8_0=GlacialInfluence) %>% # parse out 3 cases below
  dplyr::rename(OF9_0=Flood_Infastructure) %>% # parse out 4 cases below
  dplyr::rename(OF10_0=Internal_Flow_dist) %>%
  #Set up cases for OF_10
  mutate(OF10_1=if_else(OF10_0 >0 & OF10_0<10,1,0)) %>%
  mutate(OF10_2=if_else(OF10_0 >=10 & OF10_0<50,1,0)) %>%
  mutate(OF10_3=if_else(OF10_0 >=50 & OF10_0<100,1,0)) %>%
  mutate(OF10_4=if_else(OF10_0 >=100 & OF10_0<1000,1,0)) %>%
  mutate(OF10_5=if_else(OF10_0 >=1000 & OF10_0<2000,1,0)) %>%
  mutate(OF10_6=if_else(OF10_0 >=2000 | OF10_0==0,1,0)) %>%
  #Set up cases for OF_11
  dplyr::rename(OF11_0=Percent_of_Catchment) %>% #Can be 'Percent_of_catchament' in some files...
  mutate(OF11_1=if_else(OF11_0 <0.01,1,0)) %>%
  mutate(OF11_2=if_else(OF11_0 >=0.01 & OF11_0<0.1,1,0)) %>%
  mutate(OF11_3=if_else(OF11_0 >=0.1 & OF11_0<1,1,0)) %>%
  mutate(OF11_4=if_else(OF11_0 >=1,1,0)) %>%
  dplyr::mutate(OF13_1=if_else(ConservationInvestment=='0',0,1)) %>%
  dplyr::rename(OF14_1=Sustained_Sci_Use) %>%
  mutate(OF24_0=Species_of_Concern) %>% # parse out 5 cases below
  mutate(OF44_1=ifelse(WetlandAreaShort=='GD',1,0)) %>%
  mutate(OF44_2=ifelse(WetlandAreaShort=='CM',1,0)) %>%
  mutate(OF44_3=ifelse(WetlandAreaShort=='SIM',1,0)) %>%
  mutate(OF44_4=ifelse(WetlandAreaShort=='BP',1,0)) %>%
  mutate(OF44_5=ifelse(WetlandAreaShort=='TP',1,0)) %>%
  dplyr::select(WTLND_ID, OF6_1,OF8_0,OF9_0,OF10_1,OF10_2,OF10_3,OF10_4,OF10_5,OF10_6,
                OF11_1,OF11_2,OF11_3,OF11_4,OF13_1,OF14_1,OF24_0,
                OF44_1,OF44_2,OF44_3,OF44_4,OF44_5) %>%
  select(WTLND_ID,(contains('OF'))) %>%
  replace(is.na(.), 0)

OF_manual_Wetland_Co<-OF_manual %>%
  dplyr::select(WTLND_ID)

# Make list of manual variables that require parsing
ParseVars<-c('OF8_0','OF9_0','OF24_0')
#Number of sub-categories for each variable
NparseVars<-c(3,4,5)
#Function to parse
SplitFn1 <- function(i,df) {
  df2<-lapply(1:NparseVars[i], function(j) {
    #FormVName<-paste0(ParseVars[i],"_",j)
    FormVName<-sub('_0',paste0('_',j),ParseVars[i])
    df %>%
      mutate(!!FormVName:= if_else(!!rlang::sym(ParseVars[i])==j,1,0)) %>%
      dplyr::select(!!rlang::sym(FormVName))
  })
  do.call(cbind, df2)
}
#Loop through each Variable to split out and call the function
#that splits it into separate variables
df3<-lapply(1:length(ParseVars), function(x) {
  df1<-OF_manual %>%
    rowwise() %>%
    #mutate(Vparts=(strsplit(!!rlang::sym(ParseVars[x]), ","))) %>%
    #mutate(VpartsN=list(parse_number(Vparts))) %>%
    mutate(VpartsN=NparseVars[x]) %>%
    dplyr::select((ParseVars[x]),VpartsN)
  # dplyr::select((ParseVars[x]),Vparts,VpartsN)
  #SplitFn1(x,df1$VpartsN)
  SplitFn1(x,df1)
})
#Combine generated form sub-variables with original data.frame
OF_manual.1<-cbind(OF_manual_Wetland_Co,do.call(cbind, df3))
OF_manual.2 <- merge(OF_manual,OF_manual.1,by='WTLND_ID') %>%
  dplyr::select(-c(OF8_0,OF9_0,OF24_0))

OF_Answers.data <- merge(OF_Answers.1,OF_manual.2,by='WTLND_ID') %>%
  mutate_all(funs(str_replace(.,'N/A','0'))) %>%
  replace(is.na(.),0) %>%
  select(WTLND_ID, order(colnames(.)))
#Data check
OF_Answers.data.Check<-OF_Answers.data %>%
  dplyr::select(WTLND_ID, c(paste0('OF24_',(1:4))))
#Make a spatial version for checking
OF_Answers.spatial<- FWetlands  %>%
  dplyr::select(WTLND_ID) %>%
  left_join(OF_Answers.data, by = "WTLND_ID")

st_write(OF_Answers.spatial,file.path(spatialOutDir,paste0(WetlandAreaShort,'_OF_Answers.gpkg')),delete_dsn=TRUE)

WriteXLS(OF_Answers.data,file.path(dataOutDir,paste0(WetlandAreaShort,'_OF_Answers.data.xlsx')))

##################
