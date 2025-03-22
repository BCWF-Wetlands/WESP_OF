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

OF_Answers.data<-read_xlsx(file.path(dataOutDir,'OF_Answers.data.xlsx')) %>%
  dplyr::rename(Wetland_Co=WTLND_ID) #%>%
  #Select rows that are in F and S
  #dplyr::filter(Wetland_Co %in% Fn$Wetland_Co)

OF_Answers.data.Check<-OF_Answers.data %>%
  dplyr::select(Wetland_Co, c(paste0('OF24_',(1:4))))

wesp.1<-OF_Answers.data
wesp.1.check<-wesp.1 %>%
  dplyr::select(Wetland_Co, c(paste0('OF36_',(1:4))))

wesp.2<-wesp.1 #%>%
#dplyr::select(-c(Wetland_Co))
#wesp.sites<-wesp.1 %>%
#  dplry::select(Wetland_Co))
#Transpose and format the data to wespR format and export
wesp.3<-as.data.frame(t(wesp.2))
rownames(wesp.3) <- colnames(wesp.2)
colnames(wesp.3) <- rownames(wesp.2)
#wetLUT<-wesp.1[1,]

colOrder<-str_sort(rownames(wesp.3),numeric=TRUE)
wesp.4<-wesp.3[match(colOrder, rownames(wesp.3)),]

wesp_OF<-wesp.4 %>%
  mutate(Question=rownames(wesp.4)) %>%
  dplyr::select(Question,everything())

write.csv(wesp_OF, file.path(dataOutDir,paste('wesp_OF.csv',sep='')), row.names=FALSE)




#install.packages("devtools")
#devtools::install_github("BCWF-Wetlands/wespr")



##################
