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

OF_Answers<-read_xlsx(file.path(dataOutDir,'OF_Answers.data.xlsx'))

#Format the data so can feed calculator
#OF Column Names
#OF_1
OF01_f<-OF_Answers %>%
  dplyr::select(WTLND_ID,OF01) %>%
  mutate(OF1_1=if_else(OF01>0 & OF01<100,1,0)) %>%
  mutate(OF1_2=if_else(OF01>100 & OF01<500,1,0)) %>%
  mutate(OF1_3=if_else(OF01>500 & OF01<1000,1,0)) %>%
  mutate(OF1_4=if_else(OF01>1000 & OF01<5000,1,0)) %>%
  mutate(OF1_5=if_else(OF01>5000,1,0))

#OF_2
OF02_f<-OF_Answers %>%
  dplyr::select(WTLND_ID,OF02) %>%
  mutate(OF02=OF02/1000) %>% # convert m to km
  mutate(OF2_1=if_else(OF02>0 & OF02<10,1,0)) %>%
  mutate(OF2_2=if_else(OF02>10 & OF02<25,1,0)) %>%
  mutate(OF2_3=if_else(OF02>25 & OF02<50,1,0)) %>%
  mutate(OF2_4=if_else(OF02>50 & OF02<100,1,0)) %>%
  mutate(OF2_5=if_else(OF02>100 & OF02<500,1,0)) %>%
  mutate(OF2_6=if_else(OF02>500,1,0))

#OF_3
OF03_f<-OF_Answers %>%
  dplyr::select(WTLND_ID,OF03) %>%
  mutate(OF03=OF03/1000) %>% # convert m to km
  mutate(OF3_1=if_else(OF03>0 & OF03<10,1,0)) %>%
  mutate(OF3_2=if_else(OF03>10 & OF03<25,1,0)) %>%
  mutate(OF3_3=if_else(OF03>25 & OF03<50,1,0)) %>%
  mutate(OF3_4=if_else(OF03>50 & OF03<100,1,0)) %>%
  mutate(OF3_5=if_else(OF03>100 & OF03<500,1,0)) %>%
  mutate(OF3_6=if_else(OF03>100 & OF03<500,1,0)) %>%
  mutate(OF3_7=if_else(OF03>500,1,0))

#OF_4
OF4_1
OF4_2
OF4_3
OF4_4
OF4_5
OF4_6

#OF_5
OF5_1

#OF_6
OF6_1

#OF_7
OF7_1
OF7_2
OF7_3

#OF_8
OF8_1
OF8_2
OF8_3

#OF_9
OF9_1
OF9_2
OF9_3
OF9_4

#OF_10
OF10_1
OF10_2
OF10_3
OF10_4
OF10_5
OF10_6

#OF_11
OF11_1
OF11_2
OF11_3
OF11_4

OF12_1
OF12_2
OF12_3

OF13_1

OF14_1

OF15_1

OF16_1

OF17_1

OF18_1
OF18_2
OF18_3
OF18_4
OF18_5

OF19_1
OF19_2
OF19_3
OF19_4
OF19_5

OF20_1
OF20_2
OF20_3
OF20_4
OF20_5

OF21_1

OF22_1

OF23_1
OF23_2
OF23_3
OF23_4
OF23_5
OF23_6

OF24_1
OF24_2
OF24_3
OF24_4

OF25_1

OF26_1

OF27_1

OF28_1
OF28_2
OF28_3
OF28_4
OF28_5

OF29_1

OF30_1
OF30_2
OF30_3

OF31_1
OF31_2
OF31_3

OF32_1
OF32_2
OF32_3
OF32_4
OF32_5

OF33_1
OF33_2
OF33_3
OF33_4
OF33_5

OF34_1
OF34_2
OF34_3

OF35_1

OF36_1
OF36_2
OF36_3
OF36_4

OF37_1
OF37_2
OF37_3
OF37_4
OF37_5

OF38_1
OF38_2
OF38_3
OF38_4
OF38_5

OF39_1
OF39_2
OF39_3
OF39_4
OF39_5

OF40_1
OF40_2
OF40_3
OF40_4
OF40_5

OF41_1
OF41_2
OF41_3
OF41_4
OF41_5

OF42_1
OF42_2
OF42_3

OF43_1
OF43_2
OF43_3
OF43_4
OF43_5

OF44_1
OF44_2
OF44_3
OF44_4
OF44_5

