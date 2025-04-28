### Wetland Ecosystem Services Protocol (WESP) Office Questions
<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->

============================  
The B.C. Wildlife Federation’s Wetlands Workforce project is a collaboration with conservation organizations and First Nations working to maintain and monitor wetlands across British Columbia.   
https://bcwf.bc.ca/initiatives/wetlands-workforce/.  

WESP - Wetland Ecosystem Services Protocol   

There are five sets of WESP R scripts to answer the office questions for wetlands within a study area.  
1) WESP_data_prep - This repository, presents a set of scripts used to generate a new, or process existing, wetlands for a study area;  
2) WESP_Sample_Design - attributes wetlands with local human and natural landscape characteristics - https://github.com/BCWF-Wetlands/WESP_Sample_Design;   
3) WESP_Sample_Draw - Generates a report card of how samples are meeting sampling criteria and performs a draw to select wetlands to meet criteria - https://github.com/BCWF-Wetlands/WESP_Sample_Draw;  
4) WESP_OF - this repository, automates the majority of the office WESP questions. Questions OF6, OF8, OF9, OF10, OF11, OF13, OF14, OF24 must be answered manually; and  
5) WESP_Calculator - reads the office and field questions and runs wespr which calculate the Ecosystem Services for set of reference sites or a single comparison site;  

### Usage

There are a set of scripts that help prepare and clea data and conduct the analysis for generating the answers to the WESP office questions, there are five basic sets of scripts:    
1. Control scripts - set up the analysis environment;  
2. Load scripts - loads data;    
3. Clean scripts - cleans and prepares wetland data;    
4. Analysis scripts - The office questions are split into 7 types;   
&nbsp;&nbsp;&nbsp;&nbsp;1. features adjacent to a wetland,  
&nbsp;&nbsp;&nbsp;&nbsp;2. features within the wetland's Assessment watershed,  
&nbsp;&nbsp;&nbsp;&nbsp;3. Climate Change indicators,  
&nbsp;&nbsp;&nbsp;&nbsp;4. Features within certain distance from a wetland,  
&nbsp;&nbsp;&nbsp;&nbsp;5. Features within a wetland,  
&nbsp;&nbsp;&nbsp;&nbsp;6. Features within 2km of a wetland, and  
&nbsp;&nbsp;&nbsp;&nbsp;7. Features within 100m of a wetland; and   
5. Output scripts - results from the analysis scripts are consolidated and joined with the manual data.
  
#Control Scripts:   
EcoP_0_Data_run.R	Sets local variables and directories used by scripts, presents script order.  
header.R	loads R packages, sets global directories, and attributes.

#Load Scripts:   
EcoP_01_load.R	Loads core spatial layers used by routines.  

#Clean Scripts:   
EcoP_02_clean.R	Cleans data for analysis. 

#Analysis Scripts:   
EcoP_03_Adjacent.R - OF15, OF21, OF22, OF34, OF35  
EcoP_03_ASS_WS.R - OF05, OF07, OF12, OF41, OF42, OF43  
EcoP_03_CC.R - OF25, OF26, OF27  
EcoP_03_Distance.R - OF02, OF03, OF04  
EcoP_03_OF20.R - OF20  
EcoP_03_Within.R - OF01, OF16, OF17, OF23, OF28, OF29  
EcoP_03_within2km.R - OF18, OF19, OF31, OF32, OF33, OF37  
EcoP_03_within100m.R - OF30, OF36, OF38, OF39, OF40  

#Output Scripts:   
There EcoP_04_Collate.R	- collates the results from the analysis scripts along with the manual repsonses to questions OF6, OF8, OF9, OF10, OF11, OF13, OF14, OF24.  

### Project Status

The set of R WESP scripts are continually being modified and improved, including adding new study areas as sampling is initiated.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/BCWF-Wetlands/WESP_data_prep/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2022 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
