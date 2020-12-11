# Trial Recruitment Tool

![Logo](/Logo.png)

## Background

In a novel partnership between clinical trial investigators of the South African Medical Research Council and industrial engineers from the Stellenbosch University Health Systems Engineering and Innovation Hub, we developed a Trial Recruitment Tool (TRT). The objective of the tool is to serve as a computerised decisions-support system to aid the planning and management phases of the trial recruitment process. 

The tool was developed for the direct application environment and has since been modified to provide flexibility for possible further application.

## Repo Contents

This repo contains four folders as described below:

**0. Packages Installation** - The installations required to run the tool

**1. Trial Recruitment Tool** - The complete Trial Recruitment Tool

**2. Planning Tool** - A partial Trial Recruitment Tool with only the planning component

**3. Example Data** - Example data which may be used to play with the tool functionality


## Planning Component

The trial recruitment planning component assists decision-making during the planning phase of a clinical trial and comprises single site and multisite planning.

### Single Site Planning

The single site planning component provides a rough estimation of how long a trial or site is expected to take to recruit a specified sample size and can be deployed before exact site details are known. Specific known parameters are used by the simulation model to calculate and display the expected TRD. The initial values, which are displayed in the input areas of the TRT, are typical values that would be used for planning based on previous data.

### Multisite Planning

The multisite planning component allows the expected TRD to be predicted with greater accuracy by incorporating several different sites, each with their own parameters. It allows consideration of different scenarios and facilitates planning decisions based on the information presented. Once the number of sites is specified, a table based on the number of sites is generated and the parameters for each site can be inputted. As for single site planning, expected recruitment rate and goals for each site are required, with the option to input initiation delays if the site will only start recruiting after a specified period

The input parameters are used to predict the recruitment duration for each site, and the overall TRD displayed on the dashboard is taken to be the duration from the start of recruitment until the last patient is recruited. This may be derived by considering the predicted completion period of the site that is expected to complete recruitment last, termed the determinative site.


## Monitoring Component

The trial recruitment monitoring component uses and analyses the data retrieved from the trial management information system and displays it graphically. Real-world trial data is entered as a comma-separated value file or Excel spreadsheet into the TRT. The data is processed in the background and the three displays, discussed in the following sections, are generated.

### Initialising the monitoring component

This component requires baseline and trial data to be uploaded for analysis. **Example data** is provided that may be used to demonstrate the functionality of the tool or adapted for application in real world trials. The following steps may be followed to use the example data:

1. In the _Monitoring_ tab select the _1. Baseline Parameters_ section and upload the Baseline.xlsx file found in the Example data folder to the section which states _Import and Export Baseline Parameters_.
2. In the _Monitoring_ tab upload the _Mock data for analysis.xslx_ file to the upload field which states _Add the latest recruitment data:_

The tool will execute the analysis in the backend and generate three interfaces, which are discussed in the following sections.

### Trial Overview Component 

The trial overview component provides the highest-level summary of the trial progress by displaying a single view of all the current recruitment data. The total number of recruitments and a graph of the cumulative number of recruitments over the entire period are shown. Simulations for each site are performed in the background, based on the provided data and baseline information. The determinant site, with the longest expected remaining duration, is displayed. 

### Site Overview Component

The site overview component uses the baseline information to plot the different sites on an interactive map. When a site is selected, the site’s recruitment information is displayed in a pop-up information box. A specific recruitment quantity can be specified and sites that have less recruitments than specified are shown by red markers.

### Site Specific Component

Sites may be further analysed by selecting the site on the map, graph or from a dropdown list. The information of the selected site consists of the actual site recruitment information and the simulated remaining expected duration based on the actual site data. Different scenarios can be displayed by changing the simulation parameters to see the effect of changes in recruitment rate or the number of outstanding recruitments on the predicted remaining duration.


Copyright © 2020 Stellenbosch University and South African Medical Research Council. This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA. For more information please contact the SAMRC at info.ship@mrc.ac.za
