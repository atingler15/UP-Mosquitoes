# University Park Mosquitoes


This is an analysis of the effectiveness and sustainability of a community-led mosquito control program in University Park, Maryland. These analyses examine what knowledge, attitude, and practice (KAP) variables predicted whether or not residents deployed a mosquito trap, known as a gravid *aedes* trap (GAT). The analysis also examines how GAT deployment and environmental variables influence mosquito abundance. 

### Full Thesis 
The complete thesis for which this analysis was conducted can be read in the [THESIS_SUBMITTED_FINAL PDF](./THESIS_SUBMITTED_FINAL.pdf). 

### Run Directions 
Install the following packages:
1) tidyvers
2) car
3) broom
4) kableExtra
5) emmeans
6) ggplot2

Files should be run in the following order:
1) KAP_Data_Cleaning_Run1st
2) KAP_Univariate_Screen_Run2nd (non-essential screening script, can be skipped)
3) KAP_Predictors_GAT_Deployments_MultivarMod_Run3rd
Files containing the word "Plots" can be run in no particular order, as long as the first three files are run in sequence. 