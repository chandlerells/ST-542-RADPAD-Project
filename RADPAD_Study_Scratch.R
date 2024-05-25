library (tidyverse)
library(readxl)
library(tidyr)

#reading in the baseline dosimetry data 


import_base <- read_excel('/Users/angelicefloyd/Documents/Documents/ST542_Statistical_Practice/Week2/Baseline_Dosimetry_Data_RaySafe_for power calc 2.5.24.xlsx', sheet = 'Oct-27-21 to Nov-1-23')


#%>% rename(procedure =`Procedure (BVP, PDA PMI, other)`,weight = `Weight (kg)`, fluoro_sec = `Total fluoro time (sec)`, fluoro_min = `Total fluoro time (min)`)




#reading in the RADPAD data

import_treat <- read_excel('/Users/angelicefloyd/Documents/Documents/ST542_Statistical_Practice/Week2/RADPAD data for power calc 2.5.24_22.xlsx',sheet='RADPAD data') 

#%>% rename(procedure =`Procedure (BVP, PDA PMI, other)`,weight = `Weight (kg)`, fluoro_sec = `Total fluoro time (sec)`, fluoro_min = `Total fluoro time (min)`)




base_treat_import <- rbind(import_base,import_treat) %>% rename(procedure =Procedure_BVP_PDA_PMI_other ,weight = Weight_kg, fluoro_sec = Total_fluoro_time_sec, fluoro_min = `Total_fluoro_time _min`)

base_treat_import$group <- ifelse(base_treat_import$RADPAD_Used == 'Y','control','treatment')

base_treat <- as_tibble(base_treat_import)



base_treat$tec1 <- ifelse(base_treat$tech1 %in% c('Dose not recorded','No dose recorded','Not wearing badge','No data'),9991,ifelse(grepl("n/a|N/A",base_treat$tech1),9999,base_treat$tech1)) %>% as.numeric()
 
base_treat$res1 <- ifelse(base_treat$resident1 %in% c('Dose not recorded','No dose recorded','Not wearing badge','No data'),9991,ifelse(grepl(c("n/a|N/A","N/A"), base_treat$resident1),9999,base_treat$resident1)) %>% as.numeric() 

base_treat$res2 <- ifelse(base_treat$resident2 %in% c('Dose not recorded','No dose recorded','Not wearing badge','No data'),9991,ifelse(grepl(c("n/a|N/A","N/A"), base_treat$resident2),9999,base_treat$resident2)) %>% as.numeric() 


base_treat$tee <- ifelse(base_treat$TEE %in% c('Dose not recorded','No dose recorded','Not wearing badge','No data'),9991,ifelse(grepl("n/a|N/A", base_treat$TEE),9999,base_treat$TEE)) %>% as.numeric()

base_treat$anes <- ifelse(base_treat$anesthesia %in% c('Dose not recorded','No dose recorded','Not wearing badge','No data'),9991,ifelse(grepl("n/a|N/A", base_treat$anesthesia),9999,base_treat$anesthesia))  %>% as.numeric()

base_treat$fluoro_time <- ifelse(grepl("n/a|N/A", base_treat$fluoro_sec),0,base_treat$fluoro_sec)  %>% as.numeric()


#longer format for mixed effect models 

baseline <- base_treat %>% pivot_longer(cols=22:26,names_to = 'personnel_type', values_to = "Exposure") %>% select(!c('tech1','resident1','resident2','anesthesia','TEE')) 
  


# It might not be a bad idea to get initial visibility into our categorical variables to see what we have 

# Frequency of techs per procedures

tech_procedure <-  table(baseline$procedure,baseline$personnel_type)

# Frequency of procedures  per type : we can see that BVP/BPV has the highest frequency of procedures, so this may have a larger impact on the overall effect

proced_freq <- table(base_treat_import$procedure)

# Descriptive Statistics of Weight And Fluroscopy Time Per procedure 
#mean, std dev, max, min group by procedure type
#Note that PMI is associated with a higher weight in the average, min, and max categories. And is aslso associated with a larger variance than 
#other procedure types
#This also observes the mean , max, min, and standard deviation of the exposure acrosss procedure types. We can see a great deal of variation in average mean exposure across procedure types.


wght_time_exp_stats <- baseline %>% filter(Exposure < 9000) %>% group_by(procedure) %>% 
  summarise(avg_weight = mean(weight),  avg_time = mean(fluoro_time), avg_exp = mean(Exposure),
              min_weight = min(weight), min_time =  min(fluoro_time), min_exp = min(Exposure),
             max_weight =  max(weight), max_time = max(fluoro_time), max_exp = max(Exposure),
              std_weight = sd(weight), std_time = sd(fluoro_time) , std_exp = sd(Exposure)
              )
print(wght_time_exp_stats)

#Now, we can see the distribution of the exposure for baseline sample. We can see that the distribution looks to be exponentially distributed rather than normal- but we could use bootstrapping to see the sampling distribution ( even though we may be able to just rely on the CLTbvbbvc)

base <- baseline %>% filter(group == 'control' & Exposure < 9000 )
  
hist1 <- ggplot(base)+ 
  geom_histogram(mapping=aes(x=Exposure), position="identity", bins = 50) + 
  labs(x ="Exposure Level", title =  "Counts for Exposure Level by Procedure Type") +
  facet_wrap(~procedure)

print(hist1)

#We can do the same for each technician type . We can see that the distributions are quite different for each techincian type, so it is understandable that we would want to see the 

hist2 <- ggplot(base)+ 
  geom_histogram(mapping=aes(x=Exposure), position="identity", bins = 50) + 
  labs(x ="Exposure Level", title =  "Counts for Exposure Level by Personnel Type") +
  facet_wrap(~personnel_type)

print(hist2)

#Scatter plot of exposure and weight by personel type for control. Doesn't seem to have much of any correlation for any of the personnel except for resident 1 - which I believe is closest to the animal during the procedure, which shows a posiitve correlation between weight and exposure.

scatter1 <- ggplot(base, aes(x=weight,y=Exposure)) +
  geom_point(position= "jitter") + 
  labs(x = "Weight", y= "Exposure", title= "Weight by Exposure by Personnel Type") +
  facet_wrap(~personnel_type)

print(scatter1)

#Scatter plot of exposure and weight by procedure type. It seems that BVP help the strongest correlation between weight and Expoure level 

scatter2 <- ggplot(base, aes(x=weight,y=Exposure)) +
  geom_point(position= "jitter") + 
  labs(x = "Weight", y= "Exposure", title= "Weight by Exposure by Procedure Type") +
  facet_wrap(~procedure)

print(scatter2)



#Scatter plot of fluorescopy time and exposure by personel type for control. 

scatter3 <- ggplot(base, aes(x=Exposure,y=fluoro_time)) +
  geom_point(position= "jitter") + 
  labs(x = "Exposure", y= "Fluroscopy Time", title= "Exposure by Fluorscopy Tiime by Personnel Type") +
  facet_wrap(~personnel_type)

print(scatter3)

#Scatter plot of exposure and weight by procedure type. It seems that BVP help the strongest correlation between fluroscopy time and Expoure level. However, we can see tha that the correlation does seem to differ across Procedure types

scatter4 <- ggplot(base, aes(x=weight,y=fluoro_time)) +
  geom_point(position= "jitter") + 
  labs(x = "Exposure", y= "Fluroscopy Time", title= "Exposure by Fluorscopy Time by Procedure Type") +
  facet_wrap(~procedure)

print(scatter4)






