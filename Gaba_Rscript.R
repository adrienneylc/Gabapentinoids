# title: "Multinational gabapentinoids trends: Data analysis/revision protocol_ Revision_20230426"

# README  
# Title of Proposed Research: Multinational gabapentinoids consumption: a longitudinal study  
# 
# This Statistical Analysis Plan describes definitions and outcomes in this study, which will evaluate the multinational trends and patterns of gabapentinoids consumption from 2008 to 2018.    
# 
# Principal investigator(s): Kenneth Man  
# Nominated main analyst(s): Adrienne Chan, Andrew Yuen    
# Nominated second/third independent analyst(s): Andrew Yuen    
# Publicly available on any websites? TBC  
# 
# Version 2: Gabapentinoids: Gabapentin, Pregabalin, Gabapentin enacarbil were included as drugs of interest.  
# Version 3: A sensitivity analysis was added to examine the effect of removing countries without complete data. 
# Version 4: Update to per tenthousand inhabitants  

# Background  
## Rationale of study    
# Previous literature has identified growth in the use of gabapentinoid alone and with opioids in US and the UK. However, consumption of gabapentinoids on a multinational level remains unclear. This study aims to characterise the trends of gabapentinoids consumption to inform multinational prescribing given the safety concerns and the implications of widespread use.  
# 
# Data used in this analysis:
#   1. IQVIA MIDAS dataset.  
# 2. Geographical region classification: United Nations Statistics Division. Standard country or area codes for statistical use (M49). 2021. https://unstats.un.org/unsd/methodology/m49/.  
# 3.	Population estimates: United Nations Population Division. World Population Prospects 2019. 2021. https://population.un.org/wpp/.  
# 4.	Country income classification: Worldbank uses GNI per capita: http://databank.worldbank.org/data/download/site-content/OGHIST.xlsx  
# 
# # Crosschecking list  
# 1.	Analytic dataset: Output_1_Analytic_dataset.csv (may skip as it is similar to 2.1)
# 2.	Consumption levels: Multinational, Regional, and National DDD/TID per year per drug with confidence interval  
# 2.1.	National DDD/TID by Year by Drug: Output_2_Gaba_consumption_CI.csv  
# 2.2.	Multinational & Regional DDD/TID by Year by Drug: Output_3_Pooled_Gaba_consumption_CI.csv  
# 2.3.	Income level DDD/TID by Drug: Output_6_Subgroup_AAPC_income_meta.csv  
# 2.4.	Sensitivity analysis 1: Output_7_Sensitivity_1_Pooled_Gaba_consumption_CI.csv  
# 2.5.	Sensitivity analysis 2: Output_9_Sensitivity2_Pooled_Gaba_consumption_CI_sens2.csv, Output_12_Sens2_Subgroup_AAPC_income_meta.csv  
# 2.6.	Sensitivity analysis 3: Output_13_Sensitivity_3_Pooled_Gaba_consumption_CI.csv, Output_16_Sensitivity_3_Subgroup_AAPC_income_meta.csv  
# 3.	Annual average percentage change: Multinational, Regional, National, by income level per drug  
# 3.1.	National & Regional & Multinational AAPC by Drug: Output_4_lm_cty_region_multi.csv  
# 3.2.	Income level AAPC by Drug: Output_5_Subgroup_AAPC_income.csv  
# 3.3.	Sensitivity analysis 1: Output_8_Sensitivity_1_lm_cty_region_multi.csv  
# 3.4.	Sensitivity analysis 2: Output_10_Sensitivity_2_lm_cty_region_multi.csv, Output_11_Sensitivity_2_Subgroup_AAPC_income.csv     
# 3.5.	Sensitivity analysis 3: Output_14_lm_cty_region_multi.csv, Output_15_Sensitivity_3_Subgroup_AAPC_income  


# Call/install R packages  
library(tidyverse)
library(broom)
library(ggiraphExtra)
library(ggpubr)
library(rstatix)
library(matrixTests)
library(sjPlot)
library(lme4)
library(Hmisc)
library(data.table)
library(magrittr)
library(tidyr)
library(haven)
library(Hmisc)
library(compare)
library(tidyverse)
library(stringr)
library(zoo)
library(directlabels)
library(ratesci) 
library(epitools)
library(metafor)
library(meta)
library(dplyr)
library(stringr)
```

# Prepare dataframe  
## Reading data  
# Data inclusion criteria: variable name "MOL"= "PREGABALIN","GABAPENTIN","GABAPENTIN ENACARBIL","MIROGABALIN"  

library(readxl)
library(dplyr)
# data <- read.csv("D:/MIDAS ADHD/midas_adhd/adhd/MIDAS 2019.csv")
# data
getwd()
setwd("D:/R/midas gaba")
gaba<-subset(read.csv("D:/R/midas benzo/raw/gaba_extract.csv"),MOL=="PREGABALIN"|
               MOL=="GABAPENTIN"|
               MOL=="GABAPENTIN ENACARBIL"|
               MOL=="MIROGABALIN")


## Drug inspection  
# Inspect data for combination product with more than one active molecule (Gabapentinoids)  
# The remaining products that may contain more than one drug of interest are:  INDIA TOTAL SALES	RETAIL	A.N.PHARMACIA	N3A ANTI-EPILEPTICS	NEURO-P  
# * only one record per active ingredient  

library(dplyr)
library(stringr)

data.1.nodup<-gaba

#Check for combination product i.e. other than drug name, all numbers are the same
exam2<-data.1.nodup[duplicated(data.1.nodup[,-(1:10)]) | duplicated(data.1.nodup[,-(1:10)], fromLast = TRUE),]
exam2
gaba$Class<-"Gabapentinoids"

# We assume that the DDD units provided only refer to that specific drug ingredient in the data row   

## Keep SU and DDD data 

master<-gaba


## Separate data into with DDD units and without DDD units (INTWHODDDDESC NOT ASSIGNED)  
# 1) For products without DDD data, the conversion ratio is based on standard units sold, WHO-ATC defined daily dose, and product strength (539 observations)    
# 2) For products without drug strength AND DDD, we will impute the conversion ratio based on the most commonly used strength    
# 
# Two pharmacists have assigned strength of products without DDD units for conversion SU units.  
# 667 observations were read from the conversion file for gabapentinoids since a product may have more than one strength. 
# Median imputation was conducted for products with more than one identified strength. 470/539 observations with strengths identified after median imputation.    
# - Liquid formulations were removed (15/539 unique metadata combination in missing DDD file)  
# - Strength information of 54/539 country-product combinations cannot be identified  


library(dplyr)

#Identify products that need conversion from SU to INTDDD
missing_DDD<-subset(master,INTWHODDDDESC=="INTWHODDD DESC NOT ASSIGNED")
with_DDD<-subset(master,INTWHODDDDESC!="INTWHODDD DESC NOT ASSIGNED")

# #Export list for conversion factor identification
# miss_DDD<-unique(missing_DDD[c(1:8)])
# write.csv(miss_DDD,"D:/R/midas benzo/dosage_convert.csv")

#Daniel and Andrew has done the conversion
convert <- read.table(
  "D:/R/midas benzo/convert_20221114_removed.txt",
  sep="\t", header=TRUE)
convert2<-subset(convert, !is.na(convert$Strength.of.BZD.or.GABA) & (MOL=="PREGABALIN"|
                                                                       MOL=="GABAPENTIN"|
                                                                       MOL=="GABAPENTIN ENACARBIL"|
                                                                       MOL=="MIROGABALIN"))
convert_missing<-subset(convert, is.na(convert$Strength.of.BZD.or.GABA))

convert_medianimpute_strength <- convert2[c(1:10)] %>% group_by (CTY,SEC, MNF, ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,INTWHODDDDESC_2) %>% mutate(median=median(Strength.of.BZD.or.GABA, na.rm = TRUE)) 
convert_medianimpute_strength2<-unique(convert_medianimpute_strength[c(1:8,10,11)])
#left join convert to data
missing_DDD_convert <- left_join(missing_DDD,convert_medianimpute_strength2,by = c("CTY","SEC","MNF","ATC3","INTPRD","NFC123",  
                                                                                   "INTWHODDDDESC","MOL") )
write.csv(missing_DDD,"D:/R/midas gaba/withoutddd_539.csv")
missing_DDD_remain<-subset(missing_DDD_convert, !is.na(median) & Class=="Gabapentinoids")


missing_DDD_agg<-missing_DDD %>% group_by (CTY, SEC,MNF,ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,X_TYPE_,X_FREQ_,Class) %>% 
  summarise_at(vars(1:857),sum)

## Results of median imputed strengths:  

library(DT)
datatable(subset(convert_medianimpute_strength2), options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



## Aggregate separately for DDD and missing DDD dataframes 
### DDD

library(tidyr)
library(zoo)
library(dplyr)


ddd <-with_DDD

## Remove columns of other sales data e.g. LC, LCD, USD, CU, SU
ddd.2 <- ddd[-c(9:10)]%>%select(-contains(c("LC_MNF","LCD_MNF","USD_MNF","CU_MTH","SU_MTH")))
ddd.3 <- pivot_longer(ddd.2, cols=9:151, 
                      names_to = "Month", values_to = "DDD")
## Format date
ddd.3$Month<-str_remove(ddd.3$Month, "INTDDD_MTH_")
ddd.3<-ddd.3 %>% separate(Month, c("Month","Year"), 
                          sep = "([_])")
ddd.3$Year <- paste0("20", ddd.3$Year, sep = "")
ddd.3$Year<-as.numeric(ddd.3$Year)
ddd.3$Month<-as.numeric(ddd.3$Month)
ddd.3$Date <- as.yearmon(paste(ddd.3$Year, 
                               ddd.3$Month), "%Y %m")
ddd.4<-ddd.3[-c(10)]
ddd.4$DDD[is.na(ddd.4$DDD)]<-0


### Without DDD  
# Check formula: DDD units = SU units *Strength/INTWHODDDDESC  

library(tidyr)
library(zoo)
library(dplyr)

noddd <-missing_DDD_remain

## Remove columns of other sales data e.g. LC, LCD, USD, CU, SU
noddd.2 <- noddd[-c(9:10)]%>%select(-contains(c("LC_MNF","LCD_MNF","USD_MNF","CU_MTH","INTDDD_MTH")))
noddd.3 <- pivot_longer(noddd.2, cols=9:151, 
                        names_to = "Month", values_to = "SU")
## Calculate DDD
noddd.3$DDD<-noddd.3$SU*noddd.3$median/noddd.3$INTWHODDDDESC_2

## Format date
noddd.3$Month<-str_remove(noddd.3$Month, "SU_MTH_")
noddd.3<-noddd.3 %>% separate(Month, c("Month","Year"), 
                              sep = "([_])")
noddd.3$Year <- paste0("20", noddd.3$Year, sep = "")
noddd.3$Year<-as.numeric(noddd.3$Year)
noddd.3$Month<-as.numeric(noddd.3$Month)
noddd.3$Date <- as.yearmon(paste(noddd.3$Year, 
                                 noddd.3$Month), "%Y %m")
noddd.4<-noddd.3
noddd.4$DDD[is.na(noddd.4$DDD)]<-0


## Aggregate the consumption data by country, drug name, date  


ddd.5<-ddd.4
noddd.5<-noddd.4[-c(10,11,12,14)]

bind<-rbind(ddd.5,noddd.5)

aggregate<-bind[-c(2:7)]
# Clean city names
aggregate$CTY = sub(pattern = "(RETAIL)|(COMBINED)|(COMBINE)|(COMBIN)|(COMBI)|(RET)|(R.MUTUALES)|(HOSPITAL)|(TOTAL SALES)", 
                    replacement = "", x = aggregate$CTY, perl = TRUE)
aggregate.2<-aggregate %>% group_by (CTY, MOL,Class,Year, Date) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)



## Merge rx data with UN population data  

# We excluded data where Country=="FRENCH WEST AFRICA" or Country=="CENTRAL AMERICA" as national level data was not available. The included countries/regions were divided into the following areas: Northern America, Central and South America, Northern Europe, Eastern Europe, Southern Europe, Western Europe, Oceania, Central Asia, Eastern Asia, South-eastern Asia, Southern Asia, Western Asia, Northern Africa, and Southern Africa, based on their geographical regions according to United Nations (UN)’ "Standard Country or Area Codes for Statistical Use".     

library(readxl)
library(dplyr)
#population size from UN====
pop <- read_excel(path = "D:/R/midas adhd/ref data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                  sheet = "ESTIMATES",
                  range = "B17:DE18122")
pop.2<-subset(pop, Type=="Country/Area")
pop.4<-pop.2[-c(1,3:63,77:108)]
names(pop.4)[names(pop.4) == 'Region, subregion, country or area *'] <- 'Country'

pop.5 <- pivot_longer(pop.4, cols=2:14,
                      names_to = "Year", values_to = "Population")

pop.5$Country <- toupper(pop.5$Country)
names(aggregate.2)[names(aggregate.2) == 'CTY'] <- 'Country'
aggregate.2$Country <-trimws(aggregate.2$Country)
'%ni%' <- Negate('%in%')

ddd.set.2<- subset(aggregate.2, !Country=="C. AMERICA"&
                     !Country=="FR. W. AFRICA"&
                     !Country=="FRENCH WEST AFRICA"&
                     !Country=="CENTRAL AMERICA")
pop.7<-pop.5
rename <- pop.7 %>%filter(Country %ni% ddd.set.2$Country) 
rename<-unique(rename$Country)

ddd.set.2[ddd.set.2$Country == "CZECH", "Country"] <- "CZECH REPUBLIC"
ddd.set.2[ddd.set.2$Country == "NETHERLNDS", "Country"] <- "NETHERLANDS"
ddd.set.2[ddd.set.2$Country == "RUSSIAN FED.", "Country"] <- "RUSSIA"
ddd.set.2[ddd.set.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
ddd.set.2[ddd.set.2$Country == "UAE", "Country"] <- "UNITED ARAB EMIRATES"
ddd.set.2[ddd.set.2$Country == "UK", "Country"] <- "UNITED KINGDOM"
ddd.set.2[ddd.set.2$Country == "USA", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "US", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "S. AFRICA", "Country"] <- "SOUTH AFRICA"
ddd.set.2[ddd.set.2$Country == "BOSNIA", "Country"] <- "BOSNIA AND HERZEGOVINA"
ddd.set.2[ddd.set.2$Country == "KOREA", "Country"] <- "SOUTH KOREA"
pop.7<-pop.5
pop.7[pop.7$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
pop.7[pop.7$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
pop.7[pop.7$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
pop.7[pop.7$Country == "TURKEY", "Country"] <- "TÜRKİYE"
pop.7[pop.7$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
pop.7[pop.7$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
pop.7[pop.7$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"

pop.8<-as.data.frame(pop.7)
ddd.set.2$Year<-as.numeric(ddd.set.2$Year)
pop.8$Year<-as.numeric(pop.8$Year)
ddd.set.3.1<-ddd.set.2
ddd.set.3 <- left_join(ddd.set.3.1,pop.8,by = c("Country","Year") )

## Merge data with Region categorisation====
Region <- read_excel(path = "D:/R/midas adhd/ref data/UNSD — Methodology.xlsx")
Region.2<-Region[c(6,9,16)]
names(Region.2)[names(Region.2) == 'Country or Area'] <- 'Country'
Region.2$Country <- toupper(Region.2$Country)
rename <- ddd.set.3%>%filter(Country %ni% Region.2$Country) 
rename<-unique(rename$Country)
Region.2[Region.2$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
Region.2[Region.2$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
Region.2[Region.2$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
Region.2[Region.2$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
Region.2[Region.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
Region.2[Region.2$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
Region.2[Region.2$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"
Region.2[Region.2$Country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "Country"] <- "UNITED KINGDOM"

Region.3<-Region.2%>%filter(Country %in% ddd.set.3$Country) 
ddd.set.4 <- left_join(ddd.set.3,Region.3,by = c("Country") )
names(ddd.set.4)[names(ddd.set.4) == 'Sub-region Name'] <- 'Region'
ddd.set.4[ddd.set.4$Country == "TAIWAN", "Region"] <- "Eastern Asia"

ddd.set.5<-ddd.set.4[-c(9)]

## 1.9 Dave comment: change Latin America 
ddd.set.5[ddd.set.5$Region == "Latin America and the Caribbean", "Region"] <- "Central and South America and the Caribbean"
ddd.set.5[ddd.set.5$Region == "Sub-Saharan Africa", "Region"] <- "Southern Africa"

population<-pop.8
population$Population<-as.numeric(population$Population)

options(scipen=999)
```

## Final analytic dataset  
# - Data from 2007 and 2019 were removed due to missing months  
# - DDD data were aggregated by Drug, Country, Year
# - DDD data were combined with population data  
# - zeros were added to countries were there were no record in that year  
# - Outcome metric = DDD per thousand population per day (DDD/(population in thousand*365.25))  
# - Final cleaned analytic dataset is cleaned as below with variables specification:  

library(dplyr)
cty.all<-ddd.set.5

names(cty.all)[names(cty.all) == 'MOL'] <- 'Drug'
## create one df of monthly data just in case
cty.all.3<-cty.all[-c(7)] %>% group_by (Year,Date,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)

## numerator
cty.ddd.2<-cty.all[-c(5,7)] %>% group_by (Year,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
##denominator
cty.pop<-population

#Aggregate benzos and gaba
#gaba
new_gaba<-subset(cty.ddd.2)
new_gaba<-new_gaba[-c(3)]
new_gaba<-new_gaba %>% group_by (Year,Country, Region,Class) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
new_gaba$Drug<-"Gabapentinoids"
new_gaba$DDD[is.na(new_gaba$DDD)]<-0
cty.ddd.2<-rbind(cty.ddd.2,new_gaba)

#Add zeros
Drug<-sort(rep(c("Gabapentinoids","MIROGABALIN","PREGABALIN","GABAPENTIN","GABAPENTIN ENACARBIL"),715))
Country<-(rep(unique(cty.ddd.2$Country),11))
Year<-sort(rep(c(2008:2018),65))

merge_zero <- data.frame(Country, Year)
merge_zero2<-do.call("rbind", replicate(5, merge_zero, simplify = FALSE))
merge_zero2$Drug<-Drug
cty.ddd.3 <- right_join(x=cty.ddd.2,y=merge_zero2, 
                        by=c("Year","Country","Drug"))

cty.ddd.4 <- left_join(x=cty.ddd.3,y=cty.pop, 
                       by=c("Year","Country"))

cty.ddd.5 <- distinct(left_join(x=cty.ddd.4[-c(2,5)],y=ddd.set.5[c(1,3,8)], 
                                by=c("Country")))

cty.ddd.5$DDD[is.na(cty.ddd.5$DDD)]<-0

#Divide
cty.ddd.5$DDDPTPD <-(cty.ddd.5$DDD/cty.ddd.5$Population)/365.25
cty.ddd.5$Drug<-str_to_title(cty.ddd.5$Drug)
analy<-subset(cty.ddd.5,Year!=2007 & Year!=2019 & Drug!="Vigbatrin"&Drug!="Mirogabalin")

library(DT)
datatable(analy, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

str(analy,give.attr=FALSE)
write.csv(analy,"D:/R/midas gaba/R1/Output_1_Analytic_dataset.csv")





# Main Analysis: DDD/TID and trends     

## Overview of gabapentinoids consumption by Drug, Country, and Year   
# *MIROGABALIN is only available in Japan in 2019 which is not covered in our analysis

library(ggplot2)
library(RColorBrewer)
new_set.2<-analy
stacked<-subset(new_set.2, Drug!="Gabapentinoids" )
stacked$Country<-toupper(stacked$Country)
stacked[
  order( stacked[,2] ,stacked[,3]),
]

stacked$Drug <- factor(stacked$Drug)
stacked$Region <- factor(stacked$Region, levels =
                           c("Global","Northern America","Central and South America and the Caribbean",
                             "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                             "Oceania" ,
                             "Eastern Asia" ,
                             "South-eastern Asia","Southern Asia" ,
                             "Western Asia","Central Asia",
                             "Northern Africa", "Southern Africa"
                           ))
a<-ggplot(stacked[order(stacked$Drug), ], aes(x = str_to_title(Country), y = DDDPTPD*10, fill = Drug)) +
  geom_col(alpha = 0.8, width = 0.9) +
  scale_y_continuous(expand = c(0, 0.1)) +
  scale_x_discrete(limits=rev)+
  coord_flip() +
  facet_grid(rows = vars(Region), cols = vars(Year),
             scales = "free_y", switch = "y", 
             space = "free_y", 
             labeller = label_wrap_gen(width=20)) +
  labs(
    title = "Gabapentinoids consumption from 2008 to 2018",
    subtitle = "in 65 countries",
    caption = "Figure",
    y = "Defined Daily Dose per 10000 inhabitants per day"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#EC2049", "#DCEDC2", "#2F9599"))+
  #  scale_fill_brewer(palette = "Dark2")+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    panel.spacing.x = unit(1, "lines"),
    # strip.text.y = element_text(angle = 270, face = "bold"),
    strip.text.y.left = element_text(angle = 0,face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x=element_text(angle=270),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )
a

ggsave(filename="Figure 1_Consumption by Year Country Drug.png", plot=a, height =13, width=16,device="png", 
       path="D:/R/midas gaba/R1/figures",
       dpi=800)



## Calculate CIs of consumption in DDD/TID per year  
# The function pois.approx from R package epitools is used to calculation the Confidence intervals for Poisson rates with normal approximation   

library(epitools)
CIs<-pois.approx(new_set.2$DDD, pt = new_set.2$Population*365.25, conf.level = 0.95)
pool.new_set<-cbind(new_set.2,CIs)
pool.new_set.2<-subset(pool.new_set)
pool.new_set.2$DDD<-round(pool.new_set.2$DDD, digits = 5)  
write.csv(pool.new_set.2,"D:/R/midas gaba/R1/Output_2_Gaba_consumption_CI.csv")
writexl::write_xlsx(pool.new_set.2,"D:/R/midas gaba/R1/Output_2_Gaba_consumption_CI.xlsx")
```


## Meta-analysis of DDD/TID by Year  
# When DDD=0 or extremely small, Country data wont be counted as number of studies in the meta analysis  
# 
# Drug indices: [1] "Gabapentin", [2] "Gabapentin Enacarbil", [3] "Pregabalin", [4]"Gabapentinoids"   

library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<-subset(analy, Drug!="MIROGABALIN")
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$Region <- factor(meta.gaba$Region, levels =
                             c("Northern America","Central and South America and the Caribbean",
                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                               "Australia and New Zealand" ,
                               "Eastern Asia" , "Central Asia",
                               "South-eastern Asia","Southern Asia" ,
                               "Western Asia", 
                               "Northern Africa","Southern Africa"
                             ))

meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$Region)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  est.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.random$Year<-rho
  est.random$Drug<-iseed
  est.random$`DDD/TID`<-exp(summary(m1_var)$TE.random)
  est.random$`DDD/TID - lower`<-exp(summary(m1_var)$lower.random)
  est.random$`DDD/TID - upper`<-exp(summary(m1_var)$upper.random)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  return(c(est.random,est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

#Multinational
gaba_global2<-as.data.frame(do.call(rbind, datout))[c(5:9)] 
gaba_global2$Year<-as.numeric(gaba_global2$Year)
gaba_global2$`DDD/TID`<-as.numeric(gaba_global2$`DDD/TID`)
gaba_global2$`DDD/TID - lower`<-as.numeric(gaba_global2$`DDD/TID - lower`)
gaba_global2$`DDD/TID - upper`<-as.numeric(gaba_global2$`DDD/TID - upper`)
gaba_global2$Area<-"Multinational"

gaba_global2$Drug<-c(rep(("Gabapentin"), 11),
                     rep(("Gabapentin Enacarbil"), 11),
                     rep(("Pregabalin"), 11),
                     rep(("Gabapentinoids"), 11))

gaba_global3<-subset(gaba_global2,Drug!="MIROGABALIN")

a<-ggplot(gaba_global3, aes(x = Year, y = `DDD/TID`*10, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  geom_ribbon(aes(ymin = `DDD/TID - lower`*10, ymax = `DDD/TID - upper`*10), alpha = 0.1, colour = NA) + 
  scale_x_continuous(breaks = c(2008:2018))+
  ggtitle("Pooled multinational gabapentinoids consumption over time")+
  ylab("Defined daily dose per 10000 inhabitants per day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
a
ggsave(filename="Figure_2_pooled_by year drug.png", plot=a, height =7, width=11,device="png", 
       path="D:/R/midas gaba/R1/figures",
       dpi=500)

##Regional
gaba_regional<-as.data.frame(do.call(rbind, datout))[c(14:18)] 

gaba_regional$Year<-as.numeric(gaba_regional$Year)
out <- unlist(gaba_regional)
Year<-out[1:44]
Drug<-out[45:88]
DDDTID<-out[89:704]
lower<-out[705:1320]
upper<-out[1321:1936]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(14, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper

region_name<-(rep(c("Northern America","Central and South America and the Caribbean",
                    "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                    "Australia and New Zealand" ,
                    "Eastern Asia" , "Central Asia",
                    "South-eastern Asia","Southern Asia" ,
                    "Western Asia", 
                    "Northern Africa","Southern Africa"),44))
newdata$Area<-region_name
newdata$Drug<-c(rep(("Gabapentin"), 154),
                rep(("Gabapentin Enacarbil"), 154),
                rep(("Pregabalin"), 154),
                rep(("Gabapentinoids"), 154))
gaba_regional2<-newdata
gaba_regional2$`DDD/TID`<-gaba_regional2$DDDTID
gaba_regional2$`DDD/TID - lower`<-gaba_regional2$lower
gaba_regional2$`DDD/TID - upper`<-gaba_regional2$upper

gaba_regional3<-gaba_regional2[c(1,2,6:9)]
pool_Global_region<-rbind(gaba_global3,gaba_regional3)
pool_Global_region$Drug<- factor(pool_Global_region$Drug, levels = c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
pool_Global_region$Area<-factor(pool_Global_region$Area,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                                                 "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                                                 "Australia and New Zealand" ,
                                                                 "Eastern Asia" , "Central Asia",
                                                                 "South-eastern Asia","Southern Asia" ,
                                                                 "Western Asia", 
                                                                 "Northern Africa","Southern Africa"))
pool_Global_region$Year<-as.numeric(pool_Global_region$Year)
pool_Global_region[is.na(pool_Global_region)]<-0
library(ggbreak)
a<-ggplot(pool_Global_region, aes(x = Year, y = `DDD/TID`, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  facet_wrap(.~Area,nrow=1)+
  geom_ribbon(aes(ymin = `DDD/TID - lower`, ymax = `DDD/TID - upper`), alpha = 0.1, colour = NA) +
  scale_y_break(c(20,62),scales = 0.1)+
  theme_classic()+
  scale_x_continuous(breaks = c(2008:2018))+ 
  theme(panel.border=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line=element_line(),axis.ticks.x = element_blank())+
  ggtitle("Pooled multinational gabapentinoids consumption over time")
a
ggsave(filename="Reference_1_pooled_by Region year drug.png", plot=a, height =7, width=40,device="png", 
       path="D:/R/midas gaba/R1/figures",
       dpi=500)
write.csv(pool_Global_region,"D:/R/midas gaba/R1/Output_3_Pooled_Gaba_consumption_CI.csv")

library(DT)
datatable(pool_Global_region[c(6,1:5)], options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

## Visualisation: Geographical distribution of gabapentinoids use for 2008 and 2018  
### 2008

library(ggplot2)
library(dplyr)
library(maps)
library(viridis)
theme_set(
  theme_grey()
)
library(mapproj)


map.1<-subset(pool.new_set.2, Year==2008 & Drug=="Gabapentinoids")

world_map <- map_data("world")
# ggplot(world_map, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill="lightgray", colour = "white")
world_map.2 <-world_map
setDF(world_map.2)
world_map.2$region <- toupper(world_map.2$region)

# map.1$Country %in% world_map.2$region
# sort(unique(world_map.2$region))
world_map.2$region[world_map.2$region == "UK"] <- "UNITED KINGDOM"
world_map.2$region[world_map.2$region == "USA"] <- "UNITED STATES"
world_map.2$region[world_map.2$region == "TURKEY"] <- "TÜRKİYE"
names(map.1)[names(map.1) == "Country"] <- "region"

map.2 <- right_join(map.1, world_map.2)


#change to categorical
map.2015 <- map.2 %>% mutate(category=cut(DDDPTPD*10, 
                                          breaks=c(-Inf, 
                                                   0.01,
                                                   0.1,
                                                   1,
                                                   10,50,
                                                   100,
                                                   200,
                                                   Inf), 
                                          labels=c("0-0.01",
                                                   ">0.01-0.1",
                                                   ">0.1-1",
                                                   ">1-10",
                                                   ">10-50",
                                                   ">50-100",
                                                   ">100-200",
                                                   ">200")))

y<-ggplot(map.2015, group = group)+
  geom_map(map = map.2015,  color = "black", 
           aes(map_id = region, fill = category, group = group))+
  expand_limits(x = map.2015$long, y = map.2015$lat)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'))+
  guides(fill=guide_legend(title="DDD/TID"))+
  scale_fill_viridis(
    option="turbo",drop=FALSE,
    name = "Defined Daily Dose per capita per day",
    discrete = T,
    direction = +1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = FALSE
    ))+
  ggtitle("2008")
y

ggsave(filename="Figure 3a_map 2008_gaba.png", plot=y, height =7, width=11,device="png", 
       path="D:/R/midas gaba/R1/figures",
       dpi=500)


### 2018  
```{r message=FALSE, warning=FALSE}
map.1<-map.1<-subset(pool.new_set.2, Year==2018 & Drug=="Gabapentinoids")
world_map <- map_data("world")
# ggplot(world_map, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill="lightgray", colour = "white")
world_map.2 <-world_map
setDF(world_map.2)
world_map.2$region <- toupper(world_map.2$region)

# map.1$Country %in% world_map.2$region
# sort(unique(world_map.2$region))
world_map.2$region[world_map.2$region == "UK"] <- "UNITED KINGDOM"
world_map.2$region[world_map.2$region == "USA"] <- "UNITED STATES"
world_map.2$region[world_map.2$region == "TURKEY"] <- "TÜRKİYE"
names(map.1)[names(map.1) == "Country"] <- "region"
map.2 <- right_join(map.1, world_map.2)

#change to categorical
map.2019 <- map.2 %>% mutate(category=cut(DDDPTPD*10, 
                                          breaks=c(-Inf, 
                                                   0.01,
                                                   0.1,
                                                   1,
                                                   10,50,
                                                   100,
                                                   200,
                                                   Inf), 
                                          labels=c("0-0.01",
                                                   ">0.01-0.1",
                                                   ">0.1-1",
                                                   ">1-10",
                                                   ">10-50",
                                                   ">50-100",
                                                   ">100-200",
                                                   ">200")))

y<-ggplot(map.2019, group = group)+
  geom_map(map = map.2019,  color = "black", 
           aes(map_id = region, fill = category, group = group))+
  expand_limits(x = map.2019$long, y = map.2019$lat)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white'))+
  scale_fill_viridis(
    option="turbo", drop=FALSE,
    name = "Defined Daily Dose per capita per day",
    discrete = T,
    direction = +1,
    guide = guide_legend(title="DDD/TID",
                         keyheight = unit(5, units = "mm"),
                         title.position = 'top',
                         reverse = FALSE
    ))+
  ggtitle("2018")


y


ggsave(filename="Figure 3b_map 2018_gaba.png", plot=y, height =7, width=11,device="png", 
       path="D:/R/midas gaba/R1/figures",
       dpi=500)


## Calculate percentage of regions   
##National
library(dplyr)
region_percent_gaba<-pool.new_set.2 %>% filter(Year==2018 & Drug=="Gabapentinoids")
region_percent_gaba$percent<-region_percent_gaba$DDDPTPD/sum(region_percent_gaba$DDDPTPD)

library(DT)

datatable(region_percent_gaba[,c(6,3,15)], caption="Gabapentinoids", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

region_percent_gaba<-pool.new_set.2 %>% filter(Year==2018 & Drug=="Gabapentinoids")
region_percent_gaba$percent<-region_percent_gaba$DDDPTPD/sum(region_percent_gaba$DDDPTPD)

##Regional

region_percent_gaba[c(7,8)] %>% 
  dplyr::group_by(Region) %>% dplyr::mutate(sumDDDPTPD = sum(region_percent_gaba$DDDPTPD))

library(data.table)
setDT(region_percent_gaba)  # set the data frame as data table
as<-region_percent_gaba[, list(sumDDDPTPD = sum(DDDPTPD, na.rm=F)),
                        by=list(Region)]  


region_2018<-as %>%mutate(percent = prop.table(sumDDDPTPD))
datatable(region_2018, caption="Gabapentinoids", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))


## Average annual percentage change   
# Data entries with DDD=0 were removed as they do not contribute to average annual percentage change where measures changes between study years. 
# 2106 observations after removing DDD=0  

### Data view - showing which zeros were removed    

library(plyr)
library(Hmisc)
library(DT)
library(tibble)

lm.cty_gaba<-subset(pool.new_set.2)
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

datatable(lm.cty_gaba,  options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

### National level  

options(width = 30)
library(plyr)
library(Hmisc)
library(DT)
library(tibble)


lm.cty_gaba_2<-subset(lm.cty_gaba, DDD>0)

models <- dlply(lm.cty_gaba_2, .(Country,Drug), function(df) 
  lm(log(DDDPTPD) ~ Year, data = df))

coef<-sapply(models, function(df) summary(df)$coefficients[2])
lm.results<-data.frame(coef)

ad_extract_ci <- function(x){
  temp_lw <- confint.lm(x)[2,1]
  temp_up <- confint.lm(x)[2,2]
  return(c(temp_lw,temp_up))
}

lower<-unlist(lapply(lapply(models,ad_extract_ci),"[",1))
lm.results<-cbind(lm.results, lower)
upper<-unlist(lapply(lapply(models,ad_extract_ci),"[",2))
lm.results<-cbind(lm.results, upper)
pvalue<-sapply(models, function(df) summary(df)$coefficients[8])
lm.results<-cbind(lm.results, pvalue)
lm.results<-rownames_to_column(lm.results)
colnames(lm.results)[which(names(lm.results) == "rowname")] <- "Country"
lm.results$expcoef<-(exp(lm.results$coef)-1)*100
lm.results$explower<-(exp(lm.results$lower)-1)*100
lm.results$expupper<-(exp(lm.results$upper)-1)*100

lm.results2<-(lm.results) %>% separate(Country, c("Country", "Drug"), sep="[.]")


cty.lm.results<-distinct(left_join(lm.results2,pool.new_set.2[c(2,3,7)]))






### Global  
# Global_model_1 is a two level (individual countries) random intercepts model with AR1.
# 
# Examples:  
#   #random intercept
#   fitRandomIntercept <-      lme(value "~" time,random= "~" 1|subject,data=repeatdatax)
# 
#random intercept and slope
# fitRandomInterceptSlope <- lme(value"~"time,random="~"1+time|subject,data=repeatdatax)
# 
# Note to self: 
#   1. The lme function of nlme is used instead of lme4 as handling autocorrelation is more straightforward with nlme; specfically, "in nlme, it is possible to specify the variance-covariance matrix for the random effects (e.g. an AR(1)); it is not possible in lme4.". Read: https://stats.stackexchange.com/questions/5344/how-to-choose-nlme-or-lme4-r-library-for-mixed-effects-models  
# 
# 2. Intercept 1+ is included implicitly.
# 
# 3. The results of Global_model_1 can be reproduced by the follwo SAS codes:  
#   proc sort data=ana.globe;
# by   Country year;run;
# 
# proc glimmix data=ana.globe  
# plots=residualpanel(conditional marginal);;  
# class Country yearn ;  
# model lnDDDPTPD = year /solution cl ;  
# random intercept /subject=Country;  
# random yearn/subject=Country type=AR(1) residual;  
# *random _residual_ /subject=Country type=ar(1) ;    
# ods output ParameterEstimates = _es_globe;  
# run;  
# quit;  
# 
# data ana.global_est_gaba;set _es_globe;  
# if effect='Year';  
# DDDPTPD_change=(exp (Estimate)-1)*100;  
# change_lcl=(exp (Lower)-1)*100;  
# change_ucl=(exp (Upper)-1)*100;  
# run;  


library(plyr)
library(lme4)
library(nlme)
# 
# mixed_results_Global<-data.frame()
# lm.cty_gaba_x<-split(lm.cty_gaba_2, list(lm.cty_gaba_2$Drug))
# for (i in 1:4){
# Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
# correlation = corAR1(form = ~ Year|Country), data = as.data.frame(lm.cty_gaba_x[i], col.names = c("")))
# summary(Global_model_1)
# mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
# mixed_est_2 <- data.frame(mixed_est$fixed)
# mixed_pval<-anova(Global_model_1)
# mixed_pval<-as.data.frame(mixed_pval)
# mixed_results_1<-cbind(mixed_est_2,mixed_pval)
# mixed_results_1$Model<-"Global_country_int_wAR1"
# mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
# mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
# mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100
# 
# mixglo<-rbind(mixed_results_1)
# mixglo$Drug<-c(unique(lm.cty_gaba$Drug)[i])
# mixed_results_Global<-rbind(mixed_results_Global,mixglo)
# }
# 
# datatable(mixed_results_Global[c(8:12,7,1:6)], options = list(
#   autoWidth = TRUE,
#   columnDefs = list(list(width = '100px', targets = c(1, 3)))
# ))


asd<-subset(lm.cty_gaba_2, Drug=="Gabapentinoids")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_1<-cbind(mixed_est_2,mixed_pval)
mixed_results_1$Drug<-"Gabapentinoids"
mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Gabapentin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_2<-cbind(mixed_est_2,mixed_pval)
mixed_results_2$Drug<-"Gabapentin"
mixed_results_2$expcoef<-(exp(mixed_results_2$est.)-1)*100
mixed_results_2$explower<-(exp(mixed_results_2$lower)-1)*100
mixed_results_2$expupper<-(exp(mixed_results_2$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Pregabalin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_3<-cbind(mixed_est_2,mixed_pval)
mixed_results_3$Drug<-"Pregabalin"
mixed_results_3$expcoef<-(exp(mixed_results_3$est.)-1)*100
mixed_results_3$explower<-(exp(mixed_results_3$lower)-1)*100
mixed_results_3$expupper<-(exp(mixed_results_3$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Gabapentin Enacarbil")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_4<-cbind(mixed_est_2,mixed_pval)
mixed_results_4$Drug<-"Gabapentin Enacarbil"
mixed_results_4$expcoef<-(exp(mixed_results_4$est.)-1)*100
mixed_results_4$explower<-(exp(mixed_results_4$lower)-1)*100
mixed_results_4$expupper<-(exp(mixed_results_4$upper)-1)*100


mixed_results_Global<-rbind(mixed_results_1,mixed_results_2,mixed_results_3,mixed_results_4)
datatable(mixed_results_Global, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

#### Global model diagnostics (ACF, PACF of with and without AR)
# ACF	R Documentation
# Autocorrelation Function (https://rdrr.io/cran/nlme/man/ACF.html)  


# plot(nlme::ACF(Global_model_1, maxLag = 10), alpha = 0.05, resType = "normalized", main="ACF CountrywAR1")
# plot(nlme::ACF(Global_model_woAR, maxLag = 10), alpha = 0.05, resType = "normalized", main="ACF CountrywoAR1")
# 
# #https://rstudio-pubs-static.s3.amazonaws.com/5643_0700c64117454e2ab3a2a5aef1497ede.html
# pacf(residuals(Global_model_1,type="normalized"))
# pacf(residuals(Global_model_woAR,type="normalized"))


### Regional  
#### Gabapentinoids  

library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(lm.cty_gaba_2$Region),my_update_function)
gabapentinoids.mlm_region<-rbindlist(lapply(unique(lm.cty_gaba_2$Region),
                                            my_update_function))
gabapentinoids.mlm_region<-as.data.frame(gabapentinoids.mlm_region)
gabapentinoids.mlm_region$Drug<-"Gabapentinoids"


#### Gabapentin  

library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
gabapentin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
gabapentin.mlm_region<-as.data.frame(gabapentin.mlm_region)
gabapentin.mlm_region$Drug<-"Gabapentin"



#### Gabapentin enacarbil  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentin Enacarbil")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
enacarbil.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                       my_update_function))
enacarbil.mlm_region<-as.data.frame(enacarbil.mlm_region)
enacarbil.mlm_region$Drug<-"Gabapentin Enacarbil"



#### Pregabalin  
```{r message=FALSE, warning=FALSE,out.width='120%', results='hide'}
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
pregabalin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
pregabalin.mlm_region<-as.data.frame(pregabalin.mlm_region)
pregabalin.mlm_region$Drug<-"Pregabalin"
region_reg_mlm<-rbind(gabapentinoids.mlm_region,gabapentin.mlm_region, enacarbil.mlm_region, pregabalin.mlm_region)


### Result table    


bind_cty_lm<-cty.lm.results[c(2,10,1,7:9,6)]
names(bind_cty_lm)[7]<-"p-value"

bind_reg_lm<-region_reg_mlm
bind_reg_lm$Country<-bind_reg_lm$Model
bind_reg_lm$Region<-bind_reg_lm$Model
bind_reg_lm<-bind_reg_lm[c(12,14,13,9:11,7)]

bind_glo_lm<-mixed_results_Global[c(2,4,6,8),]
bind_glo_lm<-bind_glo_lm[c(8:11,7)]
bind_glo_lm$Country<-"Multinational"
bind_glo_lm$Region<-"Multinational"
bind_glo_lm<-bind_glo_lm[c(7,6,1:5)]
bind_lm<-rbind(bind_cty_lm,bind_reg_lm,bind_glo_lm)
write.csv(bind_lm,"D:/R/midas gaba/R1/Output_4_lm_cty_region_multi.csv")

datatable(bind_lm, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))


### Visualisation: Average annual percentage chnage of gabapantinoids - Multinational, Regional, National  

library(dplyr)
library(ggplot2)

bind_lm$Country<- factor(bind_lm$Country, levels = c("Multinational",
                                                     "Northern America",
                                                     "CANADA",
                                                     "UNITED STATES",
                                                     "Central and South America and the Caribbean",
                                                     "ARGENTINA",
                                                     "BRAZIL",
                                                     "CHILE",
                                                     "COLOMBIA",
                                                     "ECUADOR",
                                                     "MEXICO",
                                                     "PERU",
                                                     "PUERTO RICO",
                                                     "URUGUAY",
                                                     "VENEZUELA",
                                                     "Northern Europe",
                                                     "ESTONIA",
                                                     "FINLAND",
                                                     "IRELAND",
                                                     "LATVIA",
                                                     "LITHUANIA",
                                                     "NORWAY",
                                                     "SWEDEN",
                                                     "UNITED KINGDOM",
                                                     "Eastern Europe",
                                                     "BELARUS",
                                                     "BULGARIA",
                                                     "CZECH REPUBLIC",
                                                     "HUNGARY",
                                                     "POLAND",
                                                     "ROMANIA",
                                                     "RUSSIA",
                                                     "SLOVAKIA",
                                                     "Southern Europe",
                                                     "BOSNIA AND HERZEGOVINA",
                                                     "CROATIA",
                                                     "GREECE",
                                                     "ITALY",
                                                     "PORTUGAL",
                                                     "SERBIA",
                                                     "SLOVENIA",
                                                     "SPAIN",
                                                     "Western Europe",
                                                     "AUSTRIA",
                                                     "BELGIUM",
                                                     "FRANCE",
                                                     "GERMANY",
                                                     "LUXEMBOURG",
                                                     "NETHERLANDS",
                                                     "SWITZERLAND",
                                                     "Australia and New Zealand",
                                                     "AUSTRALIA",
                                                     "NEW ZEALAND",
                                                     "Eastern Asia",
                                                     "CHINA",
                                                     "JAPAN",
                                                     "SOUTH KOREA",
                                                     "TAIWAN",
                                                     "Central Asia",
                                                     "KAZAKHSTAN",
                                                     "South-eastern Asia",
                                                     "PHILIPPINES",
                                                     "THAILAND",
                                                     "INDIA",
                                                     "PAKISTAN",
                                                     "Southern Asia",
                                                     "JORDAN",
                                                     "KUWAIT",
                                                     "LEBANON",
                                                     "SAUDI ARABIA",
                                                     "TÜRKİYE",
                                                     "UNITED ARAB EMIRATES",
                                                     "Western Asia",
                                                     "ALGERIA",
                                                     "EGYPT",
                                                     "MOROCCO",
                                                     "Northern Africa",
                                                     "TUNISIA",
                                                     "Southern Africa",
                                                     "SOUTH AFRICA"
))
bind_lm$Region<-factor(bind_lm$Region,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                               "Australia and New Zealand" ,
                                               "Eastern Asia" , "Central Asia",
                                               "South-eastern Asia","Southern Asia" ,
                                               "Western Asia", 
                                               "Northern Africa","Southern Africa"))
bind_lm$Drug<-factor(bind_lm$Drug,levels=c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
a<-ggplot(data = bind_lm, aes(x = reorder(Country, desc(Country)),y = expcoef,fill=Region)) + #don't bother plotting the NA
  geom_bar(stat="identity")+
  geom_errorbar(
    aes(x=`Country`,
        ymin = explower,
        ymax = expupper),
    color = "red"
  )+
  scale_y_continuous(breaks = seq(-100, 3500,10), limits = c(-100, 3480))+
  facet_wrap(~Drug,nrow = 1)+coord_flip()+
  facet_grid(rows = vars(Drug),
             scales = "free_y", switch = "y",
             space = "free_y",
             labeller = label_wrap_gen(width=20)) +
  theme(axis.text.x.top= element_blank(),axis.ticks.x.top= element_blank(),
        axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5, hjust=0),
        legend.position = "right")+
  xlab("Geographical location")+
  ylab("Average annual percentage change, %") 

pg <- a +
  scale_y_break(c(140, 420), scales=0.1)+
  scale_y_break(c(440,3460), scales=0.1)
pg

ggsave(filename="Reference 2_AAPC.png", plot=pg, height =30, width=12,device="png",
       path="D:/R/midas gaba/R1/figures",
       dpi=500)

bind_lm_gaba<-subset(bind_lm,Drug=="Gabapentinoids")
a<-ggplot(data = bind_lm_gaba, aes(x = reorder(Country, desc(Country)),y = expcoef,fill=Region)) + #don't bother plotting the NA
  geom_bar(stat="identity")+
  geom_errorbar(
    aes(x=`Country`,
        ymin = explower,
        ymax = expupper),
    color = "red"
  )+
  scale_y_continuous(breaks = seq(-100, 140,10))+
  coord_flip()+
  # facet_grid(rows = vars(Region), cols = vars(Drug),
  #            scales = "free_y", switch = "y", 
  #            space = "free_y", 
  #            labeller = label_wrap_gen(width=20)) +
  theme(axis.text.x.top= element_blank(),axis.ticks.x.top= element_blank(),
        axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5, hjust=0),
        legend.position = "right")+
  xlab("Geographical location")+
  ylab("Average annual percentage change, %") 
a

# pg <- a +
#   scale_y_break(c(420, 3400), scales=1)
# pg


# geom_errorbar(
#   aes(x=`Country`,
#       ymin = explower,
#       ymax = expupper),
#   color = "red"
# )

ggsave(filename="Figure 4_AAPC_gabapentinoid.png", plot=a, height =12, width=10,device="png",
       path="D:/R/midas gaba/R1/figures",
       dpi=500)






# Subgroup: country income level  

## Gabapentinoids

library(readxl)

GDP <- read_excel("D:/R/midas gaba/Income.xlsx")

'%ni%' <- Negate('%in%')
GDP$country <- toupper(GDP$country)
rename <- lm.cty_gaba%>%filter(Country %ni% GDP$country) 
rename<-unique(rename$Country)
names(GDP)[names(GDP) == 'country'] <- "Country"
GDP[GDP$Country == "KOREA, REP.", "Country"] <- "SOUTH KOREA"
GDP[GDP$Country == "EGYPT, ARAB REP.", "Country"] <- "EGYPT"
GDP[GDP$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
GDP[GDP$Country == "SLOVAK REPUBLIC", "Country"] <- "SLOVAKIA"
GDP[GDP$Country == "VENEZUEL", "Country"] <- "VENEZUELA"
GDP[GDP$Country == "TURKEY", "Country"] <- "TÜRKİYE"
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )


library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentinoids.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                            my_update_function))
gabapentinoids.mlm_income$Drug<-"Gabapentinoids"

datatable(as.data.frame(gabapentinoids.mlm_income[,c(8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))


## Gabapentin  

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
gabapentin.mlm_income<-as.data.frame(gabapentin.mlm_income)
gabapentin.mlm_income$Drug<-"Gabapentin"

## Gabapentin enacarbil  

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentin Enacarbil")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
enacarbil.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                       my_update_function))
enacarbil.mlm_income<-as.data.frame(enacarbil.mlm_income)
enacarbil.mlm_income$Drug<-"Gabapentin Enacarbil"




## Pregabalin    

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
pregabalin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
pregabalin.mlm_income$Drug<-"Pregabalin"
pregabalin.mlm_income<-as.data.frame(pregabalin.mlm_income)


## Individual drugs income AAPC

drug_income_mlm<-rbind(gabapentinoids.mlm_income,gabapentin.mlm_income, enacarbil.mlm_income, pregabalin.mlm_income)
datatable(as.data.frame(drug_income_mlm[,c(12,8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(drug_income_mlm,"D:/R/midas gaba/R1/Output_5_Subgroup_AAPC_income.csv")


## Meta-analysis of DDD/TID by Year  
# When DDD=0 or extremely small, Country data wont be counted as number of studies in the meta analysis  


library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<- left_join(analy,GDP,by = c("Country") )
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$income <- factor(meta.gaba$income, levels =
                             c("HIC","UMIC","LMIC"))
meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$income)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  est.by.random$income<-(t(data.frame(as.list(((summary(m1_var))$byvar)))))
  return(c(est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

##income
gaba_income<-as.data.frame(do.call(rbind, datout))[c(5:10)] 

gaba_income$Year<-as.numeric(gaba_income$Year)
out <- unlist(gaba_income)
Year<-out[1:44]
Drug<-out[45:88]
DDDTID<-out[89:220]
lower<-out[221:352]
upper<-out[353:484]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(3, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper
region_name<-(rep(c("HIC","UMIC","LMIC"),44))
newdata$Income<-region_name

newdata$Drug<-c(rep(("Gabapentin"), 33),
                rep(("Gabapentin Enacarbil"), 33),
                rep(("Pregabalin"), 33),
                rep(("Gabapentinoids"), 33))
gaba_income2<-newdata
gaba_income2$`DDD/TID`<-gaba_income2$DDDTID
gaba_income2$`DDD/TID - lower`<-gaba_income2$lower
gaba_income2$`DDD/TID - upper`<-gaba_income2$upper

gaba_income3<-gaba_income2[c(1,2,6:9)]


library(DT)
datatable(gaba_income3, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(gaba_income3,"D:/R/midas gaba/R1/Output_6_Subgroup_AAPC_income_meta.csv")



## Gaba available in different countries and regions in 2018

library(dplyr)
library(readxl)
library(tidytext)
#df with year, country, drug name (Y/N)
avail<-subset(cty.ddd.3)
avail.1<-subset(avail, DDD!=0)
avail.2<-table(avail.1$Drug, avail.1$Year)
avail.2<-as.data.frame(avail.2)
names(avail.2)<-c("Drug", "Year", "Number of Countries")

avail.3<-avail.1[c(1,4)]
avail.4<-unique(avail.3)
avail.5<-table(avail.4$Year)
avail.5<-as.data.frame(avail.5)
avail.5$Drug<-"Gabapentinoids"
avail.6<-avail.5[c(1,3,2)]
names(avail.6)<-c("Year", "Drug", "Number of Countries")

avail.year<-rbind(avail.2,avail.6)
avail.year

library(dplyr)
library(readxl)
library(tidytext)
#df with year, country, drug name (Y/N)
avail<-subset(cty.ddd.3, Year==2018)
avail.1<-subset(avail, DDD!=0)
avail.1$Availability<-"No"
avail.1$Availability[avail.1$DDDPTPD>0]<-"Yes"

avail.1$Region <- factor(avail.1$Region, levels = 
                           c("Northern America","Central and South America and the Caribbean",
                             "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                             "Australia and New Zealand" ,
                             "Eastern Asia" , "Central Asia",
                             "South-eastern Asia","Southern Asia" ,
                             "Western Asia", "Sub-Saharan Africa",
                             "Northern Africa"
                           ))

avail.2 <- avail.1 %>%
  arrange(Region,Country) %>%               # sort your dataframe
  mutate(Country = factor(Country, unique(Country))) # reset your factor-column based on that order

# ggplot(avail.2, aes(Drug, Country, fill= Availability)) + 
#   geom_tile(colour = "black")+
#   scale_y_discrete(limits=rev)+
#   scale_fill_manual(values=c("darkgreen"),na.value = "white")

# by region
# ggplot(avail.1, aes(Drug, Region, fill= Availability)) + 
#   geom_tile(colour = "black")+
#   scale_y_discrete(limits=rev)+
#   scale_fill_manual(values=c("darkgreen"),na.value = "white")

#by income level
library(readxl)

avail.3 <- left_join(avail.2,GDP,by = c("Country") )
avail.3$income <- factor(avail.3$income, levels = 
                           c("HIC","UMIC","LMIC"))

avail.3 <- avail.3 %>%
  arrange(income,Region, Country) %>%               # sort your dataframe
  mutate(Country = factor(Country, unique(Country))) # reset your factor-column based on that order

ggplot(avail.3, aes(Drug, Country, fill= Availability)) + 
  geom_tile(colour = "black")+
  scale_y_discrete(limits=rev)+
  scale_fill_manual(values=c("darkgreen"),na.value = "white")+
  theme_bw(base_size = 20)+ theme(panel.grid.major = element_blank())
avail.4<-unique(avail.3[c(4,5,8)])
avail.4$Country<-as.character(avail.4$Country)


library(tidyr)
library(DT)

check_source3<-unique(data.1.nodup[c(1,2)])
check_source3<-pivot_wider(check_source3,names_from = SEC,values_from = SEC)
check_source3$CTY = sub(pattern = "(RETAIL)|(COMBINED)|(COMBINE)|(COMBIN)|(COMBI)|(RET)|(R.MUTUALES)|(HOSPITAL)|(TOTAL SALES)", 
                        replacement = "", x = check_source3$CTY, perl = TRUE)
colnames(check_source3)[1]  <- "Country"  
check_source3<- subset(check_source3, !Country=="C. AMERICA"&
                         !Country=="FR. W. AFRICA"&
                         !Country=="FRENCH WEST AFRICA"&
                         !Country=="CENTRAL AMERICA")

check_source3$Country<-trimws(check_source3$Country, which = c("right"), whitespace = "[ \t\r\n]")

check_source3[check_source3$Country == "CZECH", "Country"] <- "CZECH REPUBLIC"
check_source3[check_source3$Country == "NETHERLNDS", "Country"] <- "NETHERLANDS"
check_source3[check_source3$Country == "RUSSIAN FED.", "Country"] <- "RUSSIA"
check_source3[check_source3$Country == "TURKEY", "Country"] <- "TÜRKİYE"
check_source3[check_source3$Country == "UAE", "Country"] <- "UNITED ARAB EMIRATES"
check_source3[check_source3$Country == "UK", "Country"] <- "UNITED KINGDOM"
check_source3[check_source3$Country == "USA", "Country"] <- "UNITED STATES"
check_source3[check_source3$Country == "US", "Country"] <- "UNITED STATES"
check_source3[check_source3$Country == "S. AFRICA", "Country"] <- "SOUTH AFRICA"
check_source3[check_source3$Country == "BOSNIA", "Country"] <- "BOSNIA AND HERZEGOVINA"


check_source4 <- left_join(check_source3,avail.4,by = c("Country") )

datatable(check_source4, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(check_source4, file = "D:/R/midas gaba/R1/Reference_data_source5.csv")



# Sensitivity analysis 1  
## Removing countries without complete data for all study years for gabapentinoids  
### Meta analysis: removing BOSNIA AND HERZEGOVINA, KAZAKHSTAN, NETHERLANDS, SERBIA, THAILAND  

library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<-subset(analy, Drug=="Gabapentinoids" & Country!="BOSNIA AND HERZEGOVINA" &
                      Country!="KAZAKHSTAN" & Country!="NETHERLANDS" & Country!="SERBIA" & Country!="THAILAND")
set_subzero$DDD_dum=set_subzero$DDD
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$Region <- factor(meta.gaba$Region, levels =
                             c("Northern America","Central and South America and the Caribbean",
                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                               "Australia and New Zealand" ,
                               "Eastern Asia" ,
                               "South-eastern Asia","Southern Asia" ,
                               "Western Asia", 
                               "Northern Africa","Southern Africa"
                             ))

meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$Region)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  est.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.random$Year<-rho
  est.random$Drug<-iseed
  est.random$`DDD/TID`<-exp(summary(m1_var)$TE.random)
  est.random$`DDD/TID - lower`<-exp(summary(m1_var)$lower.random)
  est.random$`DDD/TID - upper`<-exp(summary(m1_var)$upper.random)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  return(c(est.random,est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

#Multinational
gaba_global2<-as.data.frame(do.call(rbind, datout))[c(5:9)] 
gaba_global2$Year<-as.numeric(gaba_global2$Year)
gaba_global2$`DDD/TID`<-as.numeric(gaba_global2$`DDD/TID`)
gaba_global2$`DDD/TID - lower`<-as.numeric(gaba_global2$`DDD/TID - lower`)
gaba_global2$`DDD/TID - upper`<-as.numeric(gaba_global2$`DDD/TID - upper`)
gaba_global2$Area<-"Multinational"

gaba_global2$Drug<-c(rep(("Gabapentinoids"), 11))

gaba_global3<-subset(gaba_global2,Drug!="MIROGABALIN")


##Regional
gaba_regional<-as.data.frame(do.call(rbind, datout))[c(14:18)] 

gaba_regional$Year<-as.numeric(gaba_regional$Year)
out <- unlist(gaba_regional)
Year<-out[1:11]
Drug<-out[12:22]
DDDTID<-out[23:165]
lower<-out[166:308]
upper<-out[309:451]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(13, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper

region_name<-(rep(c("Northern America","Central and South America and the Caribbean",
                    "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                    "Australia and New Zealand" ,
                    "Eastern Asia" , 
                    "South-eastern Asia","Southern Asia" ,
                    "Western Asia", 
                    "Northern Africa","Southern Africa"),11))
newdata$Area<-region_name
newdata$Drug<-c( rep(("Gabapentinoids"), 143))
gaba_regional2<-newdata
gaba_regional2$`DDD/TID`<-gaba_regional2$DDDTID
gaba_regional2$`DDD/TID - lower`<-gaba_regional2$lower
gaba_regional2$`DDD/TID - upper`<-gaba_regional2$upper

gaba_regional3<-gaba_regional2[c(1,2,6:9)]
pool_Global_region<-rbind(gaba_global3,gaba_regional3)
pool_Global_region$Drug<- factor(pool_Global_region$Drug, levels = c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
pool_Global_region$Area<-factor(pool_Global_region$Area,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                                                 "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                                                 "Australia and New Zealand" ,
                                                                 "Eastern Asia" , "Central Asia",
                                                                 "South-eastern Asia","Southern Asia" ,
                                                                 "Western Asia", 
                                                                 "Northern Africa","Southern Africa"))
pool_Global_region$Year<-as.numeric(pool_Global_region$Year)
pool_Global_region[is.na(pool_Global_region)]<-0
library(ggbreak)
a<-ggplot(pool_Global_region, aes(x = Year, y = `DDD/TID`, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  facet_wrap(.~Area,nrow=1)+
  geom_ribbon(aes(ymin = `DDD/TID - lower`, ymax = `DDD/TID - upper`), alpha = 0.1, colour = NA) +
  scale_y_break(c(20,62),scales = 0.1)+
  theme_classic()+
  scale_x_continuous(breaks = c(2008:2018))+ 
  theme(panel.border=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line=element_line(),axis.ticks.x = element_blank())+
  ggtitle("Pooled multinational gabapentinoids consumption over time")
a

write.csv(pool_Global_region,"D:/R/midas gaba/R1/Output_7_Sensitivity1_Pooled_Gaba_consumption_CI.csv")

library(DT)
datatable(pool_Global_region[c(6,1:5)], options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



## Regional mixed linear model  

library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids" & Country!="BOSNIA AND HERZEGOVINA" &
                      Country!="KAZAKHSTAN" & Country!="NETHERLANDS" & Country!="SERBIA" & Country!="THAILAND")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}


lapply(unique(regionalmlm$Region),my_update_function)
gabapentinoids.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                            my_update_function))
gabapentinoids.mlm_region<-as.data.frame(gabapentinoids.mlm_region)

# global
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year | Country), data = regionalmlm)
summary(Global_model_1)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_1<-cbind(mixed_est_2,mixed_pval)
mixed_results_1$Model<-"Global_country_int_wAR1"
mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100

mixed_results_Global<-rbind(mixed_results_1)

# Results table
bind_reg_lm<-gabapentinoids.mlm_region
bind_reg_lm$Country<-bind_reg_lm$Model
bind_reg_lm$Region<-bind_reg_lm$Model
bind_reg_lm<-bind_reg_lm[c(13,9:11,7)]

bind_glo_lm<-mixed_results_Global
bind_glo_lm<-bind_glo_lm[c(7:11)]
bind_glo_lm$Country<-"Multinational"
bind_glo_lm$Region<-"Multinational"
bind_glo_lm<-bind_glo_lm[c(7,3:5,1)]
bind_lm<-rbind(bind_reg_lm,bind_glo_lm)
write.csv(bind_lm,"D:/R/midas gaba/R1/Output_8_Sensitivity_1_lm_cty_region_multi.csv")

datatable(bind_lm, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



# Sensitivity analysis 2: reviewer's comment to only include RETAIL data   
## Reading data  
# Data inclusion criteria: variable name "MOL"= "PREGABALIN","GABAPENTIN","GABAPENTIN ENACARBIL","MIROGABALIN"  

library(readxl)
library(dplyr)
# data <- read.csv("D:/MIDAS ADHD/midas_adhd/adhd/MIDAS 2019.csv")
# data
getwd()
setwd("D:/R/midas gaba")
gaba<-subset(read.csv("D:/R/midas benzo/raw/gaba_extract.csv"),MOL=="PREGABALIN"|
               MOL=="GABAPENTIN"|
               MOL=="GABAPENTIN ENACARBIL"|
               MOL=="MIROGABALIN")
gaba$Class<-"Gabapentinoids"


## Keep SU and DDD data and only RETAIL data  
```{r echo=TRUE, message=FALSE, warning=FALSE}
master<-subset(gaba,SEC=="RETAIL")

## Separate data into with DDD units and without DDD units (INTWHODDDDESC NOT ASSIGNED)  

detach(package:plyr)
library(dplyr)
#Identify products that need conversion from SU to INTDDD
missing_DDD<-subset(master,INTWHODDDDESC=="INTWHODDD DESC NOT ASSIGNED")
with_DDD<-subset(master,INTWHODDDDESC!="INTWHODDD DESC NOT ASSIGNED")

# #Export list for conversion factor identification
# miss_DDD<-unique(missing_DDD[c(1:8)])
# write.csv(miss_DDD,"D:/R/midas benzo/dosage_convert.csv")

#Daniel and Andrew has done the conversion
convert <- read.table(
  "D:/R/midas benzo/convert_20221114_removed.txt",
  sep="\t", header=TRUE)
convert2<-subset(convert, !is.na(convert$Strength.of.BZD.or.GABA) & (MOL=="PREGABALIN"|
                                                                       MOL=="GABAPENTIN"|
                                                                       MOL=="GABAPENTIN ENACARBIL"|
                                                                       MOL=="MIROGABALIN"))
convert_missing<-subset(convert, is.na(convert$Strength.of.BZD.or.GABA))
convert_medianimpute_strength <- convert2[c(1:10)] %>% group_by (CTY,SEC, MNF, ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,INTWHODDDDESC_2) %>% mutate(median=median(Strength.of.BZD.or.GABA, na.rm = TRUE)) 
convert_medianimpute_strength2<-unique(convert_medianimpute_strength[c(1:8,10,11)])
#left join convert to data
missing_DDD_convert <- left_join(missing_DDD,convert_medianimpute_strength2,by = c("CTY","SEC","MNF","ATC3","INTPRD","NFC123",  
                                                                                   "INTWHODDDDESC","MOL") )
write.csv(missing_DDD,"D:/R/midas gaba/withoutddd_539.csv")
missing_DDD_remain<-subset(missing_DDD_convert, !is.na(median) & Class=="Gabapentinoids")


missing_DDD_agg<-missing_DDD %>% group_by (CTY, SEC,MNF,ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,X_TYPE_,X_FREQ_,Class) %>% 
  summarise_at(vars(1:857),sum)


## Results of median imputed strengths:  

library(DT)
datatable(subset(convert_medianimpute_strength2), options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



## Aggregate separately for DDD and missing DDD dataframes 
### DDD

library(tidyr)
library(zoo)
library(dplyr)


ddd <-with_DDD

## Remove columns of other sales data e.g. LC, LCD, USD, CU, SU
ddd.2 <- ddd[-c(9:10)]%>%select(-contains(c("LC_MNF","LCD_MNF","USD_MNF","CU_MTH","SU_MTH")))
ddd.3 <- pivot_longer(ddd.2, cols=9:151, 
                      names_to = "Month", values_to = "DDD")
## Format date
ddd.3$Month<-str_remove(ddd.3$Month, "INTDDD_MTH_")
ddd.3<-ddd.3 %>% separate(Month, c("Month","Year"), 
                          sep = "([_])")
ddd.3$Year <- paste0("20", ddd.3$Year, sep = "")
ddd.3$Year<-as.numeric(ddd.3$Year)
ddd.3$Month<-as.numeric(ddd.3$Month)
ddd.3$Date <- as.yearmon(paste(ddd.3$Year, 
                               ddd.3$Month), "%Y %m")
ddd.4<-ddd.3[-c(10)]
ddd.4$DDD[is.na(ddd.4$DDD)]<-0


### Without DDD  
library(tidyr)
library(zoo)
library(dplyr)

noddd <-missing_DDD_remain

## Remove columns of other sales data e.g. LC, LCD, USD, CU, SU
noddd.2 <- noddd[-c(9:10)]%>%select(-contains(c("LC_MNF","LCD_MNF","USD_MNF","CU_MTH","INTDDD_MTH")))
noddd.3 <- pivot_longer(noddd.2, cols=9:151, 
                        names_to = "Month", values_to = "SU")
## Calculate DDD
noddd.3$DDD<-noddd.3$SU*noddd.3$median/noddd.3$INTWHODDDDESC_2

## Format date
noddd.3$Month<-str_remove(noddd.3$Month, "SU_MTH_")
noddd.3<-noddd.3 %>% separate(Month, c("Month","Year"), 
                              sep = "([_])")
noddd.3$Year <- paste0("20", noddd.3$Year, sep = "")
noddd.3$Year<-as.numeric(noddd.3$Year)
noddd.3$Month<-as.numeric(noddd.3$Month)
noddd.3$Date <- as.yearmon(paste(noddd.3$Year, 
                                 noddd.3$Month), "%Y %m")
noddd.4<-noddd.3
noddd.4$DDD[is.na(noddd.4$DDD)]<-0


## Aggregate the consumption data by country, drug name, date  

ddd.5<-ddd.4
noddd.5<-noddd.4[-c(10,11,12,14)]

bind<-rbind(ddd.5,noddd.5)

aggregate<-bind[-c(2:7)]

aggregate$CTY = sub(pattern = "(RETAIL)|(COMBINED)|(COMBINE)|(COMBIN)|(COMBI)|(RET)|(R.MUTUALES)|(HOSPITAL)|(TOTAL SALES)", 
                    replacement = "", x = aggregate$CTY, perl = TRUE)
aggregate.2<-aggregate %>% group_by (CTY, MOL,Class,Year, Date) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)


## Merge rx data with UN population data  
library(readxl)
library(dplyr)
#population size from UN====
pop <- read_excel(path = "D:/R/midas adhd/ref data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                  sheet = "ESTIMATES",
                  range = "B17:DE18122")
pop.2<-subset(pop, Type=="Country/Area")
pop.4<-pop.2[-c(1,3:63,77:108)]
names(pop.4)[names(pop.4) == 'Region, subregion, country or area *'] <- 'Country'

pop.5 <- pivot_longer(pop.4, cols=2:14,
                      names_to = "Year", values_to = "Population")

pop.5$Country <- toupper(pop.5$Country)
names(aggregate.2)[names(aggregate.2) == 'CTY'] <- 'Country'
aggregate.2$Country <-trimws(aggregate.2$Country)
'%ni%' <- Negate('%in%')

ddd.set.2<- subset(aggregate.2, !Country=="C. AMERICA"&
                     !Country=="FR. W. AFRICA"&
                     !Country=="FRENCH WEST AFRICA"&
                     !Country=="CENTRAL AMERICA")
pop.7<-pop.5
rename <- pop.7 %>%filter(Country %ni% ddd.set.2$Country) 
rename<-unique(rename$Country)

ddd.set.2[ddd.set.2$Country == "CZECH", "Country"] <- "CZECH REPUBLIC"
ddd.set.2[ddd.set.2$Country == "NETHERLNDS", "Country"] <- "NETHERLANDS"
ddd.set.2[ddd.set.2$Country == "RUSSIAN FED.", "Country"] <- "RUSSIA"
ddd.set.2[ddd.set.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
ddd.set.2[ddd.set.2$Country == "UAE", "Country"] <- "UNITED ARAB EMIRATES"
ddd.set.2[ddd.set.2$Country == "UK", "Country"] <- "UNITED KINGDOM"
ddd.set.2[ddd.set.2$Country == "USA", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "US", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "S. AFRICA", "Country"] <- "SOUTH AFRICA"
ddd.set.2[ddd.set.2$Country == "BOSNIA", "Country"] <- "BOSNIA AND HERZEGOVINA"
ddd.set.2[ddd.set.2$Country == "KOREA", "Country"] <- "SOUTH KOREA"
pop.7<-pop.5
pop.7[pop.7$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
pop.7[pop.7$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
pop.7[pop.7$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
pop.7[pop.7$Country == "TURKEY", "Country"] <- "TÜRKİYE"
pop.7[pop.7$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
pop.7[pop.7$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
pop.7[pop.7$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"

pop.8<-as.data.frame(pop.7)
ddd.set.2$Year<-as.numeric(ddd.set.2$Year)
pop.8$Year<-as.numeric(pop.8$Year)
ddd.set.3.1<-ddd.set.2
ddd.set.3 <- left_join(ddd.set.3.1,pop.8,by = c("Country","Year") )

## Merge data with Region categorisation====
Region <- read_excel(path = "D:/R/midas adhd/ref data/UNSD — Methodology.xlsx")
Region.2<-Region[c(6,9,16)]
names(Region.2)[names(Region.2) == 'Country or Area'] <- 'Country'
Region.2$Country <- toupper(Region.2$Country)
rename <- ddd.set.3%>%filter(Country %ni% Region.2$Country) 
rename<-unique(rename$Country)
Region.2[Region.2$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
Region.2[Region.2$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
Region.2[Region.2$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
Region.2[Region.2$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
Region.2[Region.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
Region.2[Region.2$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
Region.2[Region.2$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"
Region.2[Region.2$Country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "Country"] <- "UNITED KINGDOM"

Region.3<-Region.2%>%filter(Country %in% ddd.set.3$Country) 
ddd.set.4 <- left_join(ddd.set.3,Region.3,by = c("Country") )
names(ddd.set.4)[names(ddd.set.4) == 'Sub-region Name'] <- 'Region'
ddd.set.4[ddd.set.4$Country == "TAIWAN", "Region"] <- "Eastern Asia"

ddd.set.5<-ddd.set.4[-c(9)]


ddd.set.5[ddd.set.5$Region == "Latin America and the Caribbean", "Region"] <- "Central and South America and the Caribbean"
ddd.set.5[ddd.set.5$Region == "Sub-Saharan Africa", "Region"] <- "Southern Africa"

population<-pop.8
population$Population<-as.numeric(population$Population)

options(scipen=999)

## Final analytic dataset  
library(dplyr)
cty.all<-ddd.set.5

names(cty.all)[names(cty.all) == 'MOL'] <- 'Drug'
## create one df of monthly data just in case
cty.all.3<-cty.all[-c(7)] %>% group_by (Year,Date,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)

## numerator
cty.ddd.2<-cty.all[-c(5,7)] %>% group_by (Year,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
##denominator
cty.pop<-population

#Aggregate benzos and gaba
#gaba
new_gaba<-subset(cty.ddd.2)
new_gaba<-new_gaba[-c(3)]
new_gaba<-new_gaba %>% group_by (Year,Country, Region,Class) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
new_gaba$Drug<-"Gabapentinoids"
new_gaba$DDD[is.na(new_gaba$DDD)]<-0
cty.ddd.2<-rbind(cty.ddd.2,new_gaba)

#Add zeros
Drug<-sort(rep(c("Gabapentinoids","MIROGABALIN","PREGABALIN","GABAPENTIN","GABAPENTIN ENACARBIL"),693))
Country<-(rep(unique(cty.ddd.2$Country),11))
Year<-sort(rep(c(2008:2018),63))

merge_zero <- data.frame(Country, Year)
merge_zero2<-do.call("rbind", replicate(5, merge_zero, simplify = FALSE))
merge_zero2$Drug<-Drug
cty.ddd.3 <- right_join(x=cty.ddd.2,y=merge_zero2, 
                        by=c("Year","Country","Drug"))

cty.ddd.4 <- left_join(x=cty.ddd.3,y=cty.pop, 
                       by=c("Year","Country"))

cty.ddd.5 <- distinct(left_join(x=cty.ddd.4[-c(2,5)],y=ddd.set.5[c(1,3,8)], 
                                by=c("Country")))

cty.ddd.5$DDD[is.na(cty.ddd.5$DDD)]<-0

#Divide
cty.ddd.5$DDDPTPD <-(cty.ddd.5$DDD/cty.ddd.5$Population)/365.25
cty.ddd.5$Drug<-str_to_title(cty.ddd.5$Drug)
analy<-subset(cty.ddd.5,Year!=2007 & Year!=2019 & Drug!="Vigbatrin"&Drug!="Mirogabalin")

library(DT)
datatable(analy, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

str(analy,give.attr=FALSE)

## Main Analysis: DDD/TID and trends     

## Overview of gabapentinoids consumption by Drug, Country, and Year   

library(ggplot2)
library(RColorBrewer)
new_set.2<-analy
stacked<-subset(new_set.2, Drug!="Gabapentinoids" )
stacked$Country<-toupper(stacked$Country)
stacked[
  order( stacked[,2] ,stacked[,3]),
]

stacked$Drug <- factor(stacked$Drug)
stacked$Region <- factor(stacked$Region, levels =
                           c("Global","Northern America","Central and South America and the Caribbean",
                             "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                             "Oceania" ,
                             "Eastern Asia" ,
                             "South-eastern Asia","Southern Asia" ,
                             "Western Asia","Central Asia",
                             "Northern Africa", "Southern Africa"
                           ))
a<-ggplot(stacked[order(stacked$Drug), ], aes(x = str_to_title(Country), y = DDDPTPD*10, fill = Drug)) +
  geom_col(alpha = 0.8, width = 0.9) +
  scale_y_continuous(expand = c(0, 0.1)) +
  scale_x_discrete(limits=rev)+
  coord_flip() +
  facet_grid(rows = vars(Region), cols = vars(Year),
             scales = "free_y", switch = "y", 
             space = "free_y", 
             labeller = label_wrap_gen(width=20)) +
  labs(
    title = "Gabapentinoids consumption from 2008 to 2018",
    subtitle = "in 65 countries",
    caption = "Figure",
    y = "Defined Daily Dose per 10000 inhabitants per day"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#EC2049", "#DCEDC2", "#2F9599"))+
  #  scale_fill_brewer(palette = "Dark2")+
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(size = 15, face = "bold"),
    panel.spacing.x = unit(1, "lines"),
    # strip.text.y = element_text(angle = 270, face = "bold"),
    strip.text.y.left = element_text(angle = 0,face = "bold"),
    strip.placement = "outside",
    axis.title.x = element_text(margin = margin(t = 0.5, b = 0.5, unit = "cm")),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x=element_text(angle=270),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )
a

## Calculate CIs of consumption in DDD/TID per year  
library(epitools)
CIs<-pois.approx(new_set.2$DDD, pt = new_set.2$Population*365.25, conf.level = 0.95)
pool.new_set<-cbind(new_set.2,CIs)
pool.new_set.2<-subset(pool.new_set)
pool.new_set.2$DDD<-round(pool.new_set.2$DDD, digits = 5)  
write.csv(pool.new_set.2,"D:/R/midas gaba/R1/Output_9_Sensitivity2_Gaba_consumption_CI_sens2.csv")
write.xlsx(pool.new_set.2,"D:/R/midas gaba/R1/Output_9_Sensitivity2_Gaba_consumption_CI_sens2.csv")


## Meta-analysis of DDD/TID by Year  
# 
# Drug indices: [1] "Gabapentin", [2] "Gabapentin Enacarbil", [3] "Pregabalin", [4]"Gabapentinoids"   
library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<-subset(analy, Drug!="MIROGABALIN")
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$Region <- factor(meta.gaba$Region, levels =
                             c("Northern America","Central and South America and the Caribbean",
                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                               "Australia and New Zealand" ,
                               "Eastern Asia" , "Central Asia",
                               "South-eastern Asia","Southern Asia" ,
                               "Western Asia", 
                               "Northern Africa","Southern Africa"
                             ))

meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$Region)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  est.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.random$Year<-rho
  est.random$Drug<-iseed
  est.random$`DDD/TID`<-exp(summary(m1_var)$TE.random)
  est.random$`DDD/TID - lower`<-exp(summary(m1_var)$lower.random)
  est.random$`DDD/TID - upper`<-exp(summary(m1_var)$upper.random)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  return(c(est.random,est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

#Multinational
gaba_global2<-as.data.frame(do.call(rbind, datout))[c(5:9)] 
gaba_global2$Year<-as.numeric(gaba_global2$Year)
gaba_global2$`DDD/TID`<-as.numeric(gaba_global2$`DDD/TID`)
gaba_global2$`DDD/TID - lower`<-as.numeric(gaba_global2$`DDD/TID - lower`)
gaba_global2$`DDD/TID - upper`<-as.numeric(gaba_global2$`DDD/TID - upper`)
gaba_global2$Area<-"Multinational"

gaba_global2$Drug<-c(rep(("Gabapentin"), 11),
                     rep(("Gabapentin Enacarbil"), 11),
                     rep(("Pregabalin"), 11),
                     rep(("Gabapentinoids"), 11))

gaba_global3<-subset(gaba_global2,Drug!="MIROGABALIN")

a<-ggplot(gaba_global3, aes(x = Year, y = `DDD/TID`*10, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  geom_ribbon(aes(ymin = `DDD/TID - lower`*10, ymax = `DDD/TID - upper`*10), alpha = 0.1, colour = NA) + 
  scale_x_continuous(breaks = c(2008:2018))+
  ggtitle("Pooled multinational gabapentinoids consumption over time")+
  ylab("Defined daily dose per 10000 inhabitants per day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
a

##Regional
gaba_regional<-as.data.frame(do.call(rbind, datout))[c(14:18)] 

gaba_regional$Year<-as.numeric(gaba_regional$Year)
out <- unlist(gaba_regional)
Year<-out[1:44]
Drug<-out[45:88]
DDDTID<-out[89:704]
lower<-out[705:1320]
upper<-out[1321:1936]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(14, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper

region_name<-(rep(c("Northern America","Central and South America and the Caribbean",
                    "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                    "Australia and New Zealand" ,
                    "Eastern Asia" , "Central Asia",
                    "South-eastern Asia","Southern Asia" ,
                    "Western Asia", 
                    "Northern Africa","Southern Africa"),44))
newdata$Area<-region_name
newdata$Drug<-c(rep(("Gabapentin"), 154),
                rep(("Gabapentin Enacarbil"), 154),
                rep(("Pregabalin"), 154),
                rep(("Gabapentinoids"), 154))
gaba_regional2<-newdata
gaba_regional2$`DDD/TID`<-gaba_regional2$DDDTID
gaba_regional2$`DDD/TID - lower`<-gaba_regional2$lower
gaba_regional2$`DDD/TID - upper`<-gaba_regional2$upper

gaba_regional3<-gaba_regional2[c(1,2,6:9)]
pool_Global_region<-rbind(gaba_global3,gaba_regional3)
pool_Global_region$Drug<- factor(pool_Global_region$Drug, levels = c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
pool_Global_region$Area<-factor(pool_Global_region$Area,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                                                 "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                                                 "Australia and New Zealand" ,
                                                                 "Eastern Asia" , "Central Asia",
                                                                 "South-eastern Asia","Southern Asia" ,
                                                                 "Western Asia", 
                                                                 "Northern Africa","Southern Africa"))
pool_Global_region$Year<-as.numeric(pool_Global_region$Year)
pool_Global_region[is.na(pool_Global_region)]<-0
library(ggbreak)
a<-ggplot(pool_Global_region, aes(x = Year, y = `DDD/TID`, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  facet_wrap(.~Area,nrow=1)+
  geom_ribbon(aes(ymin = `DDD/TID - lower`, ymax = `DDD/TID - upper`), alpha = 0.1, colour = NA) +
  scale_y_break(c(20,62),scales = 0.1)+
  theme_classic()+
  scale_x_continuous(breaks = c(2008:2018))+ 
  theme(panel.border=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line=element_line(),axis.ticks.x = element_blank())+
  ggtitle("Pooled multinational gabapentinoids consumption over time")
a

write.csv(pool_Global_region,"D:/R/midas gaba/R1/Output_9_Sensitivity2_Pooled_Gaba_consumption_CI_sens2.csv")

library(DT)
datatable(pool_Global_region[c(6,1:5)], options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))


## Average annual percentage change   

### Data view - showing which zeros were removed    
library(plyr)
library(Hmisc)
library(DT)
library(tibble)

lm.cty_gaba<-subset(pool.new_set.2)
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

datatable(lm.cty_gaba,  options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

### National level  
library(plyr)
library(Hmisc)
library(DT)
library(tibble)


lm.cty_gaba_2<-subset(lm.cty_gaba, DDD>0)

models <- dlply(lm.cty_gaba_2, .(Country,Drug), function(df) 
  lm(log(DDDPTPD) ~ Year, data = df))

coef<-sapply(models, function(df) summary(df)$coefficients[2])
lm.results<-data.frame(coef)

ad_extract_ci <- function(x){
  temp_lw <- confint.lm(x)[2,1]
  temp_up <- confint.lm(x)[2,2]
  return(c(temp_lw,temp_up))
}

lower<-unlist(lapply(lapply(models,ad_extract_ci),"[",1))
lm.results<-cbind(lm.results, lower)
upper<-unlist(lapply(lapply(models,ad_extract_ci),"[",2))
lm.results<-cbind(lm.results, upper)
pvalue<-sapply(models, function(df) summary(df)$coefficients[8])
lm.results<-cbind(lm.results, pvalue)
lm.results<-rownames_to_column(lm.results)
colnames(lm.results)[which(names(lm.results) == "rowname")] <- "Country"
lm.results$expcoef<-(exp(lm.results$coef)-1)*100
lm.results$explower<-(exp(lm.results$lower)-1)*100
lm.results$expupper<-(exp(lm.results$upper)-1)*100

lm.results2<-(lm.results) %>% separate(Country, c("Country", "Drug"), sep="[.]")


cty.lm.results<-distinct(left_join(lm.results2,pool.new_set.2[c(2,3,7)]))




### Global  


library(plyr)
library(lme4)
library(nlme)
# 
# mixed_results_Global<-data.frame()
# lm.cty_gaba_x<-split(lm.cty_gaba_2, list(lm.cty_gaba_2$Drug))
# for (i in 1:4){
# Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
# correlation = corAR1(form = ~ Year|Country), data = as.data.frame(lm.cty_gaba_x[i], col.names = c("")))
# summary(Global_model_1)
# mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
# mixed_est_2 <- data.frame(mixed_est$fixed)
# mixed_pval<-anova(Global_model_1)
# mixed_pval<-as.data.frame(mixed_pval)
# mixed_results_1<-cbind(mixed_est_2,mixed_pval)
# mixed_results_1$Model<-"Global_country_int_wAR1"
# mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
# mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
# mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100
# 
# mixglo<-rbind(mixed_results_1)
# mixglo$Drug<-c(unique(lm.cty_gaba$Drug)[i])
# mixed_results_Global<-rbind(mixed_results_Global,mixglo)
# }
# 
# datatable(mixed_results_Global[c(8:12,7,1:6)], options = list(
#   autoWidth = TRUE,
#   columnDefs = list(list(width = '100px', targets = c(1, 3)))
# ))


asd<-subset(lm.cty_gaba_2, Drug=="Gabapentinoids")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_1<-cbind(mixed_est_2,mixed_pval)
mixed_results_1$Drug<-"Gabapentinoids"
mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Gabapentin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_2<-cbind(mixed_est_2,mixed_pval)
mixed_results_2$Drug<-"Gabapentin"
mixed_results_2$expcoef<-(exp(mixed_results_2$est.)-1)*100
mixed_results_2$explower<-(exp(mixed_results_2$lower)-1)*100
mixed_results_2$expupper<-(exp(mixed_results_2$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Pregabalin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_3<-cbind(mixed_est_2,mixed_pval)
mixed_results_3$Drug<-"Pregabalin"
mixed_results_3$expcoef<-(exp(mixed_results_3$est.)-1)*100
mixed_results_3$explower<-(exp(mixed_results_3$lower)-1)*100
mixed_results_3$expupper<-(exp(mixed_results_3$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Gabapentin Enacarbil")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_4<-cbind(mixed_est_2,mixed_pval)
mixed_results_4$Drug<-"Gabapentin Enacarbil"
mixed_results_4$expcoef<-(exp(mixed_results_4$est.)-1)*100
mixed_results_4$explower<-(exp(mixed_results_4$lower)-1)*100
mixed_results_4$expupper<-(exp(mixed_results_4$upper)-1)*100


mixed_results_Global<-rbind(mixed_results_1,mixed_results_2,mixed_results_3,mixed_results_4)
datatable(mixed_results_Global, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))


### Regional  
#### Gabapentinoids  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(lm.cty_gaba_2$Region),my_update_function)
gabapentinoids.mlm_region<-rbindlist(lapply(unique(lm.cty_gaba_2$Region),
                                            my_update_function))
gabapentinoids.mlm_region<-as.data.frame(gabapentinoids.mlm_region)
gabapentinoids.mlm_region$Drug<-"Gabapentinoids"



#### Gabapentin  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
gabapentin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
gabapentin.mlm_region<-as.data.frame(gabapentin.mlm_region)
gabapentin.mlm_region$Drug<-"Gabapentin"


#### Gabapentin enacarbil  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentin Enacarbil")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
enacarbil.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                       my_update_function))
enacarbil.mlm_region<-as.data.frame(enacarbil.mlm_region)
enacarbil.mlm_region$Drug<-"Gabapentin Enacarbil"


#### Pregabalin  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
pregabalin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
pregabalin.mlm_region<-as.data.frame(pregabalin.mlm_region)
pregabalin.mlm_region$Drug<-"Pregabalin"
region_reg_mlm<-rbind(gabapentinoids.mlm_region,gabapentin.mlm_region, enacarbil.mlm_region, pregabalin.mlm_region)

### Result table    

bind_cty_lm<-cty.lm.results[c(2,10,1,7:9,6)]
names(bind_cty_lm)[7]<-"p-value"

bind_reg_lm<-region_reg_mlm
bind_reg_lm$Country<-bind_reg_lm$Model
bind_reg_lm$Region<-bind_reg_lm$Model
bind_reg_lm<-bind_reg_lm[c(12,14,13,9:11,7)]

bind_glo_lm<-mixed_results_Global[c(2,4,6,8),]
bind_glo_lm<-bind_glo_lm[c(8:11,7)]
bind_glo_lm$Country<-"Multinational"
bind_glo_lm$Region<-"Multinational"
bind_glo_lm<-bind_glo_lm[c(7,6,1:5)]
bind_lm<-rbind(bind_cty_lm,bind_reg_lm,bind_glo_lm)
write.csv(bind_lm,"D:/R/midas gaba/R1/Output_10_Sensitivity_2_lm_cty_region_multi.csv")

datatable(bind_lm, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

### Visualisation: Average annual percentage chnage of gabapantinoids - Multinational, Regional, National  
library(dplyr)
library(ggplot2)

bind_lm$Country<- factor(bind_lm$Country, levels = c("Multinational",
                                                     "Northern America",
                                                     "CANADA",
                                                     "UNITED STATES",
                                                     "Central and South America and the Caribbean",
                                                     "ARGENTINA",
                                                     "BRAZIL",
                                                     "CHILE",
                                                     "COLOMBIA",
                                                     "ECUADOR",
                                                     "MEXICO",
                                                     "PERU",
                                                     "PUERTO RICO",
                                                     "URUGUAY",
                                                     "VENEZUELA",
                                                     "Northern Europe",
                                                     "ESTONIA",
                                                     "FINLAND",
                                                     "IRELAND",
                                                     "LATVIA",
                                                     "LITHUANIA",
                                                     "NORWAY",
                                                     "SWEDEN",
                                                     "UNITED KINGDOM",
                                                     "Eastern Europe",
                                                     "BELARUS",
                                                     "BULGARIA",
                                                     "CZECH REPUBLIC",
                                                     "HUNGARY",
                                                     "POLAND",
                                                     "ROMANIA",
                                                     "RUSSIA",
                                                     "SLOVAKIA",
                                                     "Southern Europe",
                                                     "BOSNIA AND HERZEGOVINA",
                                                     "CROATIA",
                                                     "GREECE",
                                                     "ITALY",
                                                     "PORTUGAL",
                                                     "SERBIA",
                                                     "SLOVENIA",
                                                     "SPAIN",
                                                     "Western Europe",
                                                     "AUSTRIA",
                                                     "BELGIUM",
                                                     "FRANCE",
                                                     "GERMANY",
                                                     "LUXEMBOURG",
                                                     "NETHERLANDS",
                                                     "SWITZERLAND",
                                                     "Australia and New Zealand",
                                                     "AUSTRALIA",
                                                     "NEW ZEALAND",
                                                     "Eastern Asia",
                                                     "CHINA",
                                                     "JAPAN",
                                                     "SOUTH KOREA",
                                                     "TAIWAN",
                                                     "Central Asia",
                                                     "KAZAKHSTAN",
                                                     "South-eastern Asia",
                                                     "PHILIPPINES",
                                                     "THAILAND",
                                                     "INDIA",
                                                     "PAKISTAN",
                                                     "Southern Asia",
                                                     "JORDAN",
                                                     "KUWAIT",
                                                     "LEBANON",
                                                     "SAUDI ARABIA",
                                                     "TÜRKİYE",
                                                     "UNITED ARAB EMIRATES",
                                                     "Western Asia",
                                                     "ALGERIA",
                                                     "EGYPT",
                                                     "MOROCCO",
                                                     "Northern Africa",
                                                     "TUNISIA",
                                                     "Southern Africa",
                                                     "SOUTH AFRICA"
))
bind_lm$Region<-factor(bind_lm$Region,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                               "Australia and New Zealand" ,
                                               "Eastern Asia" , "Central Asia",
                                               "South-eastern Asia","Southern Asia" ,
                                               "Western Asia", 
                                               "Northern Africa","Southern Africa"))
bind_lm$Drug<-factor(bind_lm$Drug,levels=c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
a<-ggplot(data = bind_lm, aes(x = reorder(Country, desc(Country)),y = expcoef,fill=Region)) + #don't bother plotting the NA
  geom_bar(stat="identity")+
  geom_errorbar(
    aes(x=`Country`,
        ymin = explower,
        ymax = expupper),
    color = "red"
  )+
  scale_y_continuous(breaks = seq(-100, 3500,10), limits = c(-100, 3480))+
  facet_wrap(~Drug,nrow = 1)+coord_flip()+
  facet_grid(rows = vars(Drug),
             scales = "free_y", switch = "y",
             space = "free_y",
             labeller = label_wrap_gen(width=20)) +
  theme(axis.text.x.top= element_blank(),axis.ticks.x.top= element_blank(),
        axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5, hjust=0),
        legend.position = "right")+
  xlab("Geographical location")+
  ylab("Average annual percentage change, %") 

pg <- a +
  scale_y_break(c(140, 420), scales=0.1)+
  scale_y_break(c(440,3460), scales=0.1)
pg

ggsave(filename="Reference 2_AAPC.png", plot=pg, height =30, width=12,device="png",
       path="D:/R/midas gaba/R1/figures",
       dpi=500)

bind_lm_gaba<-subset(bind_lm,Drug=="Gabapentinoids")
a<-ggplot(data = bind_lm_gaba, aes(x = reorder(Country, desc(Country)),y = expcoef,fill=Region)) + #don't bother plotting the NA
  geom_bar(stat="identity")+
  geom_errorbar(
    aes(x=`Country`,
        ymin = explower,
        ymax = expupper),
    color = "red"
  )+
  scale_y_continuous(breaks = seq(-100, 140,10))+
  coord_flip()+
  # facet_grid(rows = vars(Region), cols = vars(Drug),
  #            scales = "free_y", switch = "y", 
  #            space = "free_y", 
  #            labeller = label_wrap_gen(width=20)) +
  theme(axis.text.x.top= element_blank(),axis.ticks.x.top= element_blank(),
        axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5, hjust=0),
        legend.position = "right")+
  xlab("Geographical location")+
  ylab("Average annual percentage change, %") 
a

# pg <- a +
#   scale_y_break(c(420, 3400), scales=1)
# pg


# geom_errorbar(
#   aes(x=`Country`,
#       ymin = explower,
#       ymax = expupper),
#   color = "red"
# )

ggsave(filename="Figure 4_AAPC_gabapentinoid.png", plot=a, height =12, width=10,device="png",
       path="D:/R/midas gaba/R1/figures",
       dpi=500)





# Subgroup: country income level  

## Gabapentinoids
library(readxl)

GDP <- read_excel("D:/R/midas gaba/Income.xlsx")

'%ni%' <- Negate('%in%')
GDP$country <- toupper(GDP$country)
rename <- lm.cty_gaba%>%filter(Country %ni% GDP$country) 
rename<-unique(rename$Country)
names(GDP)[names(GDP) == 'country'] <- "Country"
GDP[GDP$Country == "KOREA, REP.", "Country"] <- "SOUTH KOREA"
GDP[GDP$Country == "EGYPT, ARAB REP.", "Country"] <- "EGYPT"
GDP[GDP$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
GDP[GDP$Country == "SLOVAK REPUBLIC", "Country"] <- "SLOVAKIA"
GDP[GDP$Country == "VENEZUEL", "Country"] <- "VENEZUELA"
GDP[GDP$Country == "TURKEY", "Country"] <- "TÜRKİYE"
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )


library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentinoids.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                            my_update_function))
gabapentinoids.mlm_income$Drug<-"Gabapentinoids"

datatable(as.data.frame(gabapentinoids.mlm_income[,c(8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

## Gabapentin  

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
gabapentin.mlm_income<-as.data.frame(gabapentin.mlm_income)
gabapentin.mlm_income$Drug<-"Gabapentin"

## Gabapentin enacarbil  

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentin Enacarbil")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
enacarbil.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                       my_update_function))
enacarbil.mlm_income<-as.data.frame(enacarbil.mlm_income)
enacarbil.mlm_income$Drug<-"Gabapentin Enacarbil"



## Pregabalin    
lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
pregabalin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
pregabalin.mlm_income$Drug<-"Pregabalin"
pregabalin.mlm_income<-as.data.frame(pregabalin.mlm_income)

## Individual drugs income AAPC
drug_income_mlm<-rbind(gabapentinoids.mlm_income,gabapentin.mlm_income, enacarbil.mlm_income, pregabalin.mlm_income)
datatable(as.data.frame(drug_income_mlm[,c(12,8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(drug_income_mlm,"D:/R/midas gaba/R1/Output_11_Sensitivity_2_Subgroup_AAPC_income.csv")

## Meta-analysis of DDD/TID by Year  
library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<- left_join(analy,GDP,by = c("Country") )
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$income <- factor(meta.gaba$income, levels =
                             c("HIC","UMIC","LMIC"))
meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$income)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  est.by.random$income<-(t(data.frame(as.list(((summary(m1_var))$byvar)))))
  return(c(est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

##income
gaba_income<-as.data.frame(do.call(rbind, datout))[c(5:10)] 

gaba_income$Year<-as.numeric(gaba_income$Year)
out <- unlist(gaba_income)
Year<-out[1:44]
Drug<-out[45:88]
DDDTID<-out[89:220]
lower<-out[221:352]
upper<-out[353:484]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(3, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper
region_name<-(rep(c("HIC","UMIC","LMIC"),44))
newdata$Income<-region_name

newdata$Drug<-c(rep(("Gabapentin"), 33),
                rep(("Gabapentin Enacarbil"), 33),
                rep(("Pregabalin"), 33),
                rep(("Gabapentinoids"), 33))
gaba_income2<-newdata
gaba_income2$`DDD/TID`<-gaba_income2$DDDTID
gaba_income2$`DDD/TID - lower`<-gaba_income2$lower
gaba_income2$`DDD/TID - upper`<-gaba_income2$upper

gaba_income3<-gaba_income2[c(1,2,6:9)]


library(DT)
datatable(gaba_income3, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(gaba_income3,"D:/R/midas gaba/R1/Output_12_Sens2_Subgroup_AAPC_income_meta.csv")


# Sensitivity analysis 3: without imputed data   
## Reading data  
library(readxl)
library(dplyr)
# data <- read.csv("D:/MIDAS ADHD/midas_adhd/adhd/MIDAS 2019.csv")
# data
getwd()
setwd("D:/R/midas gaba")
gaba<-subset(read.csv("D:/R/midas benzo/raw/gaba_extract.csv"),MOL=="PREGABALIN"|
               MOL=="GABAPENTIN"|
               MOL=="GABAPENTIN ENACARBIL"|
               MOL=="MIROGABALIN")
gaba$Class<-"Gabapentinoids"


## Keep SU and DDD data 
master<-gaba

## Separate data into with DDD units and without DDD units (INTWHODDDDESC NOT ASSIGNED)  

detach(package:plyr)
library(dplyr)
#Identify products that need conversion from SU to INTDDD
missing_DDD<-subset(master,INTWHODDDDESC=="INTWHODDD DESC NOT ASSIGNED")
with_DDD<-subset(master,INTWHODDDDESC!="INTWHODDD DESC NOT ASSIGNED")

# #Export list for conversion factor identification
# miss_DDD<-unique(missing_DDD[c(1:8)])
# write.csv(miss_DDD,"D:/R/midas benzo/dosage_convert.csv")

#Daniel and Andrew has done the conversion
convert <- read.table(
  "D:/R/midas benzo/convert_20221114_removed.txt",
  sep="\t", header=TRUE)
convert2<-subset(convert, !is.na(convert$Strength.of.BZD.or.GABA) & (MOL=="PREGABALIN"|
                                                                       MOL=="GABAPENTIN"|
                                                                       MOL=="GABAPENTIN ENACARBIL"|
                                                                       MOL=="MIROGABALIN"))
convert_missing<-subset(convert, is.na(convert$Strength.of.BZD.or.GABA))

convert_medianimpute_strength <- convert2[c(1:10)] %>% group_by (CTY,SEC, MNF, ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,INTWHODDDDESC_2) %>% mutate(median=median(Strength.of.BZD.or.GABA, na.rm = TRUE)) 
convert_medianimpute_strength2<-unique(convert_medianimpute_strength[c(1:8,10,11)])
#left join convert to data
missing_DDD_convert <- left_join(missing_DDD,convert_medianimpute_strength2,by = c("CTY","SEC","MNF","ATC3","INTPRD","NFC123",  
                                                                                   "INTWHODDDDESC","MOL") )
write.csv(missing_DDD,"D:/R/midas gaba/withoutddd_539.csv")
missing_DDD_remain<-subset(missing_DDD_convert, !is.na(median) & Class=="Gabapentinoids")


missing_DDD_agg<-missing_DDD %>% group_by (CTY, SEC,MNF,ATC3,INTPRD,NFC123,INTWHODDDDESC,MOL,X_TYPE_,X_FREQ_,Class) %>% 
  summarise_at(vars(1:857),sum)

## Results of median imputed strengths:  
library(DT)
datatable(subset(convert_medianimpute_strength2), options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



## Aggregate separately for DDD and missing DDD dataframes 
### DDD
library(tidyr)
library(zoo)
library(dplyr)


ddd <-with_DDD

## Remove columns of other sales data e.g. LC, LCD, USD, CU, SU
ddd.2 <- ddd[-c(9:10)]%>%select(-contains(c("LC_MNF","LCD_MNF","USD_MNF","CU_MTH","SU_MTH")))
ddd.3 <- pivot_longer(ddd.2, cols=9:151, 
                      names_to = "Month", values_to = "DDD")
## Format date
ddd.3$Month<-str_remove(ddd.3$Month, "INTDDD_MTH_")
ddd.3<-ddd.3 %>% separate(Month, c("Month","Year"), 
                          sep = "([_])")
ddd.3$Year <- paste0("20", ddd.3$Year, sep = "")
ddd.3$Year<-as.numeric(ddd.3$Year)
ddd.3$Month<-as.numeric(ddd.3$Month)
ddd.3$Date <- as.yearmon(paste(ddd.3$Year, 
                               ddd.3$Month), "%Y %m")
ddd.4<-ddd.3[-c(10)]
ddd.4$DDD[is.na(ddd.4$DDD)]<-0


## Aggregate the consumption data by country, drug name, date  

ddd.5<-ddd.4

bind<-(ddd.5)

aggregate<-bind[-c(2:7)]
aggregate$CTY = sub(pattern = "(RETAIL)|(COMBINED)|(COMBINE)|(COMBIN)|(COMBI)|(RET)|(R.MUTUALES)|(HOSPITAL)|(TOTAL SALES)", 
                    replacement = "", x = aggregate$CTY, perl = TRUE)
aggregate.2<-aggregate %>% group_by (CTY, MOL,Class,Year, Date) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)


## Merge rx data with UN population data  
library(readxl)
library(dplyr)
#population size from UN====
pop <- read_excel(path = "D:/R/midas adhd/ref data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx",
                  sheet = "ESTIMATES",
                  range = "B17:DE18122")
pop.2<-subset(pop, Type=="Country/Area")
pop.4<-pop.2[-c(1,3:63,77:108)]
names(pop.4)[names(pop.4) == 'Region, subregion, country or area *'] <- 'Country'

pop.5 <- pivot_longer(pop.4, cols=2:14,
                      names_to = "Year", values_to = "Population")

pop.5$Country <- toupper(pop.5$Country)
names(aggregate.2)[names(aggregate.2) == 'CTY'] <- 'Country'
aggregate.2$Country <-trimws(aggregate.2$Country)
'%ni%' <- Negate('%in%')

ddd.set.2<- subset(aggregate.2, !Country=="C. AMERICA"&
                     !Country=="FR. W. AFRICA"&
                     !Country=="FRENCH WEST AFRICA"&
                     !Country=="CENTRAL AMERICA")
pop.7<-pop.5
rename <- pop.7 %>%filter(Country %ni% ddd.set.2$Country) 
rename<-unique(rename$Country)

ddd.set.2[ddd.set.2$Country == "CZECH", "Country"] <- "CZECH REPUBLIC"
ddd.set.2[ddd.set.2$Country == "NETHERLNDS", "Country"] <- "NETHERLANDS"
ddd.set.2[ddd.set.2$Country == "RUSSIAN FED.", "Country"] <- "RUSSIA"
ddd.set.2[ddd.set.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
ddd.set.2[ddd.set.2$Country == "UAE", "Country"] <- "UNITED ARAB EMIRATES"
ddd.set.2[ddd.set.2$Country == "UK", "Country"] <- "UNITED KINGDOM"
ddd.set.2[ddd.set.2$Country == "USA", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "US", "Country"] <- "UNITED STATES"
ddd.set.2[ddd.set.2$Country == "S. AFRICA", "Country"] <- "SOUTH AFRICA"
ddd.set.2[ddd.set.2$Country == "BOSNIA", "Country"] <- "BOSNIA AND HERZEGOVINA"
ddd.set.2[ddd.set.2$Country == "KOREA", "Country"] <- "SOUTH KOREA"
pop.7<-pop.5
pop.7[pop.7$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
pop.7[pop.7$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
pop.7[pop.7$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
pop.7[pop.7$Country == "TURKEY", "Country"] <- "TÜRKİYE"
pop.7[pop.7$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
pop.7[pop.7$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
pop.7[pop.7$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"

pop.8<-as.data.frame(pop.7)
ddd.set.2$Year<-as.numeric(ddd.set.2$Year)
pop.8$Year<-as.numeric(pop.8$Year)
ddd.set.3.1<-ddd.set.2
ddd.set.3 <- left_join(ddd.set.3.1,pop.8,by = c("Country","Year") )

## Merge data with Region categorisation====
Region <- read_excel(path = "D:/R/midas adhd/ref data/UNSD — Methodology.xlsx")
Region.2<-Region[c(6,9,16)]
names(Region.2)[names(Region.2) == 'Country or Area'] <- 'Country'
Region.2$Country <- toupper(Region.2$Country)
rename <- ddd.set.3%>%filter(Country %ni% Region.2$Country) 
rename<-unique(rename$Country)
Region.2[Region.2$Country == "CZECHIA", "Country"] <- "CZECH REPUBLIC"
Region.2[Region.2$Country == "REPUBLIC OF KOREA", "Country"] <- "SOUTH KOREA"
Region.2[Region.2$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
Region.2[Region.2$Country == "CHINA, TAIWAN PROVINCE OF CHINA", "Country"] <- "TAIWAN"
Region.2[Region.2$Country == "TURKEY", "Country"] <- "TÜRKİYE"
Region.2[Region.2$Country == "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "Country"] <- "VENEZUELA"
Region.2[Region.2$Country == "UNITED STATES OF AMERICA", "Country"] <- "UNITED STATES"
Region.2[Region.2$Country == "UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND", "Country"] <- "UNITED KINGDOM"

Region.3<-Region.2%>%filter(Country %in% ddd.set.3$Country) 
ddd.set.4 <- left_join(ddd.set.3,Region.3,by = c("Country") )
names(ddd.set.4)[names(ddd.set.4) == 'Sub-region Name'] <- 'Region'
ddd.set.4[ddd.set.4$Country == "TAIWAN", "Region"] <- "Eastern Asia"

ddd.set.5<-ddd.set.4[-c(9)]

## 1.9 Dave comment: change Latin America 
ddd.set.5[ddd.set.5$Region == "Latin America and the Caribbean", "Region"] <- "Central and South America and the Caribbean"
ddd.set.5[ddd.set.5$Region == "Sub-Saharan Africa", "Region"] <- "Southern Africa"

population<-pop.8
population$Population<-as.numeric(population$Population)

options(scipen=999)

## Final analytic dataset  
library(dplyr)
cty.all<-ddd.set.5

names(cty.all)[names(cty.all) == 'MOL'] <- 'Drug'
## create one df of monthly data just in case
cty.all.3<-cty.all[-c(7)] %>% group_by (Year,Date,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)

## numerator
cty.ddd.2<-cty.all[-c(5,7)] %>% group_by (Year,Class,Drug, Country, Region) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
##denominator
cty.pop<-population

#Aggregate benzos and gaba
#gaba
new_gaba<-subset(cty.ddd.2)
new_gaba<-new_gaba[-c(3)]
new_gaba<-new_gaba %>% group_by (Year,Country, Region,Class) %>% 
  summarise_at(vars(1),sum, na.rm = TRUE)
new_gaba$Drug<-"Gabapentinoids"
new_gaba$DDD[is.na(new_gaba$DDD)]<-0
cty.ddd.2<-rbind(cty.ddd.2,new_gaba)

#Add zeros
Drug<-sort(rep(c("Gabapentinoids","PREGABALIN","GABAPENTIN"),715))
Country<-(rep(unique(cty.ddd.2$Country),11))
Year<-sort(rep(c(2008:2018),65))

merge_zero <- data.frame(Country, Year)
merge_zero2<-do.call("rbind", replicate(3, merge_zero, simplify = FALSE))
merge_zero2$Drug<-Drug
cty.ddd.3 <- right_join(x=cty.ddd.2,y=merge_zero2, 
                        by=c("Year","Country","Drug"))

cty.ddd.4 <- left_join(x=cty.ddd.3,y=cty.pop, 
                       by=c("Year","Country"))

cty.ddd.5 <- distinct(left_join(x=cty.ddd.4[-c(2,5)],y=ddd.set.5[c(1,3,8)], 
                                by=c("Country")))

cty.ddd.5$DDD[is.na(cty.ddd.5$DDD)]<-0

#Divide
cty.ddd.5$DDDPTPD <-(cty.ddd.5$DDD/cty.ddd.5$Population)/365.25
cty.ddd.5$Drug<-str_to_title(cty.ddd.5$Drug)
analy<-subset(cty.ddd.5,Year!=2007 & Year!=2019 & Drug!="Vigbatrin"&Drug!="Mirogabalin")

library(DT)
datatable(analy, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

str(analy,give.attr=FALSE)


## Main Analysis: DDD/TID and trends     

## Overview of gabapentinoids consumption by Drug, Country, and Year   
library(ggplot2)
library(RColorBrewer)
new_set.2<-analy


## Calculate CIs of consumption in DDD/TID per year  
library(epitools)
CIs<-pois.approx(new_set.2$DDD, pt = new_set.2$Population*365.25, conf.level = 0.95)
pool.new_set<-cbind(new_set.2,CIs)
pool.new_set.2<-subset(pool.new_set)
pool.new_set.2$DDD<-round(pool.new_set.2$DDD, digits = 5)  



## Meta-analysis of DDD/TID by Year  

Drug indices: [1] "Gabapentin", [2] "Gabapentin Enacarbil", [3] "Pregabalin", [4]"Gabapentinoids"   
library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<-subset(analy, Drug!="MIROGABALIN")
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$Region <- factor(meta.gaba$Region, levels =
                             c("Northern America","Central and South America and the Caribbean",
                               "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                               "Australia and New Zealand" ,
                               "Eastern Asia" , "Central Asia",
                               "South-eastern Asia","Southern Asia" ,
                               "Western Asia", 
                               "Northern Africa","Southern Africa"
                             ))

meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$Region)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  est.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.random$Year<-rho
  est.random$Drug<-iseed
  est.random$`DDD/TID`<-exp(summary(m1_var)$TE.random)
  est.random$`DDD/TID - lower`<-exp(summary(m1_var)$lower.random)
  est.random$`DDD/TID - upper`<-exp(summary(m1_var)$upper.random)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  return(c(est.random,est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

#Multinational
gaba_global2<-as.data.frame(do.call(rbind, datout))[c(5:9)] 
gaba_global2$Year<-as.numeric(gaba_global2$Year)
gaba_global2$`DDD/TID`<-as.numeric(gaba_global2$`DDD/TID`)
gaba_global2$`DDD/TID - lower`<-as.numeric(gaba_global2$`DDD/TID - lower`)
gaba_global2$`DDD/TID - upper`<-as.numeric(gaba_global2$`DDD/TID - upper`)
gaba_global2$Area<-"Multinational"

gaba_global2$Drug<-c(rep(("Gabapentin"), 11),
                     rep(("Pregabalin"), 11),
                     rep(("Gabapentinoids"), 11))

gaba_global3<-subset(gaba_global2,Drug!="MIROGABALIN")

a<-ggplot(gaba_global3, aes(x = Year, y = `DDD/TID`*10, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  geom_ribbon(aes(ymin = `DDD/TID - lower`*10, ymax = `DDD/TID - upper`*10), alpha = 0.1, colour = NA) + 
  scale_x_continuous(breaks = c(2008:2018))+
  ggtitle("Pooled multinational gabapentinoids consumption over time")+
  ylab("Defined daily dose per 10000 inhabitants per day")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
a


##Regional
gaba_regional<-as.data.frame(do.call(rbind, datout))[c(14:18)] 

gaba_regional$Year<-as.numeric(gaba_regional$Year)
out <- unlist(gaba_regional)
Year<-out[1:33]
Drug<-out[34:66]
DDDTID<-out[67:528]
lower<-out[529:990]
upper<-out[991:1452]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(14, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper

region_name<-(rep(c("Northern America","Central and South America and the Caribbean",
                    "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                    "Australia and New Zealand" ,
                    "Eastern Asia" , "Central Asia",
                    "South-eastern Asia","Southern Asia" ,
                    "Western Asia", 
                    "Northern Africa","Southern Africa"),33))
newdata$Area<-region_name
newdata$Drug<-c(rep(("Gabapentin"), 154),
                rep(("Pregabalin"), 154),
                rep(("Gabapentinoids"), 154))
gaba_regional2<-newdata
gaba_regional2$`DDD/TID`<-gaba_regional2$DDDTID
gaba_regional2$`DDD/TID - lower`<-gaba_regional2$lower
gaba_regional2$`DDD/TID - upper`<-gaba_regional2$upper

gaba_regional3<-gaba_regional2[c(1,2,6:9)]
pool_Global_region<-rbind(gaba_global3,gaba_regional3)
pool_Global_region$Drug<- factor(pool_Global_region$Drug, levels = c("Gabapentinoids","Gabapentin","Gabapentin Enacarbil","Pregabalin"))
pool_Global_region$Area<-factor(pool_Global_region$Area,levels=c("Multinational","Northern America","Central and South America and the Caribbean",
                                                                 "Northern Europe", "Eastern Europe","Southern Europe", "Western Europe",
                                                                 "Australia and New Zealand" ,
                                                                 "Eastern Asia" , "Central Asia",
                                                                 "South-eastern Asia","Southern Asia" ,
                                                                 "Western Asia", 
                                                                 "Northern Africa","Southern Africa"))
pool_Global_region$Year<-as.numeric(pool_Global_region$Year)
pool_Global_region[is.na(pool_Global_region)]<-0
library(ggbreak)
a<-ggplot(pool_Global_region, aes(x = Year, y = `DDD/TID`, group=Drug, colour=Drug, fill=Drug))+ 
  geom_line() + 
  facet_wrap(.~Area,nrow=1)+
  geom_ribbon(aes(ymin = `DDD/TID - lower`, ymax = `DDD/TID - upper`), alpha = 0.1, colour = NA) +
  scale_y_break(c(20,62),scales = 0.1)+
  theme_classic()+
  scale_x_continuous(breaks = c(2008:2018))+ 
  theme(panel.border=element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.line=element_line(),axis.ticks.x = element_blank())+
  ggtitle("Pooled multinational gabapentinoids consumption over time")
a

write.csv(pool_Global_region,"D:/R/midas gaba/R1/Output_13_Sensitivity_3_Pooled_Gaba_consumption_CI.csv")

library(DT)
datatable(pool_Global_region[c(6,1:5)], options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))



## Average annual percentage change   
### Data view - showing which zeros were removed    
library(plyr)
library(Hmisc)
library(DT)
library(tibble)

lm.cty_gaba<-subset(pool.new_set.2)
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

datatable(lm.cty_gaba,  options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

### National level  
library(plyr)
library(Hmisc)
library(DT)
library(tibble)

lm.cty_gaba_2<-subset(lm.cty_gaba, DDD>0)

models <- dlply(lm.cty_gaba_2, .(Country,Drug), function(df) 
  lm(log(DDDPTPD) ~ Year, data = df))

coef<-sapply(models, function(df) summary(df)$coefficients[2])
lm.results<-data.frame(coef)

ad_extract_ci <- function(x){
  temp_lw <- confint.lm(x)[2,1]
  temp_up <- confint.lm(x)[2,2]
  return(c(temp_lw,temp_up))
}

lower<-unlist(lapply(lapply(models,ad_extract_ci),"[",1))
lm.results<-cbind(lm.results, lower)
upper<-unlist(lapply(lapply(models,ad_extract_ci),"[",2))
lm.results<-cbind(lm.results, upper)
pvalue<-sapply(models, function(df) summary(df)$coefficients[8])
lm.results<-cbind(lm.results, pvalue)
lm.results<-rownames_to_column(lm.results)
colnames(lm.results)[which(names(lm.results) == "rowname")] <- "Country"
lm.results$expcoef<-(exp(lm.results$coef)-1)*100
lm.results$explower<-(exp(lm.results$lower)-1)*100
lm.results$expupper<-(exp(lm.results$upper)-1)*100

lm.results2<-(lm.results) %>% separate(Country, c("Country", "Drug"), sep="[.]")


cty.lm.results<-distinct(left_join(lm.results2,pool.new_set.2[c(2,3,7)]))



### Global  
library(plyr)
library(lme4)
library(nlme)


asd<-subset(lm.cty_gaba_2, Drug=="Gabapentinoids")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_1<-cbind(mixed_est_2,mixed_pval)
mixed_results_1$Drug<-"Gabapentinoids"
mixed_results_1$expcoef<-(exp(mixed_results_1$est.)-1)*100
mixed_results_1$explower<-(exp(mixed_results_1$lower)-1)*100
mixed_results_1$expupper<-(exp(mixed_results_1$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Gabapentin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_2<-cbind(mixed_est_2,mixed_pval)
mixed_results_2$Drug<-"Gabapentin"
mixed_results_2$expcoef<-(exp(mixed_results_2$est.)-1)*100
mixed_results_2$explower<-(exp(mixed_results_2$lower)-1)*100
mixed_results_2$expupper<-(exp(mixed_results_2$upper)-1)*100

asd<-subset(lm.cty_gaba_2, Drug=="Pregabalin")
Global_model_1 <- lme(log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(form = ~ Year|Country), data = asd)
mixed_est<-intervals(Global_model_1, level = 0.95, which = "fixed")
mixed_est_2 <- data.frame(mixed_est$fixed)
mixed_pval<-anova(Global_model_1)
mixed_pval<-as.data.frame(mixed_pval)
mixed_results_3<-cbind(mixed_est_2,mixed_pval)
mixed_results_3$Drug<-"Pregabalin"
mixed_results_3$expcoef<-(exp(mixed_results_3$est.)-1)*100
mixed_results_3$explower<-(exp(mixed_results_3$lower)-1)*100
mixed_results_3$expupper<-(exp(mixed_results_3$upper)-1)*100


mixed_results_Global<-rbind(mixed_results_1,mixed_results_2,mixed_results_3,mixed_results_4)
datatable(mixed_results_Global, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))
```

### Regional  
#### Gabapentinoids  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(lm.cty_gaba_2$Region),my_update_function)
gabapentinoids.mlm_region<-rbindlist(lapply(unique(lm.cty_gaba_2$Region),
                                            my_update_function))
gabapentinoids.mlm_region<-as.data.frame(gabapentinoids.mlm_region)
gabapentinoids.mlm_region$Drug<-"Gabapentinoids"

#### Gabapentin  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
gabapentin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
gabapentin.mlm_region<-as.data.frame(gabapentin.mlm_region)
gabapentin.mlm_region$Drug<-"Gabapentin"


#### Pregabalin  
library(DT)
library(data.table)
regionalmlm<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
my_update_function <- function(x){
  
  regionalmlm2<-regionalmlm[regionalmlm$Region==x,]
  regional_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                      correlation = corAR1(), data = regionalmlm2)
  
  mixed_est<-intervals(regional_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(regional_lme)
  mixed_results_regional<-cbind(mixed_est_2,mixed_pval)
  mixed_results_regional$Model<-x
  mixed_results_regional$expcoef<-(exp(mixed_results_regional$est.)-1)*100
  mixed_results_regional$explower<-(exp(mixed_results_regional$lower)-1)*100
  mixed_results_regional$expupper<-(exp(mixed_results_regional$upper)-1)*100
  mixed_results_regional <- mixed_results_regional[2,]
  return(mixed_results_regional)
}

lapply(unique(regionalmlm$Region),my_update_function)
pregabalin.mlm_region<-rbindlist(lapply(unique(regionalmlm$Region),
                                        my_update_function))
pregabalin.mlm_region<-as.data.frame(pregabalin.mlm_region)
pregabalin.mlm_region$Drug<-"Pregabalin"
region_reg_mlm<-rbind(gabapentinoids.mlm_region,gabapentin.mlm_region, enacarbil.mlm_region, pregabalin.mlm_region)
```

### Result table    

bind_cty_lm<-cty.lm.results[c(2,10,1,7:9,6)]
names(bind_cty_lm)[7]<-"p-value"

bind_reg_lm<-region_reg_mlm
bind_reg_lm$Country<-bind_reg_lm$Model
bind_reg_lm$Region<-bind_reg_lm$Model
bind_reg_lm<-bind_reg_lm[c(12,14,13,9:11,7)]

bind_glo_lm<-mixed_results_Global[c(2,4,6,8),]
bind_glo_lm<-bind_glo_lm[c(8:11,7)]
bind_glo_lm$Country<-"Multinational"
bind_glo_lm$Region<-"Multinational"
bind_glo_lm<-bind_glo_lm[c(7,6,1:5)]
bind_lm<-rbind(bind_cty_lm,bind_reg_lm,bind_glo_lm)
write.csv(bind_lm,"D:/R/midas gaba/R1/Output_14_Sensitivity_3_lm_cty_region_multi.csv")

datatable(bind_lm, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

## Subgroup: country income level  
### Gabapentinoids
library(readxl)

GDP <- read_excel("D:/R/midas gaba/Income.xlsx")

'%ni%' <- Negate('%in%')
GDP$country <- toupper(GDP$country)
rename <- lm.cty_gaba%>%filter(Country %ni% GDP$country) 
rename<-unique(rename$Country)
names(GDP)[names(GDP) == 'country'] <- "Country"
GDP[GDP$Country == "KOREA, REP.", "Country"] <- "SOUTH KOREA"
GDP[GDP$Country == "EGYPT, ARAB REP.", "Country"] <- "EGYPT"
GDP[GDP$Country == "RUSSIAN FEDERATION", "Country"] <- "RUSSIA"
GDP[GDP$Country == "SLOVAK REPUBLIC", "Country"] <- "SLOVAKIA"
GDP[GDP$Country == "VENEZUEL", "Country"] <- "VENEZUELA"
GDP[GDP$Country == "TURKEY", "Country"] <- "TÜRKİYE"
lm.cty_gaba$Year<-as.numeric(lm.cty_gaba$Year)

lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentinoids")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )


library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentinoids.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                            my_update_function))
gabapentinoids.mlm_income$Drug<-"Gabapentinoids"

datatable(as.data.frame(gabapentinoids.mlm_income[,c(8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

### Gabapentin  
lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Gabapentin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
gabapentin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
gabapentin.mlm_income<-as.data.frame(gabapentin.mlm_income)
gabapentin.mlm_income$Drug<-"Gabapentin"



### Pregabalin    
lm.cty_gaba_3<-subset(lm.cty_gaba_2,Drug=="Pregabalin")
gdp.2 <- left_join(lm.cty_gaba_3,GDP,by = c("Country") )

library(plyr)
library(lme4)
library(nlme)
library(DT)
my_update_function <- function(x){
  incomemlm<-gdp.2[gdp.2$income==x,]
  income_lme <- lme(fixed = log(DDDPTPD)~Year, random = ~1|Country,
                    correlation = corAR1(), data = incomemlm,
                    control =list(opt = "optim",msMaxIter = 1000, msMaxEval = 1000, tolerance = 1e-200))
  mixed_est<-intervals(income_lme, level = 0.95, which = "fixed")
  mixed_est_2 <- data.frame(mixed_est$fixed)
  mixed_pval<-anova(income_lme)
  mixed_results_income<-cbind(mixed_est_2,mixed_pval)
  mixed_results_income$Model<-x
  mixed_results_income$AAPC<-(exp(mixed_results_income$est.)-1)*100
  mixed_results_income$explower<-(exp(mixed_results_income$lower)-1)*100
  mixed_results_income$expupper<-(exp(mixed_results_income$upper)-1)*100
  mixed_results_income <- mixed_results_income[2,]
  return(mixed_results_income)
}
lapply(unique(gdp.2$income),my_update_function)
pregabalin.mlm_income<-rbindlist(lapply(unique(gdp.2$income),
                                        my_update_function))
pregabalin.mlm_income$Drug<-"Pregabalin"
pregabalin.mlm_income<-as.data.frame(pregabalin.mlm_income)

## Individual drugs income AAPC
drug_income_mlm<-rbind(gabapentinoids.mlm_income,gabapentin.mlm_income, pregabalin.mlm_income)
datatable(as.data.frame(drug_income_mlm[,c(12,8:11,7)]),caption = "Average annual percentage change by income level", options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(drug_income_mlm,"D:/R/midas gaba/R1/Output_15_Sensitivity_3_Subgroup_AAPC_income.csv")

### Meta-analysis of DDD/TID by Year  
library(Rcpp)
library(meta)
library(data.table)
library(dplyr)
set_subzero<- left_join(analy,GDP,by = c("Country") )
set_subzero$DDD_dum=set_subzero$DDD
set_subzero$DDD_dum[set_subzero$DDD==0]<-0.00001
CIs<-pois.approx(set_subzero$DDD_dum, set_subzero$Population*365.25, conf.level = 0.95)
meta.gaba<-cbind(set_subzero,CIs)
meta.gaba$income <- factor(meta.gaba$income, levels =
                             c("HIC","UMIC","LMIC"))
meta <- function(rho, iseed){
  meta.gaba_1<-  subset(meta.gaba,  Year==rho & Drug==iseed)
  
  m1_var<-metagen(log(meta.gaba_1$rate),
                  lower = log(meta.gaba_1$lower), 
                  upper = log(meta.gaba_1$upper),
                  studlab = meta.gaba_1$Country, 
                  sm = "IRLN", method.tau = "DL", 
                  comb.fixed = TRUE, 
                  byvar = meta.gaba_1$income)
  
  print(c(rho, iseed))
  print(summary(m1_var), digits=4)
  
  est.by.random<-c("Year", "DDD/TID", "DDD/TID - lower","DDD/TID - upper")
  est.by.random$Year<-rho
  est.by.random$Drug<-iseed
  est.by.random$`DDD/TID`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$TE)))))
  est.by.random$`DDD/TID - lower`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$lower)))))
  est.by.random$`DDD/TID - upper`<-(t(data.frame(as.list(exp((summary(m1_var))$within.random$upper)))))
  est.by.random$income<-(t(data.frame(as.list(((summary(m1_var))$byvar)))))
  return(c(est.by.random))
}


datin <- expand.grid(rho = unique(meta.gaba$Year), iseed = unique(meta.gaba$Drug))
i <- 1:nrow(datin)
datout <- with(datin,
               lapply(i, function(j){meta(rho[j],  iseed[j])}))

##income
gaba_income<-as.data.frame(do.call(rbind, datout))[c(5:10)] 

gaba_income$Year<-as.numeric(gaba_income$Year)
out <- unlist(gaba_income)
Year<-out[1:33]
Drug<-out[34:66]
DDDTID<-out[67:165]
lower<-out[166:264]
upper<-out[265:363]

out2<-data.frame(Year,Drug)
out3<-do.call("rbind", replicate(3, out2, simplify = FALSE))
newdata <- out3%>% arrange(Drug, Year)

newdata$DDDTID<-DDDTID
newdata$lower<-lower
newdata$upper<-upper
region_name<-(rep(c("HIC","UMIC","LMIC"),33))
newdata$Income<-region_name

newdata$Drug<-c(rep(("Gabapentin"), 33),
                rep(("Pregabalin"), 33),
                rep(("Gabapentinoids"), 33))
gaba_income2<-newdata
gaba_income2$`DDD/TID`<-gaba_income2$DDDTID
gaba_income2$`DDD/TID - lower`<-gaba_income2$lower
gaba_income2$`DDD/TID - upper`<-gaba_income2$upper

gaba_income3<-gaba_income2[c(1,2,6:9)]


library(DT)
datatable(gaba_income3, options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px', targets = c(1, 3)))
))

write.csv(gaba_income3,"D:/R/midas gaba/R1/Output_16_Sensitivity_3_Subgroup_AAPC_income_meta.csv")

