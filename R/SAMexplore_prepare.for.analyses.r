# Script to prepare SAMexplore data for analyses via theoretical model and Boruta/RandomForest
# Written by Milou Sep

# Packages & data ---------------------------------------------------------
library(dplyr)
library(tidyselect) # for all_of()
# imputed data
readRDS("processed_data/SAMimputed_with.AUC.variables.RDS")->implist # Actual data!
#readRDS("data/simulated.implist.RDS")->implist # simulated data for sharing & code check

# imputation setting
M=10

# # to prepare implist with raw data (for descriptive tables)
# readRDS("processed_data/SAMimputed.andRAW_with.AUC.variables.rds")->implist
# M=M+1

# Variable Selection ------------------------------------------------------

# Collect questionnaire total and subscale scores in vectors
scl.subscales<-c("SCL_SOM_total", "SCL_OC_total", "SCL_IS_total", "SCL_DEP_total", "SCL_ANX_total", "SCL_HOS_total", "SCL_PHOB_total", "SCL_Sleep_total")
jtv.subscales<-c("JTV_PA_total", "JTV_EA_total", "JTV_SA_total", "JTV_PN_total", "JTV_EN_total")
hexaco.subscales <- c("HEXACO_hh.mean", "HEXACO_emo.mean", "HEXACO_extr.mean", "HEXACO_ag.mean", "HEXACO_co.mean", "HEXACO_op.mean")
vtci.subscales <- c( "VTCI_SD_total",  "VTCI_NS_total", "VTCI_CO_total", "VTCI_HA_total",  "VTCI_ST_total",  "VTCI_RD_total", "VTCI_PS_total")

all.t0.total<-c( "SCL_total", "JTV_total", "LSC_event_total", "STAI_T_total" , 
                 #  scl.subscales, jtv.subscales,
                 hexaco.subscales, vtci.subscales)
# Collect demographic variables
demografics<-c("subject", 
               "BMI", "age", "Condition", 
               "roken", "alcohol", "drugs", 
               "psychol", "lich_ziekte",  
               "nachtritme_verst")
# collect memory measures
outcomes<-c("fear.memory", "fear.learning", "neutral.memory", "emotional.memory")

# Merge all variables together
varsRF<-c(demografics,all.t0.total,outcomes)

# Select variables above
implist2<-lapply(1:M, function(x){
   # x=1
  implist[[x]]->x.new
  x.new %>% select(all_of(varsRF), grep("AUC", names(.), value = T))-> x.new2
 return(x.new2)
})

# # to save intermediate data (with raw data in implist)
# saveRDS(implist2, "processed_data/implist.withRAW11.RDS")

# z-scores ----------------------------------------------------------------
# All numeric variables are transformed to z-scores and a cumulative stress measure is calculated

# Stress-exposure score:
# This score is based on the cumulative score used by [Kok & Sep etal. 2016](https://www.sciencedirect.com/science/article/pii/S0165032716304530?via%3Dihub) 
# and [Vinkers etal. 2014](https://doi.org/10.1002/da.22262): 
# "A comprehensive assessment of stressful experiences during life is more informative with respect to an individual's vulnerability for psychopathology than focusing on a single stress domain" (Vinkers et al., 2014). 
# Therefore, z-scores of CTQ and LSC-R outcomes were used to calculate a stress exposure score (Vinkers et al., 2014).

# Kok et al. (2016): stress-exposure: Centered variable composed by adding up centered CTQ and LSC-R scores
# Vinkers et al. (2014): "LSC-r scored as `The number of experienced major life events was calculated by summing the positively scored items`." Cumulative score: 
# 1. Levels of childhood maltreatment, major life events, and daily hassles were standardized to a z-score
# 2. Each stress domain weighed equally in its contribution to the cumulative stress index
# 3. By definition, the average standardized z-score is 0 for each stress domain. In contrast, this is not the case for the cumulative stress index that is the sum of three independent stress domains.

implist3<-lapply(1:M, function(x){
  # get dataframe
  implist2[[x]]->x.new
  # calculate z-scores
  x.new %>% 
    # https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
    mutate_if(is.numeric, ~as.vector(scale(.,center = TRUE, scale = TRUE))) ->x.new2 # solution to lose attributes (from scale)
  # create cumulative stress measure by sum of centered z-scores:
  x.new2$stress_exposure<- x.new2$JTV_total + x.new2$LSC_event_total
  return(x.new2)
})
# implist3[[3]]
  

# Split experimental conditions -------------------------------------------

# create empty lists, so each imputed dataset can be split in the 3 conditions
imp.nos<-list()
imp.ims<-list()
imp.des<-list()
# The combined dataset is subsequently split in the 3 experimental conditions.
for(i in 1:M){
  # select the imputed dataset
  implist3[[i]]->x.new
  # add imputation index
  # x.new$.imp<-rep(i, nrow(x.new))
  # and split this dataset per condition
  split(x.new, x.new$Condition)->df_splitted
  # add each list element to the imputations lists for each experimental condition
  imp.nos[[i]]<- df_splitted$`No-Stress`
  imp.ims[[i]]<- df_splitted$`Immediate-Stress`
  imp.des[[i]]<- df_splitted$`Delayed-Stress`
}
rm(df_splitted, x.new)

# For Boruta analysis / random forest
# functions to prepare data for the 3 outcomes

prep.emo<-function(data){
  data %>%
    mutate(subject=as.numeric(subject) ) %>% # random forest use a categorical variable with more than 53 categories (= factor subject)
    filter(!is.na(emotional.memory)) %>% # only emotional & neutral contain missing values
    select(-c(neutral.memory, fear.learning, fear.memory, Condition))->df
  return(df)
}

prep.neu<-function(data){
  data %>%
    mutate(subject=as.numeric(subject) ) %>%
    filter(!is.na(neutral.memory)) %>%
    select(-c(emotional.memory, fear.learning, fear.memory, Condition))->df
  return(df)
}
  
prep.fear.m<-function(data){
    data %>%
    mutate(subject=as.numeric(subject) ) %>%
    filter(!is.na(fear.memory))%>%
    select(-c(emotional.memory, neutral.memory, Condition))->df
 return(df)
}

