# Script to merge questionnaire, endocrine, memory performance data (MCT & FGT) from the SAM-study for the SAMexplore project
# Written by Milou Sep

rm(list=ls())
library(dplyr)
library(tidyr) # oa pivot_wider
library(readr)


# Questionnaire data ------------------------------------------------------
readRDS("processed_data/SAM_netq_scored.rds")->sam_netq

# rename to align with other datasets
dplyr::rename(data.frame(sam_netq), # note: without data.frame() errors with renaming 'haven-labeled' data.
              subject = SubjectNumber) %>%
  mutate(subject=sub("SMGH", "", subject )) -> sam_netq # remove "SMGH" from subject codes.
# str(sam_netq$Condition) # factor
# str(sam_netq$subject) # character


# Endocrine data ----------------------------------------------------------
# Preparations: the endocrine (salivary AA and Cortisol) is loaded (note, preprocessed with the code from github.com/mscsep/SAM_sAA_Cortisol)
endocrine_data <- readRDS("data/sAA_sCORT_condition_dataset.rds")

# rename to align with other datasets
dplyr::rename(endocrine_data, 
             # Group = Condition, # Rename Condition in Group  (for FGT paper)
              subject = id) -> endocrine_data 
# str(endocrine_data$subject) # note "SMGH" already removed (still factor)
# str(endocrine_data$Condition)
endocrine_data$subject <- as.character(endocrine_data$subject)
# endocrine_data$Condition <- factor(endocrine_data$Condition, levels=c(3,2,1), labels =c("No-Stress", "Immediate-Stress", "Delayed-Stress"))

endocrine_data$Condition <- factor(endocrine_data$Condition, levels=c("Control","Direct","Delayed"), labels =c("No-Stress", "Immediate-Stress", "Delayed-Stress"))
pivot_wider(endocrine_data, id_cols = c(subject, Condition, Sample), names_from = Sample, values_from = c(sAA, CORT) )-> endo_wide


# MCT data ----------------------------------------------------------------
## Neutral and Emotional Memories
# The variables that reflect neutral and emotional memory are derived from the Memory Contextualization Task [(MCT)](https://doi.org/10.1016/j.psyneuen.2019.06.021).

MCT <- read.csv2("data/SAM_MCT.csv", na.strings = 'NA')
# rename to align with other datasets
dplyr::rename(MCT, 
              subject = subjName)  %>%
  mutate(subject=sub("SMGH", "", subject )) -> MCT
# str(MCT$Condition)

MCT$Condition <- factor(MCT$Condition, levels=c(3,2,1), labels =c("No-Stress", "Immediate-Stress", "Delayed-Stress"))
# str(MCT$subject)# char

# The dPrime Contextualization Index for neutral (*N_dPrime_diff*) and negative information (*A_dPrime_diff*) are used from this dataset. Note these indices are also used in [Sep,2019](https://doi.org/10.1016/j.psyneuen.2019.06.021).
MCT %>% select(subject, Condition, N_dPrime_diff, A_dPrime_diff) %>%
  dplyr::rename(., 
                neutral.memory = N_dPrime_diff,
                emotional.memory = A_dPrime_diff) -> neutral.negative.memories
# is.na(neutral.negative.memories) # only missing in CMT, when complete CMT is missing 

# hist(neutral.negative.memories$neutral.memory, main = "neutral contextualization")
# hist(neutral.negative.memories$emotional.memory, main = "emotional contextualization")


# FGT data ----------------------------------------------------------------
# Context-dependency of fear memories is reflected by (the inverse) of a composite fear generalization score from the Fear Generalization Task (FGT).
FGT <- read.csv2("data/SAM_FGT.csv", na.strings = c("NaN","5555","8888","9999"))
# NB Missing codes (provided with matlab code):
# 5555 % If digital registration error, message on screen and MissingValue 5555.
# 8888 % Excessive baseline activity. (More then 2 SD)
# 9999 % Latency onset not valid (Note only present for in onset scores (should not be present in magnitude scores)

dplyr::rename(FGT, 
              subject = subjName)  %>%
  mutate(subject=sub("SMGH", "", subject )) -> FGT
# str(FGT$Condition)

FGT$Condition <- factor(FGT$Condition, levels=c(3,2,1), labels =c("No-Stress", "Immediate-Stress", "Delayed-Stress"))
# str(FGT$subject)# char

grep(pattern = "sG_Cue_Smag_T", x = names(FGT), value = TRUE, fixed=T) -> fgt.threat
grep(pattern = "sG_Cue_Smag_S", x = names(FGT), value = TRUE, fixed=T) -> fgt.safe
grep(pattern = "sG_Cue_Smag_N", x = names(FGT), value = TRUE, fixed=T) -> fgt.new

grep(pattern = "sA_Cue_Smag_T", x = names(FGT), value = TRUE, fixed=T) -> fgt.threat.acq
grep(pattern = "sA_Cue_Smag_S", x = names(FGT), value = TRUE, fixed=T) -> fgt.safe.acq


# This composite score is especially created for this project (and not reported before). 
# The Fear context-dependency Score is based on the mean Standardized Fear Potentiated Startle Response of participants to 
# the 1) Threat trials, 2) Safe trials, and 3) New trials in the Test phase of the FGT. 
  
FGT %>% mutate(mean.threat= rowMeans(.[fgt.threat]),
               mean.safe= rowMeans(.[fgt.safe]),
               mean.new= rowMeans(.[fgt.new]),
               mean.threat.acq=rowMeans(.[fgt.threat.acq]),
               mean.safe.acq=rowMeans(.[fgt.safe.acq]),
               
               # the score represents differential fear contextualization (or context-dependent fear acquisition)
               # The formula used is:  $Fear Contextualization Score  =  mean(FPS.threat) - mean(FPS.safe)$ 
               fear.learning= (mean.threat.acq - mean.safe.acq),
               
               # The formula used is:  $Fear memory context-dependency Score  = - (( mean(FPS.new) - mean(FPS.safe) )/ mean(FPS.threat))$ 
               # Note. this is the inverse of the formula for $fear generalization = ((mean(FPS.new) - mean(FPS.safe) )/ mean(FPS.threat))$ 
               # The Fear memory context-dependency Score defines fear context-dependency as "the inverse of fear generalization (which is the response to the stimulus in a novel context (corrected for the response to the stimulus in the safe context), 
               # as proportion of the response to this stimulus in the threat context". The inverse of the generalization index is selected, because a higher value of the generalization index 
               # reflects 'more generalization of fear' and thereby LESS context-dependency. While higher values in the *neutral* and  *emotional* memory indices reflect MORE contextualization.
               fear.memory = - ((mean.new-mean.safe) / mean.threat)
               )->FGT
  
FGT %>% select(subject, Condition, fgt.threat, fgt.safe, fgt.new, fgt.threat.acq, fgt.safe.acq,
               mean.threat, mean.safe, mean.new, mean.threat.acq, mean.safe.acq, fear.learning, fear.memory)->fear
# glimpse(fear)


# Merge dataframes --------------------------------------------------------
full_join(sam_netq, endo_wide, by=c("subject", "Condition"))->netq_endo
# glimpse(netq_endo)
full_join(netq_endo, neutral.negative.memories, by=c("subject", "Condition"))->netq_endo_mct
# glimpse(netq_endo_mct)
full_join(netq_endo_mct, fear, by=c("subject", "Condition"))->netq_endo_mct_fgt
# glimpse(netq_endo_mct_fgt)


# Save merged data --------------------------------------------------------
saveRDS(netq_endo_mct_fgt,"processed_data/SAM_complete.rds")
