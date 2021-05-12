# Code to create fake pseudo-data for independent code check (VB); may 2020
# will be input for "SAMexplore_prepare.for.analyses.r"

rm(list=ls())
readRDS("processed_data/SAMimputed_with.AUC.variables.RDS")->implist
library(fakeR)
library(dplyr)

M=10
simulated.implist<-list()
# i=2
for(i in 1:M){
  df<-data.frame(implist[[i]])
  # remove ordering: The ordered factors are changed to normal factors, otherwise error message, due to missing in these variables.
  df %>% mutate_at(.,c("roken", 'alcohol', 'drugs', 'nachtritme_verst'), ~factor(., ordered = F))-> df2
  # Note the condition column needs to be excluded (otherwise issues with splitting per condition later on)
  simulate_dataset(df2, stealth.level = 1, ignore = c("Condition"))->simulated.other
  # outcomes simulated separably, otherwise 1 decimal numbers
  simulate_dataset(df2[,c("neutral.memory","emotional.memory","fear.memory","fear.learning")],  digits=10, stealth.level = 1) %>% data.frame() -> simulated.outcomes
  simulated.other$neutral.memory <-simulated.outcomes$neutral.memory
  simulated.other$emotional.memory <-simulated.outcomes$emotional.memory
  simulated.other$fear.memory <-simulated.outcomes$fear.memory
  simulated.other$fear.learning <-simulated.outcomes$fear.learning
  simulated.other -> simulated.implist[[i]]
}
saveRDS(simulated.implist,"data/simulated.implist.RDS")
# readRDS("data/simulated.implist.RDS")->implist