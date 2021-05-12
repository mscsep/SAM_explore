# Functions to score questionnaire data from the SAM study.
# written by Milou Sep

# scoring of T0 questionnaires --------------------------------------------

# 1) Function to calculate sum score
sum_scoring_sam<- function(df, items, outcome, miss_limit){
# This function returns total sum score of <items>
  
  varname.total= paste0(outcome,"_total")
  #items_quo=quo_name(enquo(items)) #not needed...
  
  # Step 1 select data for scoring
  df %>% select(!!!syms(items)) -> df2 # select columns in data set that correspond to names in <items>
  
  # Step 2 define quosures to calculate sum and cutoff-scores
  quo.sum <- quo(rowSums(df2))
  quo.missing.quo = quo(rowSums(is.na(df2)))
  quo.sum.missing.corrected =  ifelse(rlang::eval_tidy(quo.missing.quo) > miss_limit, NA, rlang::eval_tidy(quo.sum) )
  
  #  Step 5 'execute' all quotations in dplyr > mutate to create new variables 
  df2 %>% dplyr::mutate(!!sym(varname.total) := !!quo(quo.sum.missing.corrected)) -> df3
  
  # Step 6: return only added columns
  df3 %>% dplyr::select(!!sym(varname.total)) %>% droplevels() ->df4

  # add new col to original dataframe
  cbind(df, df4) -> df6
  
  return(df6)
  
}


# 2) Function to calculate sum  AND cutoff scores
sum_cutoff_scoring_sam<- function(df, items, outcome, miss_limit, cutoff){
  # this function returns total score + cutoff of the <items> 
  # CUTOFF bigger or equal to!
  
  # Define outcome variable names for each subscale:
  varname.total= paste0(outcome,"_total")
   varname.cutoff= paste0(outcome, "_cutoff")
  
  df %>% select(!!!syms(items)) -> df2 # select only items 
  
  quo.sum <- quo(rowSums(df2))
  quo.missing.quo = quo(rowSums(is.na(df2)))
  quo.sum.missing.corrected =  ifelse(rlang::eval_tidy(quo.missing.quo) > miss_limit, NA, rlang::eval_tidy(quo.sum) )
  
  quo.total_cutoff <- ifelse(rlang::eval_tidy(quo.sum.missing.corrected) >= cutoff, yes=1,no=0)
 # quo.total_cutoff_labeled = factor(quo.total_cutoff,levels=c(1,0), labels = c("High", "Low"))    # do not label, then you cannot use 0 and 1 later in e.g. logical tests etc.

  df2 %>% 
    dplyr::mutate(!!sym(varname.total) := !!quo(quo.sum.missing.corrected),
                  !!sym(varname.cutoff) := !!quo(quo.total_cutoff)) -> df3
  
  # Step 6: return only added columns
  df3 %>% 
    dplyr::select(!!sym(varname.total), !!sym(varname.cutoff)) %>% droplevels() ->df4
  
  # add new col to original dataframe
  cbind(df, df4) -> df6
  
  # return output
  return(df6)
  
}


# Scoring STAI State at T2-T10 ---------------------------------------------

# 1) recode STAI-State
recode_STAIS_SAM<-function(df, timepoint){
  # This function uses quosures (a data structure that captures an expression along with its associated environment, as found in function arguments.). 
  # for more info see: https://stackoverflow.com/questions/46206677/concatenate-quosures-and-string
  
  # Define item names
  items= purrr::map(c(1:20),function(x) {paste0("StaiSt",timepoint,"__",LETTERS[x])})  # There are 20 items in PANAS
  items= unlist(items)  # Map returns a list, and dplyr can not use indices with a list, therefor unlist()
  
  # recode variables STAI-S (items 1,2,5,8,10,11,15,16,19,20)
  c("A", "B", "E", "H", "J", "K", "O", "P", "S", "T")-> recode_letters
  which(LETTERS %in% recode_letters)-> recode_items
  items[recode_items] ->recode_names # select names of the items that need to be recoded.
  
  df %>% 
    mutate_at(recode_names ,function(x){5-x}) -> df
  
  df %>% select(!!!syms(items)) -> df2
  
  return(df2)
  
}

# 2) score STAI-State
score_STAIS_SAM<-function(df, timepoint){
  # This function uses quosures (a data structure that captures an expression along with its associated environment, as found in function arguments.). 
  # for more info see: https://stackoverflow.com/questions/46206677/concatenate-quosures-and-string
  
  # Define outcome variable names for each subscale:
  varname.STAIS.total= paste0("STAIS_Total_",timepoint)
  varname.STAIS.cutoff= paste0("STAIS_CutOff_",timepoint)
  
  # Define item names
  items= purrr::map(c(1:20),function(x) {paste0("StaiSt",timepoint,"__",LETTERS[x])})  # There are 20 items in PANAS
  items= unlist(items)  # Map returns a list, and dplyr can not use indices with a list, therefor unlist()
  
  # select STAI data
  # df %>% select(items) -> stais.data
  df %>% select(!!!syms(items)) -> stais.data
  
  # quosures 
  # calculate sum scores
  sum.quo = quo(rowSums(stais.data))
  # count missing values
  missing.quo = quo(rowSums(is.na(stais.data)))
  
  # Quosures that correct sum scores for missing values. see ifelse & quotation... https://github.com/tidyverse/dplyr/issues/3063
  sum.missing.corrected =  ifelse(rlang::eval_tidy(missing.quo) !=0, NA, rlang::eval_tidy(sum.quo) )
  
  # create cutoff score
  stais_total_cutoff <- ifelse(rlang::eval_tidy(sum.missing.corrected) <=39, 0,1) # Other cut-off values can also be used?
  stais_total_cutoff_labeled = factor(stais_total_cutoff,levels=c(0,1), labels = c("Low State Anxiety", "High State Anxiety"))
  
  #  'execute' all quotations in dplyr > mutate to create new variables for each subscale.
  df %>% 
    dplyr::mutate(!!sym(varname.STAIS.total) := !!quo(sum.missing.corrected),
                  !!sym(varname.STAIS.cutoff) := !!quo(stais_total_cutoff_labeled)) -> df2
  
  # return only added columns
  df2 %>% 
    dplyr::select(!!sym(varname.STAIS.total), !!sym(varname.STAIS.cutoff)) %>% droplevels() ->df3
  
  return(df3)
}



# PANAS scoring function T2-T10 --------------------------------------------

PANAS_SAM <- function (df, timepoint){
  # Assign value of timepoint to function environment. See https://stackoverflow.com/questions/46206677/concatenate-quosures-and-string
  # NB. The quosure: a data structure that captures an expression along with its associated environment, as found in function arguments.
  
  # Define outcome variable names for each subscale:

  varname.PA= paste0("PANAS_",timepoint,"_PosA")
  varname.NA= paste0("PANAS_",timepoint,"_NegA")
  
  # Define item names
  items= purrr::map(c(1:20),function(x) {paste0("PANAS",timepoint,"__",LETTERS[x])})  # There are 20 items in PANAS
  # items1=quo(unlist(items2))
  items=unlist(items) # Map returns a list, and dplyr can not use indices with a list, therefor unlist()
  
  # The PANAS items in the dataset are indicated with letters (not numbers). For each subscale a vector with the item letters is created. 
  # Subsequently, the r-build in vector "LETTERS" is used look for the index numbers that correspond to each alphabet letter.
  
  # *** PANAS Positive Affect score  ***
  c("A", "C", "E", "I", "J", "L", "N", "P", "Q", "S")-> PosA_letters
  which(LETTERS %in% PosA_letters)->PosA_items
  
  #*** PANAS Negative Affect score  ***
  c("B", "D", "F", "G", "H", "K", "M", "O", "R", "T") -> NegA_letters
  which(LETTERS %in% NegA_letters)->NegA_items
  
  #   # Create quosures that include the calculations of the sum score of both subscales. (NOTE dplyr mutate can only work with unquoted (!!) quosures, 
  #   # therefore they need to be defined before entering the dplyr pipeline. See: https://shipt.tech/https-shipt-tech-advanced-programming-and-non-standard-evaluation-with-dplyr-e043f89deb3d
  # https://tidyeval.tidyverse.org/dplyr.html
  
  pos.names= (items[PosA_items])
  neg.names= (items[NegA_items])
  
  # set dataframes
  df %>% select(!!!syms(pos.names)) -> pos.data  # quasiquotation is used here: https://www.rdocumentation.org/packages/rlang/versions/0.2.2/topics/quasiquotation
  df %>% select(!!!syms(neg.names)) -> neg.data
  
  # calculate sum scores
  pos.sum.quo = quo(rowSums(pos.data))
  pos.missing.quo = quo(rowSums(is.na(pos.data)))
  # count missing values
  neg.sum.quo = quo(rowSums(neg.data))
  neg.missing.quo = quo(rowSums(is.na(neg.data)))
  
  # Quosures that correct sum scores for missing values. see ifelse & quotation... https://github.com/tidyverse/dplyr/issues/3063
  pos.sum.missing.corrected =  ifelse(rlang::eval_tidy(pos.missing.quo) !=0, NA, rlang::eval_tidy(pos.sum.quo) )
  neg.sum.missing.corrected =  ifelse(rlang::eval_tidy(neg.missing.quo) !=0, NA, rlang::eval_tidy(neg.sum.quo) )
  
  # # 'execute' all quotations in dplyr > mutate to create new variables for each subscale.
  df %>% 
    dplyr::mutate(!!sym(varname.PA) := !!quo(pos.sum.missing.corrected),
                  !!sym(varname.NA) := !!quo(neg.sum.missing.corrected)) -> df2
  
  # return only added columns
  df2 %>% 
    dplyr::select(!!sym(varname.PA), !!sym(varname.NA)) %>% droplevels() ->df3
  
  return(df3)
}