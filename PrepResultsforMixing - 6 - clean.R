## Process Item #6
# Description: Script to extract results from sensitivity analysis models and 
  # check for consistency in number of evaluation points across models. If ntiles 
  # were not unique, duplicate marginal random effects to match number of eval points.
# Author: Kate Nelson
# Date updated: 01/10/2023


# Setup data and functions ----

library(pacman)
source('Scripts/SA_functions_02122024.R')

p_load(tidyverse, sf, spdep, foreach, tigris, INLA, doParallel)
wd<-getwd()




# Code below is for a single crop (alfalfa). Repeat for each crop #



# Check for consistent number of eval points across all models  ----


alfalfa_models <-list.files("Output/", pattern = ("alfalfa*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]

fixlist <- c()

foreach (i=1:length(model_results)) %do% {
  
  results <- readRDS(paste0("Output/",model_results[i]))
  
  
  if(length(results$marginals.random[[6]]) != 10){
    print(paste0("ONLY ", length(results$marginals.random[[6]]),
                 " EVAL POINTS for ", model_results[i], "!"))
    fixthis <- model_results[i]
    fixlist <- append(fixlist, fixthis)
  }
  
  rm(results)
}

fixlist <- unique(fixlist)


# Fix missing evaluation points ----

dup_missing_evals <- function(metric, crop, n){
  
  vals <- df_new_met %>%
    select(all_of({{metric}})) %>%
    as.vector()
  
  q <- vals[[1]] %>%
    quantile(., probs = seq(0.1,1, by = 100/n/100), na.rm =T)
  
  uq <- unique(q)
  
  
  ###Need to reconcile q with uq
  
  q_eval <- q[1:length(q)] 
  uq_eval <- uq[1:length(uq)]
  
  q_eval <- q_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(q_eval)))
  
  uq_eval <- uq_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(uq_eval)))
  
  
  counts_X <- q_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  counts_Y <- uq_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  merged_counts <- full_join(counts_Y, counts_X, by = "Value", suffix = c("_Y", "_X"))
  
  # Identify elements in Y that need to be repeated to reproduce X
  repeated_elements <- merged_counts %>%
    filter(n_Y < n_X)
  
  rep_these <- repeated_elements %>%
    # filter(ID_X != 1) %>%
    mutate(times = n_X - n_Y) %>%
    distinct(Value, ID_Y, .keep_all = TRUE) %>%
    ungroup()
  
  rep_IDs <- rep(rep_these$ID_Y, rep_these$times)
  
  # rep_IDs <- unique(rep_these$ID_Y) #The IDS that need to be repeated
  
  uq_eval <- uq[1:length(uq)]
  
  dup_evals <- sort(append(uq, uq[rep_IDs], after = max(rep_IDs))) #check to make sure it works before changing model output
  
  # Now duplicate elements in model output based on index of duplicate quantile values
  
  r <- readRDS(paste0("Output/", metric, crop, "_ntiles_02142024.rds"))
  rw.a <- r$marginals.random[[6]]
  
  rw.a.dup <-append(rw.a, rw.a[rep_IDs], after = max(rep_IDs))
  rw.a.dup <- rw.a.dup[order(names(rw.a.dup))]
  
  r$marginals.random[[6]] <- rw.a.dup
  
  saveRDS(r,paste0("Output/", metric, crop, "_ntiles_02142024.rds")) 
} #function for duplicating missing ntiles



 df_new_met <- readRDS("PreppedData/alfalfa_ntiles_02142024.rds") #Read in base data 

##Set up loop to duplicate random marginals when ntiles were not unique ----

n <- 10
metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
crop <- "alfalfa"

foreach (i=1:length(fixlist)) %do% {
  n <- 10
  metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
  dup_missing_evals(metric, crop, n)
}


### Check to make sure it worked ----

fixmore <- c()

foreach (i=1:length(fixlist)) %do% {
  
  results <- readRDS(paste0("Output/",fixlist[i]))
  
  
  if(length(results$marginals.random[[6]]) != 10){
    print(paste0("ONLY ", length(results$marginals.random[[6]]),
                 " EVAL POINTS for ", fixlist[i], "!"))
    fixthis <- fixlist[i]
    fixmore <- append(fixmore, fixthis)
  }
  
  rm(results)
}

fixmore <- unique(fixmore)



