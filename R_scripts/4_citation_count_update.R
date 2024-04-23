# citation update 

library(rscopus)
library(tidyverse)
library(googledrive)
library(googlesheets4)

source("api_key.R")

# Getting publication data via scopus

# set year thresholds
min_year <- 2010
max_year <- 2022

# prep empty output    
out_df <- NULL

# search by year (broken down by year because of 5k limit on records)

for(i in min_year:max_year){
  
  res <- scopus_search(query = paste("all(ecology OR evolution)
                                  AND SUBJAREA(AGRI)
                                  AND pubyear = ", i,
                                     "AND LANGUAGE(english)
                                  AND REFTITLE({R: A Language and Environment for Statistical Computing})
                                  AND srctype(j)"),
                       count = 25,
                       max_count = 5000)
  
  
  n_records <- res$total_results
  
  if(n_records > 5000){stop("Brian, add a while() loop")}
  
  out_df <- bind_rows(out_df,gen_entries_to_df(res$entries)$df)
  
}#year loop

# clean up

rm(i, n_records, res, min_year, max_year)

#  save df of articles
# saveRDS(object = out_df,
#         file = "data/search_results_2024_04_23.RDS")

# read in df if needed  


