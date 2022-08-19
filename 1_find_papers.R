#This first step of the workflow should

# 1) pull a list of all eco/evo papers published in the last 10 years or so
# 2) randomly order them
# 3) assign unique IDs
# 4) create empty google drive folders for each
# 5) Publish list of papers as google doc

# 6) Manual step: look into papers, download R files where possible

# 7) identify packages used by papers
# 8) generate a google docs spreadsheet for e.g. the first 100 papers, should list all the packages used
################################################################################


library(wosr)#doesn't work?
library(pubmedR)
library(rscopus)
library(tidyverse)


#wosr package seems out of date
  # auth(username = NULL,
  #      password = NULL)


#pubmed


?pmApiRequest

test <- pmApiRequest(query = "Ecology[subject] AND 2000:2022[dp]",limit = 10)
test <- pmApiRequest(query = "(Ecology[mh] or Evolution[mh]) AND
                              2000:2022[dp] AND
                              english[LA] AND
                              R A Language and Environment for Statistical Computing[citing]
                              ",
                     limit = 1000)

test <- pubmedR::pmApi2df(test)

#scopus


out_df <- df$df


#set year thresholds
  min_year <- 2010
  max_year <- 2022

out_df <- NULL
for(i in min_year:max_year){

  res <- scopus_search(query = paste("all(ecology)
                              AND SUBJAREA(AGRI)
                              AND pubyear = ", i,
                              "AND LANGUAGE(english)
                    AND REF('R Core Team')
                    AND srctype(j)"),
                       count = 25,
                       max_count = 5000)
  
  
  n_records <- res$total_results
  
  if(n_records > 5000){stop("Brian, add a while() loop")}
  
  out_df <- bind_rows(out_df,gen_entries_to_df(res$entries)$df)

}#year loop



  
# save df of articles


# reorder articles so that they are stratified

  #randomize order
  # group by year
  # add unique identifier to each record (within years)
  # reorder by year, record number

















