#'@author Brian Maitner

#'@description
#' This code is designed to:
#' 
#' 1) pull a list of all eco/evo papers published since 2010 that DON'T cite R
#'  (alternatively, pull all papers and compare with the list of papers that DO)
#' 2) randomly order them
#' 3) assign unique IDs
#' 4) create empty google drive folders for each publication
#' 5) Publish the list of papers as google doc
#'
# Next, there is a manual step:
#'
# 6) Manual step: look into papers, recording information on any papers that included R code.

################################################################################


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
                                  AND srctype(j)
                                  AND NOT REFTITLE({R: A Language and Environment for Statistical Computing})
                                  "),
                       count = 25,
                       max_count = 5000,
                       api_key = api_key)
  
  
  # n_records <- res$total_results
  
  # if(n_records > 5000){stop("Brian, add a while() loop")}
  
  out_df <- bind_rows(out_df,gen_entries_to_df(res$entries)$df)
  
}#year loop

# clean up

rm(i, n_records, res, min_year, max_year)

#  save df of articles

  # saveRDS(object = out_df,
  #         file = "data/search_results_NOT_citing_R_4_22_2024.RDS")

# read in df if needed  

  papers <- readRDS(file = "data/search_results_NOT_citing_R_4_22_2024.RDS")

  # reorder articles so that they are stratified
  
  # toss unnecessary fields and rename others for convenience
  
  papers <- 
    papers %>%
    dplyr::select(c("dc:title",
                    "dc:creator",
                    "prism:publicationName",
                    "prism:issn",
                    "prism:volume",
                    "prism:pageRange",
                    "prism:coverDate",
                    "prism:coverDisplayDate",
                    "prism:doi",
                    "citedby-count",
                    "subtypeDescription",
                    "openaccess")) %>%
    rename(title =  'dc:title',
           author = 'dc:creator',
           journal = 'prism:publicationName',
           issn = 'prism:issn',
           volume = 'prism:volume',
           pages = 'prism:pageRange',
           date = 'prism:coverDate',
           display_date = 'prism:coverDisplayDate',
           doi = 'prism:doi',
           citations = 'citedby-count',
           article_type = subtypeDescription,
           open_access = openaccess ) %>%
    mutate(year = lubridate::year(date))
  
  # randomize order of publications
  
  papers <- papers[sample(x = 1:nrow(papers),size = nrow(papers),replace = FALSE),]
  
  #stratify sampling by year
  
  papers %>%
    group_by(year) %>%
    mutate(n = row_number())%>%
    arrange(n, year) %>%
    ungroup() %>%
    
    #add needed columns      
    
    mutate(uid = row_number(),
           uses_R = NA,
           comments = NA,
           doi_url = paste("https://doi.org/",doi,sep = "")) %>%
    
    #reorder for convenience
    
    select("uid",
           "doi_url",
           "uses_R",
           "comments",
           "title",
           "author",
           "year",
           "doi",
           "journal",
           "issn",
           "volume",
           "pages",
           "date",
           "display_date",
           "citations",
           "article_type",
           "open_access",
           "n"
    ) -> papers
  
  # replace doi_url = https://doi.org/NA with NA
  
  papers$doi_url[which(is.na(papers$doi))] <- NA
  
  # create a google sheet from papers df (can't specify location, so have to move the file)
  
  # googlesheets4::gs4_create(name = "papers_without_R_citations_2024_04_22.tmp",
  #                           sheets = papers)
  # 
  # drive_cp(file = "papers_without_R_citations_2024_04_22.tmp",
  #          path = "Manuscripts/R_citations/papers_without_R_citations_2024_04_22",
  #          overwrite = FALSE)
  # 
  # drive_trash("R_citations_2022_08_19.tmp")
  
  # change field settings and options on google sheets manually and then fill in by hand.
  
  R_in_non_R_papers <- read_sheet("1LB2f4r3kzQG5KYUuqRo8WGGJdnpQH10SDjJmoeedFGA",
                                  n_max = 300)    
  
  R_in_non_R_papers %>%
    filter(!is.na(uses_R))%>%
    group_by(year) %>%
    summarise(n=n())

  R_in_non_R_papers %>%
    filter(!is.na(uses_R))%>%
    group_by(year) %>%
    select(year,uses_R)%>%
    mutate(uses_R_bin = uses_R == "yes") %>%
    group_by(year) %>%
    summarise(n=n(),
              n_uses_R = sum(uses_R_bin))
  