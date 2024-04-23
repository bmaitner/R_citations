#'@author Brian Maitner

#'@description
#' This code is designed to:
#' 
#' 1) pull a list of all eco/evo papers published since 2010 that cite R
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
#         file = "data/search_results_2022_08_19.RDS")
  
# read in df if needed  
  
  papers <- readRDS(file = "data/search_results_2022_08_19.RDS")

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
           r_scripts_available = NA,
           comments = NA,
           folder = NA,
           recorded_by = NA,
           doi_url = paste("https://doi.org/",doi,sep = "")) %>%
    
      #reorder for convenience
        
    select("uid",
           "doi_url",
           "r_scripts_available",
           "folder",
           "recorded_by",
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
  

  # next, create a google drive folder for each paper  
    
    # googledrive::drive_mkdir("Manuscripts/R_citations/R_scripts")
      
    for(i in papers$uid){
      
      if(!is.na(papers$folder[which(papers$uid == i)])){next}
      
      message(round((i/length(papers$uid))*100,digits = 2)," pct done")
      
      if(round((i/length(papers$uid))*100,digits = 2)>=20){break} #for starters we'll only populate folders for 20% of papers.  Can't imagine needing even that many
  
      folder_md <- googledrive::drive_mkdir(name = paste("uid_",i,sep = ""),
                                            path = "Manuscripts/R_citations/R_scripts/")
      
      papers$folder[which(papers$uid == i)] <- folder_md$drive_resource[[1]]$webViewLink
      
      
    }

    # replace doi_url = https://doi.org/NA with NA
    
      papers$doi_url[which(is.na(papers$doi))] <- NA
    
    # save paper list
      
      # saveRDS(object = papers,
      #         file = "data/search_results_2022_08_19_w_drive_links.RDS")
      
    papers <- readRDS("data/search_results_2022_08_19_w_drive_links.RDS")
    
  # create a google sheet from papers df (can't specify location, so have to move the file)

    # googlesheets4::gs4_create(name = "R_citations_2022_08_19.tmp",
    #                           sheets = papers)
    # 
    # drive_cp(file = "R_citations_2022_08_19.tmp",
    #          path = "Manuscripts/R_citations/R_citations_2022_08_19",
    #          overwrite = FALSE)
    # 
    # drive_trash("R_citations_2022_08_19.tmp")

  # change field settings and options on google sheets manually and then fill in by hand.

    