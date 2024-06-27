# Checking for code citations

library(tidyverse)
library(rscopus)
source("api_key.R")


  cite_data <- readRDS("data/cite_data.RDS") %>%
    ungroup()

cite_data %>%
  filter(r_scripts_available == "yes")-> papers_w_code

papers_w_code %>%
  select(`code location`) %>%
  table()

# 22/55 (40%) had code in the SI or appendix, which couldn't be differentiated from the paper itself.

  # write.csv(x = papers_w_code %>%
  #             select(uid,doi,doi_url,comments,`code location`)%>%
  #             arrange(`code location`),
  #           file = "data/code_urls_to_search.csv",
  #           row.names = FALSE)

# Manually curate the dois and urls, which are mostly in the comments (but a few need to be found)

  # Load in the curated urls

    paper_locations <- read.csv("data/code_urls_to_search.csv")
  
  # Now, use search for citations of that code
  
    paper_locations$n_code_citations <- 0
  
  for(i in 1:nrow(paper_locations)){

    if(paper_locations$code_url[i] == ""){next}
    
      focal_paper <- papers_w_code %>%
        filter(uid ==     paper_locations$uid[i])
    
    res <- scopus_search(query = paste(paper_locations$code_url[i]),
                         count = 25,
                         max_count = 5000)
    
    res2 <- scopus_search(query = paste("REF({",paper_locations$code_url[i],"})"),
                         count = 25,
                         max_count = 5000)
    
    
    
    if(res$total_results == 0 &
       res2$total_results == 0){next}
    

    res_df <- gen_entries_to_df(res$entries)$df
    res2_df <- gen_entries_to_df(res2$entries)$df

    if(nrow(res_df) > 1){stop()}
    if(nrow(res2_df) > 1){stop()}

    doi_i <- res_df$`prism:doi`
    
    if(doi_i == focal_paper$doi){
      message("self cite")
      }else(stop("write code"))
    
    doi2_i <- res2_df$`prism:doi`
    
    if(doi2_i == focal_paper$doi){
      message("self cite")
    }else(stop("write code"))
    
    
    

  }    

# Shockingly, none of the code shows a citation. Let's try using the dois alone where appropriate
    
  paper_locations <-  
  paper_locations %>%
    mutate(code_doi_sans_url = case_when(grepl(x = code_url,pattern = "doi.org") ~  gsub(pattern = "https://doi.org/",replacement = "",x=code_url)))

  
  for(i in 1:nrow(paper_locations)){
    
    if(is.na(paper_locations$code_doi_sans_url[i])){next}
    
    focal_paper <- papers_w_code %>%
      filter(uid ==  paper_locations$uid[i])
    
    res <- scopus_search(query = paste(paper_locations$code_doi_sans_url[i]),
                         count = 25,
                         max_count = 5000)
    
    if(res$total_results == 0){next}
    
    
    res_df <- gen_entries_to_df(res$entries)$df
    
    if(nrow(res_df)>1){stop()}
    
    doi_i <- res_df$`prism:doi`
    
    if(doi_i == focal_paper$doi){
      message("self cite")
      next
    }else(stop("write code"))
    
  }    

  
  # Still nothing, however, other URLs have hits, suggesting it is a real result and not simply an error in the searching
  