# citation update 

# To make sure our results aren't driven by lack of time for citations to accrue to younger papers, here we repeat the model using citations from 2024, which gives the younger papers 2 years to accrue publications

library(rscopus)
library(tidyverse)
library(googledrive)
library(googlesheets4)

# source("api_key.R")

# Getting publication data via scopus

# set year thresholds
# min_year <- 2010
# max_year <- 2022

# prep empty output    
# out_df <- NULL

# search by year (broken down by year because of 5k limit on records)

# for(i in min_year:max_year){
#   
#   res <- scopus_search(query = paste("all(ecology OR evolution)
#                                   AND SUBJAREA(AGRI)
#                                   AND pubyear = ", i,
#                                      "AND LANGUAGE(english)
#                                   AND REFTITLE({R: A Language and Environment for Statistical Computing})
#                                   AND srctype(j)"),
#                        count = 25,
#                        max_count = 5000)
#   
#   
#   n_records <- res$total_results
#   
#   if(n_records > 5000){stop("Brian, add a while() loop")}
#   
#   out_df <- bind_rows(out_df,gen_entries_to_df(res$entries)$df)
  
# }#year loop

# clean up

# rm(i, n_records, res, min_year, max_year)

#  save df of articles
# saveRDS(object = out_df,
#         file = "data/search_results_2024_04_23.RDS")

# read in updated _data   

  new_cite_counts <- readRDS("data/search_results_2024_04_23.RDS")

  # rename fields for consistency:
  
  new_cite_counts <- 
    new_cite_counts %>%
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
  
  
  
# read in OG data
  
  cite_data <- readRDS("data/cite_data.RDS") %>% ungroup()
  
# merge cite data
  
  new_cite_counts %>%
    select(title,citations) %>%
    rename(citations_by_title = citations)%>%
    right_join(cite_data) -> cite_data
  
  new_cite_counts %>%
    select(doi,citations) %>%
    rename(citations_by_doi = citations)%>%
    filter(!is.na(doi))%>%
    right_join(cite_data) -> cite_data
  
  
  cite_data %>%
    mutate(cites_2024 = case_when(!is.na(citations_by_title) ~ citations_by_title,
                                  is.na(citations_by_title) ~ citations_by_doi)) -> cite_data

  
  # For remaining papers, add by doi search

    cite_data %>%
    filter(is.na(cites_2024))%>%
      pull(doi)-> needed_dois
  
  out_df <- NULL  
    for(i in needed_dois){

      res <- scopus_search(query = paste("all(",i,")"),
                           count = 25,
                           max_count = 5000)


      n_records <- res$total_results

      if(n_records > 5000){stop("Brian, add a while() loop")}

      out_df <- bind_rows(out_df,gen_entries_to_df(res$entries)$df)
    
     }#doi loop
    
    # clean up
    
   rm(i, n_records, res, min_year, max_year)
   
   out_df %>%
     dplyr::filter(`prism:doi` %in% needed_dois)%>%
     rename(doi = `prism:doi`,citations = `citedby-count`)%>%
   select(doi,citations) %>%
     rename(citations_by_doi2 = citations)%>%
     filter(!is.na(doi))%>%
     right_join(cite_data) -> cite_data
   
   cite_data %>%
     mutate(cites_2024 = case_when(!is.na(cites_2024) ~ cites_2024,
                                   is.na(cites_2024) ~ citations_by_doi2)) -> cite_data

   
  cite_data %>%
    select(-citations_by_doi2,
           -citations_by_doi,
           -citations_by_title) -> cite_data

  saveRDS(object = cite_data,file = "data/cite_data_w_2024.RDS")
  
  
  ########################################################
  
  
  cite_data <- readRDS("data/cite_data_w_2024.RDS") %>%
    ungroup()

  
  # add impact factor
  
  
  impact_factors<- read.csv("data/manual_downloads/impact_factors.csv")
  
  cite_data %>%
    left_join(y = impact_factors,
              by = c("journal" = "needed_journals")) -> cite_data
  
  rm(impact_factors)

  
  
  # Data formatting 
  
  cite_data %>%
    mutate(citations = as.numeric(citations),
           cites_2024 = as.numeric(cites_2024)) -> cite_data
  
  cite_data %>%
    mutate(age = 2024-year) -> cite_data
  
  cite_data$age_scaled <- scale(cite_data$age)
  cite_data$ImpactFactor_scaled <- scale(cite_data$ImpactFactor)
  
  age_scaling = data.frame(scale =    attr(cite_data$age_scaled,"scaled:scale"),
                           center=   attr(cite_data$age_scaled,"scaled:center"))
  
  if_scaling = data.frame(scale =    attr(cite_data$ImpactFactor_scaled,"scaled:scale"),
                          center=   attr(cite_data$ImpactFactor_scaled,"scaled:center"))
  
  cite_data$age_scaled <- as.numeric(cite_data$age_scaled)
  cite_data$ImpactFactor_scaled <- as.numeric(cite_data$ImpactFactor_scaled)
  

# Models
  
  # models
  m0 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  1 ) # Null model, intercept only
  
  m1 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  r_scripts_available +
              data_available + 
              open_access +
              age_scaled + 
              ImpactFactor_scaled) # main effects only
  
  m2 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  age_scaled ) # Citations increase over time
  
  
  m3 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled ) # Citations increase due to impact factor and time
  
  
  m4 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled ) # citations increase due to impact factor and R scripts
  
  
  m5 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled ) # citations increase due to impact, R, and open access
  
  m6 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled +
              data_available*age_scaled) # citations increase due to impact, R, open access and open data over time
  
  m7 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled +
              data_available*age_scaled +
              data_available*r_scripts_available+
              r_scripts_available*open_access+
              open_access*data_available) #everything increases with age, plus interactions b/t open access components
  
  
  m8 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled +
              open_access*r_scripts_available) # rscripts and age matter
  
  m9 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled +
              open_access*age_scaled +
              open_access*r_scripts_available ) # open access matters
  
  m10 <- glm(data = cite_data,
             family = "poisson",
             formula = cites_2024 ~ 
               ImpactFactor_scaled*age_scaled +
               r_scripts_available*age_scaled +
               data_available*age_scaled +
               data_available*r_scripts_available ) # open access matters
  
  AICtab(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
  
  summarym7 <- summary(m7)
  summarym7
  pref_model <- m7  
  
  
#########################################
  
  # switching to age in days
  
  cite_data %>%
    mutate(age_continuous = Sys.Date()-as_date(date))%>%
    mutate(as.numeric(age_continuous))->cite_data
  
  cite_data$age_continuous_scaled <- scale(cite_data$age_continuous) %>% as.numeric()
  # models
  
  m0 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  1 ) # Null model, intercept only
  
  m1 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  r_scripts_available +
              data_available + 
              open_access +
              age_continuous_scaled + 
              ImpactFactor_scaled) # main effects only
  
  m2 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~  age_continuous_scaled ) # Citations increase over time
  
  
  m3 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled ) # Citations increase due to impact factor and time
  
  
  m4 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled ) # citations increase due to impact factor and R scripts
  
  
  m5 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled+
              open_access*age_continuous_scaled ) # citations increase due to impact, R, and open access
  
  m6 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled+
              open_access*age_continuous_scaled +
              data_available*age_continuous_scaled) # citations increase due to impact, R, open access and open data over time
  
  m7 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled+
              open_access*age_continuous_scaled +
              data_available*age_continuous_scaled +
              data_available*r_scripts_available+
              r_scripts_available*open_access+
              open_access*data_available) #everything increases with age, plus interactions b/t open access components
  
  
  m8 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled +
              open_access*r_scripts_available) # rscripts and age matter
  
  m9 <- glm(data = cite_data,
            family = "poisson",
            formula = cites_2024 ~ 
              ImpactFactor_scaled*age_continuous_scaled +
              r_scripts_available*age_continuous_scaled +
              open_access*age_continuous_scaled +
              open_access*r_scripts_available ) # open access matters
  
  m10 <- glm(data = cite_data,
             family = "poisson",
             formula = cites_2024 ~ 
               ImpactFactor_scaled*age_continuous_scaled +
               r_scripts_available*age_continuous_scaled +
               data_available*age_continuous_scaled +
               data_available*r_scripts_available ) # open access matters
  
  AICtab(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

  summary(m7)  

    

  