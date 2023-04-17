library(tidyverse)
source("api_key.R")

plos <- read.csv("data/manual_downloads/Comparator-Dataset_v2_Mar23.csv")

min(plos$Publication_Year)
unique(plos$Publication_Year)

# what we need is an Rscopus search that links these URLs to their subject area



res <- scopus_search(query = paste("all(ecology OR evolution)
                                  AND SUBJAREA(AGRI)
                                  AND pubyear = ", i,
                                   "AND LANGUAGE(english)
                                  AND REFTITLE({R: A Language and Environment for Statistical Computing})
                                  AND srctype(j)"),
                     count = 25,
                     max_count = 5000)

#Get some basic info from scopus

  # out_df <- NULL
  # 
  # for(i in 1:nrow(plos)){
  #   
  #   print(i)
  #   
  #   res <- scopus_search(query = paste('DOI(',plos$DOI[i],')',sep = ""),
  #                        count = 25,
  #                        max_count = 5000)
  #   
  #   res <- gen_entries_to_df(res$entries)$df
  #   
  #   if(nrow(res) != 1){
  #     
  #     res <- res[which.max(res$`citedby-count`),]
  # 
  #   }
  #   
  #   if(nrow(res) != 1){
  #     
  #     stop()
  #     
  #   }
  #   
  #   out_df <- bind_rows(out_df,res)
  #   
  #   
  # }
  # 
  # saveRDS(object = out_df,file = "data/plos_md.RDS")  

  out_df <- readRDS(file = "data/plos_md.RDS")  
  
# Merge with plos-provided info

  plos <-bind_cols(plos,out_df)
  
  rm(out_df)
  

# Can I pull subject area from the journals?
  
  journals <- plos$`prism:publicationName` %>% unique()
  
  journals <- data.frame(journal = journals,subj_area  = NA)  
  
  areas <- subject_areas()

  for( i in 1:nrow(journals)){
    
    print(paste("journal", i, "of ", nrow(journals)))
    
    for(a in areas){
    
    if(!is.na(journals$subj_area[i])){next}  
    
    journal_i <- journals$journal[i]
    journal_i <- gsub(pattern = "(",replacement = "",x = journal_i,fixed = TRUE)
    journal_i <- gsub(pattern = ")",replacement = "",x = journal_i,fixed = TRUE)
    
    
    res <- rscopus::scopus_search(query = paste("SRCTITLE(",journal_i,")AND SUBJAREA(",a,")" ,sep = ""),
                                  count = 25,max_count = 25)
    
    res <- gen_entries_to_df(res$entries)$df
    
    if("error" %in% colnames(res)){
      Sys.sleep(.5)
      next
    }
    
    journals$subj_area[i] <- a

    
    }
  }  
  
  
  plos %>%
    left_join(journals,
              by = c(`prism:publicationName` = 'journal')) -> plos

    saveRDS(object = plos,
            file = "data/plos_data_with_subj_type.RDS ")
    
    

  # since scopus treats eco and evo as a subset of agriculture, may need to do a search for eco and evo and then do a set diff

  
  subject_area_codes()
  ?subject_areas
  
  
##########################
  
  #preliminary stats
  
  plos %>%
    mutate(data_shared = case_when(Data_Shared == "Yes" ~ 1,
                                   Data_Shared == "No" ~ 0),
           code_shared = case_when(Code_Shared == "Yes" ~ 1,
                                  Code_Shared == "No" ~ 0)) %>%
    mutate(Publication_Year = as.numeric(Publication_Year)) %>%
  group_by(subj_area, Publication_Year) %>%
    summarise(n_w_data = sum(data_shared),
              n_w_code = sum(code_shared),
              n_total = n(),
              frac_w_data = n_w_data/n_total,
              frac_w_code = n_w_code/n_total) %>%
    filter(n_total > 80) %>%
    ggplot(mapping = aes(x = Publication_Year,
                         y = frac_w_code,
                         color = subj_area))+
    geom_point()+
    geom_line()+
    ylab("Fraction with Code")+
    xlab("Year")
    
  
  
  plos %>%
    mutate(data_shared = case_when(Data_Shared == "Yes" ~ 1,
                                   Data_Shared == "No" ~ 0),
           code_shared = case_when(Code_Shared == "Yes" ~ 1,
                                   Code_Shared == "No" ~ 0)) %>%
    mutate(Publication_Year = as.numeric(Publication_Year)) %>%
    group_by(subj_area, Publication_Year) %>%
    summarise(n_w_data = sum(data_shared),
              n_w_code = sum(code_shared),
              n_total = n(),
              frac_w_data = n_w_data/n_total,
              frac_w_code = n_w_code/n_total) %>%
    filter(n_total > 80) %>%
    ggplot(mapping = aes(x = Publication_Year,
                         y = frac_w_data,
                         color = subj_area))+
    geom_point()+
    geom_line()+
    ylab("Fraction with Data")+
    xlab("Year")
  
    
  
    

    
  