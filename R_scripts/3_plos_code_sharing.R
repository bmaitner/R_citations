library(tidyverse)
source("api_key.R")

#plos <- read.csv("data/manual_downloads/Comparator-Dataset_v2_Mar23.csv")


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

#   out_df <- readRDS(file = "data/plos_md.RDS")  
#   
# # Merge with plos-provided info
# 
#   plos <-bind_cols(plos,out_df)
#   
#   rm(out_df)
#   
# 
# # Can I pull subject area from the journals?
#   
#   journals <- plos$`prism:publicationName` %>% unique()
#   
#   journals <- data.frame(journal = journals,subj_area  = NA)  
#   
#   areas <- subject_areas()
# 
#   for( i in 1:nrow(journals)){
#     
#     print(paste("journal", i, "of ", nrow(journals)))
#     
#     for(a in areas){
#     
#     if(!is.na(journals$subj_area[i])){next}  
#     
#     journal_i <- journals$journal[i]
#     journal_i <- gsub(pattern = "(",replacement = "",x = journal_i,fixed = TRUE)
#     journal_i <- gsub(pattern = ")",replacement = "",x = journal_i,fixed = TRUE)
#     
#     
#     res <- rscopus::scopus_search(query = paste("SRCTITLE(",journal_i,")AND SUBJAREA(",a,")" ,sep = ""),
#                                   count = 25,max_count = 25)
#     
#     res <- gen_entries_to_df(res$entries)$df
#     
#     if("error" %in% colnames(res)){
#       Sys.sleep(.5)
#       next
#     }
#     
#     journals$subj_area[i] <- a
# 
#     
#     }
#   }  
#   
#   
#   plos %>%
#     left_join(journals,
#               by = c(`prism:publicationName` = 'journal')) -> plos

    # saveRDS(object = plos,
    #         file = "data/plos_data_with_subj_type.RDS ")


  plos <- readRDS("data/plos_data_with_subj_type.RDS")    
    

  # since scopus treats eco and evo as a subset of agriculture, may need to do a search for eco and evo and then do a set diff

  
##########################
  
  #preliminary stats
  
  plos %>%
    mutate(data_shared = case_when(Data_Shared == "Yes" ~ 1,
                                   Data_Shared == "No" ~ 0),
           code_shared = case_when(Code_Shared == "Yes" ~ 1,
                                   Code_Shared == "No" ~ 0,
                                   Code_Shared == "" ~ 0)) %>%
    filter(Publication_Year != "N/A",
           Publication_Year != "2023")%>%
    mutate(Publication_Year = as.numeric(Publication_Year)) %>%
    left_join(x = .,
              y = group_by(.,subj_area) %>%
                summarise(total_pubs_in_area = n())
              ) %>%
    filter(total_pubs_in_area > 500) %>%
    group_by(subj_area, Publication_Year) %>%
    summarise(n_w_data = sum(data_shared),
              n_w_code = sum(code_shared),
              n_total = n(),
              frac_w_data = n_w_data/n_total,
              frac_w_code = n_w_code/n_total) %>%
    ggplot(mapping = aes(x = Publication_Year,
                         y = frac_w_code,
                         color = subj_area))+
    geom_point()+
    geom_line()+
    ylab("Fraction with Code")+
    xlab("Year")+
    theme_bw()+
    labs(color = "Subject Area")+
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) 
    
  
  
  plos %>%
    mutate(data_shared = case_when(Data_Shared == "Yes" ~ 1,
                                   Data_Shared == "No" ~ 0),
           code_shared = case_when(Code_Shared == "Yes" ~ 1,
                                   Code_Shared == "No" ~ 0,
                                   Code_Shared == "" ~ 0)) %>%
    filter(Publication_Year != "N/A",
           Publication_Year != "2023")%>%
    mutate(Publication_Year = as.numeric(Publication_Year)) %>%
    left_join(x = .,
              y = group_by(.,subj_area) %>%
                summarise(total_pubs_in_area = n())
    ) %>%
    filter(total_pubs_in_area > 500) %>%
    group_by(subj_area, Publication_Year) %>%
    summarise(n_w_data = sum(data_shared),
              n_w_code = sum(code_shared),
              n_total = n(),
              frac_w_data = n_w_data/n_total,
              frac_w_code = n_w_code/n_total) %>%
    ggplot(mapping = aes(x = Publication_Year,
                         y = frac_w_data,
                         color = subj_area))+
    geom_point()+
    geom_line()+
    ylab("Fraction with Data")+
    xlab("Year")+
    theme_bw()+
    labs(color = "Subject Area")+
    scale_y_continuous(limits = c(0,1), expand = c(0, 0))
  

    #####################
  
  #stats
  plos %>%
    filter(subj_area == "AGRI") %>%
    mutate(data_shared = case_when(Data_Shared == "Yes" ~ 1,
                                   Data_Shared == "No" ~ 0),
           code_shared = case_when(Code_Shared == "Yes" ~ 1,
                                   Code_Shared == "No" ~ 0,
                                   Code_Shared == "" ~ 0)) %>%
    summarise(n_w_code= sum(code_shared),
              n_w_data = sum(data_shared),
              total = n())%>%
    mutate(pct_w_code = (n_w_code/total)*100,
           pct_w_data = (n_w_data/total)*100)
    
  
  plos$Code_Shared
  
