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

res <- scopus_search(query = "all(ecology) OR all(evolution)
                              AND SUBJAREA(AGRI)
                              AND pubyear > 2010
                              AND LANGUAGE(english)
                    AND REF(R Core Team)
                    AND srctype(j)",count = 20,
                     max_count = 200)


n_records <- res$total_results
df <- gen_entries_to_df(res$entries)
out_df <- df$df


while(nrow(out_df) < n_records){
  
  message(round(nrow(out_df)/n_records,digits = 2)*100, " pct done")
  
  Sys.sleep(25)

  res <- scopus_search(query = "all(ecology) OR all(evolution)
                              AND SUBJAREA(AGRI)
                              AND pubyear > 2010
                              AND LANGUAGE(english)
                    AND REF(R Core Team)
                    AND srctype(j)",count = 20,
                       max_count = 200,
                       start = nrow(out_df)+1)
  
  
  df <- gen_entries_to_df(res$entries)
  
  out_df <- bind_rows(out_df, df$df)

}


Sys.setenv('Elsevier_API' = "a2d541ae2c77641c21a2405f634a22f8")
?Sys.setenv

#a2d541ae2c77641c21a2405f634a22f8






