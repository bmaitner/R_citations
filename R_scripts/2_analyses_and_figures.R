#' @author Brian Maitner

# In this script, the goal is to extract information from the manually-populated Google sheet

# Data to be gathered includes:

# 1) Number of papers with vs without scripts available over time
# 2) Additional metadata about the script format 

################################################################################

# Install non-CRAN libraries if needed
  #remotes::install_github('jkeirstead/scholar')

# Load libraries
  
  # Data wrangling

    library(tidyverse)
    library(googledrive)
    library(googlesheets4)
    library(stringdist)

  # Analyses
    library(bbmle)
    library(scholar)
    library(stats) #not needed to call, but here for attribution purposes

  # Plotting
    library(ggpmisc)
    library(ggplot2)

# Pull the current version of the google sheet

  #cite_data <- read_sheet("1pYB_oJt-Sx__OKJdmlgEpBmDFLmGLxh9Rddp9qKNooE")
  
# Limit the data to the comprehensive sampling at the beginning (to avoid any biases due to more freely-available journals)
# Also omitting papers that cite but don't use R (n=3)
# Dividing the sample size of 1001 evenly across years (77 per year)

  # cite_data %>%
  #   filter(!is.na(r_scripts_available),
  #          !is.na(data_available),
  #          r_used == "yes") %>%
  #   group_by(year) %>%
  #   slice_head(n=77) -> cite_data

    
# Remove unnecessary fields

    # cite_data %>%
    #   select(-folder) %>%
    #   select(-folder_link) %>%
    #   select(-recorded_by) -> cite_data

# Save the data so I don't have to re-download it
  
  # saveRDS(object = cite_data,file = "data/cite_data.RDS")
  cite_data <- readRDS("data/cite_data.RDS") %>% ungroup()

  
######################################################################################
  ##########################################################
  
  # We need to get impact factors for the journals so we can account for that in our models
  
  # needed_journals <- unique(cite_data$journal)
  # impact_factors <- scholar::get_impactfactor(journals = needed_journals,
  #                                             max.distance = 0.01)
  # impact_factors <- cbind(needed_journals,impact_factors)
  
  #the scholar package is currently returning some questionable matches well above the max.distance.
  #I'll toss these out.
  
  # impact_factors$dist <- stringdist(a = tolower(impact_factors$needed_journals),b = tolower(impact_factors$Journal))
  
  #write as a csv to manually update the missing or incorrect data
  
  # write.csv(x = impact_factors,
  #           file = "data/manual_downloads/impact_factors.csv",row.names = FALSE)
  
  # Per the file history, the impact factor was downloaded on June 16, 2023
  
    # Need to update one journal's impact factor on April 1, 2024

  #impact_factors <- read.csv(file = "data/manual_downloads/impact_factors.csv",sep = ";")
  
  # needed_journals_update <- cite_data$journal[which(!cite_data$journal %in% impact_factors$needed_journals)]
  
    # impact factor is no longer provided by scholar, so for this single journal I'll grab it manually
  
  # botany_impact_factor <- data.frame(needed_journals = "Botany",
  #                                    Journal = "Botany",
  #                                    Cites = NA,
  #                                    ImpactFactor = 1.1,
  #                                    Eigenfactor = 0.00102,
  #                                    dist = 0 )
  # 
  # impact_factors <- bind_rows(impact_factors,botany_impact_factor)
  
  # write.csv(x = impact_factors,
  #           file = "data/manual_downloads/impact_factors.csv",
  #           row.names = FALSE)

  impact_factors<- read.csv("data/manual_downloads/impact_factors.csv")

  cite_data %>%
    left_join(y = impact_factors,
              by = c("journal" = "needed_journals")) -> cite_data
  
  rm(impact_factors)
  ############################################################################
  
  # Data formatting 
  
  cite_data %>%
    mutate(citations = as.numeric(citations)) -> cite_data
  
  cite_data %>%
    mutate(age = 2023-year) -> cite_data
  
  cite_data$age_scaled <- scale(cite_data$age)
  cite_data$ImpactFactor_scaled <- scale(cite_data$ImpactFactor)
  
  age_scaling = data.frame(scale =    attr(cite_data$age_scaled,"scaled:scale"),
                           center=   attr(cite_data$age_scaled,"scaled:center"))
  
  if_scaling = data.frame(scale =    attr(cite_data$ImpactFactor_scaled,"scaled:scale"),
                          center=   attr(cite_data$ImpactFactor_scaled,"scaled:center"))
  
  cite_data$age_scaled <- as.numeric(cite_data$age_scaled)
  cite_data$ImpactFactor_scaled <- as.numeric(cite_data$ImpactFactor_scaled)
  
######################################################################################  

# Figure 1

  # Papers with code as a function of the year, model as proportion data
  
    cite_data %>%
      mutate(r_scripts_available_binary = case_when(r_scripts_available == "yes" ~ 1,
                                                    r_scripts_available == "no" ~ 0))%>%
      mutate(year = year-2010) %>%
      glm(formula = r_scripts_available_binary ~ year,
          family = "binomial") -> cite_v_year_binary
  
  summary(cite_v_year_binary)
  
  # Check for temporal autocorrelation
  library(DHARMa)  
  
  simulateResiduals(cite_v_year_binary)%>%
    recalculateResiduals(group = cite_data$age_scaled)%>%
    testTemporalAutocorrelation(time = unique(cite_data$age_scaled))  
  
  
  # Get probabilities over time
    new_binary <- data.frame(year = 0:12,
                             r_scripts_available_binary = 1)
    
    binary_logit_preds <- predict(object = cite_v_year_binary,
            newdata = new_binary)
    
    predict.glm(object = cite_v_year_binary,
            newdata = new_binary,type = "response")
  
    prob.predictions <- 1 / (1 + exp(-binary_logit_preds))
    
    # Get probability increase over time
    
    lm(prob.predictions~new_binary$year) # probability of including code increases at a rate of ~ 0.006 per year
  
  # Papers with data as a function of the year, model as proportion data
  
    cite_data %>%
      mutate(data_available_binary = case_when(data_available == "yes" ~ 1,
                                                    data_available == "no" ~ 0))%>%
      mutate(year = year-2010) %>%
      glm(formula = data_available_binary ~ year,
          family = "binomial") -> data_available_v_year_binary
  
    
  summary(data_available_v_year_binary)
  

  # Papers with open access as a function of the year, model as proportion data
  
  cite_data %>%
    mutate(open_access = as.numeric(open_access))%>%
    mutate(year = year-2010) %>%
    glm(formula = open_access ~ year,
        family = "binomial") -> open_access_v_year_binary
  
  
  summary(open_access_v_year_binary)
  
    # model predictions for plotting
  
      new_years <- data.frame(year = 0:12)
      
      #
      code_preds <- predict(object = cite_v_year_binary,
                                    newdata = new_years)
      
      code_prob.predictions <- 1 / (1 + exp(-code_preds))
      
      #
      data_preds <- predict(object = data_available_v_year_binary,
                            newdata = new_years)
      
      data_prob.predictions <- 1 / (1 + exp(-data_preds))
      
      #
      open_preds <- predict(object = open_access_v_year_binary,
                            newdata = new_years)
      
      open_prob.predictions <- 1 / (1 + exp(-open_preds))
      
            
      # combine into one data.frame
      
      open_science_preds <- 
      bind_rows(data.frame(year = new_years,
                 value = code_prob.predictions,
                 Availability = 'Open Code'),
      data.frame(year = new_years,
                 value = data_prob.predictions,
                 Availability = 'Open Data'),
      data.frame(year = new_years,
                 value = open_prob.predictions,
                 Availability = 'Open Publication')) %>%
        mutate( year = year+2010)%>%
        mutate(sig = case_when(Availability %in% c("Open Publication") ~ 2,
                               Availability %in% c("Open Data", "Open Code") ~ 1))%>%
        mutate(value = value*100) %>%
        mutate(sig = as.character(sig))
      
      open_science_preds$Availability <- factor(x = open_science_preds$Availability,
                                                levels = c("Open Publication", "Open Data", "Open Code"))
      
      
        
  #Figure 1
  
  cite_data %>%
    group_by(year) %>%
    summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
              n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
              prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available),
              pct_scripts_available = prop_scripts_available *100,
              
              n_open_access_pub = sum(open_access=="1"),
              n_closed_access_pub = sum(open_access=="0"),
              prop_open_access_pub = n_open_access_pub / (n_open_access_pub + n_closed_access_pub),
              pct_open_access_pub = prop_open_access_pub *100,
              
              n_data_available = sum(na.omit(data_available=="yes")),
              n_data_not_available = sum(na.omit(data_available=="no")),
              prop_data_available = n_data_available / (n_data_available + n_data_not_available),
              pct_data_available = prop_data_available *100
    ) %>%
    ungroup() %>%
    mutate(year=as.integer(year)) %>%
    
    select(year,
           pct_scripts_available,
           pct_open_access_pub,
           pct_data_available) %>%
    rename('Open Code' = pct_scripts_available,
           'Open Publication' = pct_open_access_pub,
           'Open Data' = pct_data_available) %>%
    pivot_longer(cols = 2:4,names_to = "Availability") -> temp
  
  # order availability to match lines: pub, then data, then code
  
  temp$Availability <- factor(x = temp$Availability,levels = c("Open Publication", "Open Data", "Open Code"))
  


    temp %>%
    ggplot(aes(x=year,
               y=value,
               color = Availability))+
    geom_point(size=3)+
    geom_line(data = open_science_preds,
              mapping = aes(x=year,y=value,color=Availability,
                            lty=sig),
              linewidth=3)+
    ylab("Availability")+
    xlab("Year")+
    xlab("\nYear")+
    scale_x_continuous(limits = c(2009.7, 2022.3),
                       breaks = seq(2010, 2022, 1),
                       expand = c(0,0),
                       minor_breaks = NULL)+
    scale_y_continuous(limits=c(-3,41),
                       breaks = seq(0,40,5),
                       expand = c(0,1),
                       minor_breaks = seq(0,40,1),
                       labels = c("0%","5%","10%","15%","20%","25%","30%","35%","40%"))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9,size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(angle = 90, vjust=3,size=20),
          axis.title.x = element_text(size=20),
          title = element_text(size=20),
          legend.title=element_blank(),
          legend.text = element_text(size=20))+
      guides(linetype="none")->fig1_rev
  
  ggsave(plot = fig1_rev, filename = "figures/figure1.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig1_rev, filename = "figures/figure1.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
#############        
  
# Total number of papers we've found scripts for

  cite_data %>%
    summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")))

# Total number of papers we've evaluated
  
  cite_data %>%
    summarise(n_papers_evaluated = sum(na.omit(r_scripts_available %in% c("yes","no"))))

# Overall fraction of papers with scripts
  
  cite_data %>%
    summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")))/
    cite_data %>%
    summarise(n_papers_evaluated = sum(na.omit(r_scripts_available %in% c("yes","no"))))
  
# Where is the code being stored?
  
  cite_data %>%
    filter(r_scripts_available == "yes")%>%
    select(`code location`,`code format`) -> code_storage
  
  # Let's lump the appendix with the CI
  code_storage <-
    code_storage %>%
    mutate(`code location` = case_when(`code location`=="appendix" ~ "SI",
                                       `code location`!="appendix" ~ `code location`))
  
  # Let's lump the rmd with R
  code_storage <-
    code_storage %>%
    mutate(`code format` = case_when(`code format`=="rmd" ~ "R",
                                     `code format`!="rmd" ~ `code format`))
  
  code_storage %>%
    group_by(`code location`)%>%
    summarise(counts= n())%>%
    mutate(percent = counts / sum(counts)*100)%>%
    arrange(-percent)
  
  code_storage %>%
    group_by(`code format`)%>%
    summarise(counts= n())%>%
    mutate(percent = counts / sum(counts)*100)
  
# Licenses  
  #Proportion of papers with an explicit license of some sort
  
  cite_data %>%
    filter(r_scripts_available == "yes") %>%
    pull(`code license`) %>%
    table()
    
  cite_data %>%
    filter(r_scripts_available == "yes") %>%
    pull(`code license`) %>%
    table()/55*100
  
  
  
##################################

  #Open access vs code sharing?
  
  # Testing whether open vs closed access differ in likelihood of code inclusion
  
  set.seed(2005) #because Transformers: The Movie is set in that year.
  chisq.test(x = as.factor(cite_data$open_access),
             y = as.factor(cite_data$r_scripts_available),
             simulate.p.value = TRUE,B = 10000)
  
  # what proportion of OA papers have data?
  
  cite_data %>%
    filter(cite_data$open_access == "1") %>%
    group_by(r_scripts_available) %>%
    summarise(n = n())%>%
    mutate(prop = n/ sum(n))
  
  cite_data %>%
    filter(cite_data$open_access == "0") %>%
    group_by(r_scripts_available) %>%
    summarise(n = n())%>%
    mutate(prop = n/ sum(n))
  
  8.5/4.24
    
  #open data vs code sharing?
  
  set.seed(2005) #because Transformers: The Movie is set in that year.
  chisq.test(x = as.factor(cite_data$data_available),
             y = as.factor(cite_data$r_scripts_available),
             simulate.p.value = TRUE,B = 10000)
  
  
  cite_data %>%
    filter(cite_data$data_available == "yes") %>%
    group_by(r_scripts_available) %>%
    summarise(n = n())%>%
    mutate(prop = n/ sum(n))
  
  cite_data %>%
    filter(cite_data$data_available == "no") %>%
    group_by(r_scripts_available) %>%
    summarise(n = n())%>%
    mutate(prop = n/ sum(n))
  26.5/2.2
  
  # Testing whether shared code is disproportionately in high-impact journals
  
  aov(data = cite_data,
      formula = ImpactFactor_scaled ~ r_scripts_available ) -> aov_res
  summary(aov_res)
  
  cite_data %>%
    group_by(r_scripts_available) %>%
    summarise(mean_if = mean(ImpactFactor))

###############
  
# Samples per year

  cite_data %>%
    dplyr::filter(r_scripts_available %in% c("yes","no"))%>%
    group_by(year)%>%
    summarise(n = n()) %>%
    ggplot(mapping = aes(x = year, y = n))+
    geom_line()+
    scale_x_continuous(limits = c(2010, 2022),
                       breaks = seq(2010, 2022, 1),minor_breaks = NULL)+
    scale_y_continuous(limits=c(0,100),breaks = seq(0,100,10))
  
##########################################################  

  # variable ranges
    min(cite_data$ImpactFactor)
    max(cite_data$ImpactFactor)
    
    min(cite_data$age)
    max(cite_data$age)
  
##########################################################
  
  
  ######################
  
  # Modeling citation rate
    # Thanks to reviewer 2 for suggesting the poisson model

  # models
  
  m0 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~  1 ) # Null model, intercept only
  
  m1 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~  r_scripts_available +
              data_available + 
              open_access +
              age_scaled + 
              ImpactFactor_scaled) # main effects only
  
  m2 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~  age_scaled ) # Citations increase over time
  
  
  m3 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled ) # Citations increase due to impact factor and time
  
  
  m4 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled ) # citations increase due to impact factor and R scripts
  
  
  m5 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled ) # citations increase due to impact, R, and open access
  
  m6 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled +
              data_available*age_scaled) # citations increase due to impact, R, open access and open data over time
  
  m7 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled+
              open_access*age_scaled +
              data_available*age_scaled +
              data_available*r_scripts_available+
              r_scripts_available*open_access+
              open_access*data_available) #everything increases with age, plus interactions b/t open access components
  
  
  m8 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled +
              open_access*r_scripts_available) # rscripts and age matter
  
  m9 <- glm(data = cite_data,
            family = "poisson",
            formula = citations ~ 
              ImpactFactor_scaled*age_scaled +
              r_scripts_available*age_scaled +
              open_access*age_scaled +
              open_access*r_scripts_available ) # open access matters
  
  m10 <- glm(data = cite_data,
             family = "poisson",
             formula = citations ~ 
               ImpactFactor_scaled*age_scaled +
               r_scripts_available*age_scaled +
               data_available*age_scaled +
               data_available*r_scripts_available ) # open access matters
  
  AICtab(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
  
  summarym7 <- summary(m7)
  summarym7
  pref_model <- m7  

  write.csv(x = summarym7$coefficients%>%round(6),
            file = "figures/model_coefficients.csv",
            row.names = TRUE)
  
  sink("figures/summarym7.csv")
  print(summary(m7))
  sink()  # returns output to the console


  #library(rsq)
  library(MuMIn)
  MuMIn::r.squaredGLMM(m7) #0.93

################################################

  # Predicted data for plotting
  
  fully_open <- data.frame(r_scripts_available ="yes",
                           data_available = "yes",
                           age_scaled = unique(cite_data$age_scaled),
                           open_access="1",
                           ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_code <- data.frame(r_scripts_available ="yes",
                          data_available = "no",
                          age_scaled = unique(cite_data$age_scaled),
                          open_access="0",
                          ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_pub <- data.frame(r_scripts_available ="no",
                         data_available = "no",
                         age_scaled = unique(cite_data$age_scaled),
                         open_access="1",
                         ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_data <- data.frame(r_scripts_available ="no",
                          data_available = "yes",
                         age_scaled = unique(cite_data$age_scaled),
                         open_access="0",
                         ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  fully_closed <- data.frame(r_scripts_available ="no",
                             data_available = "no",
                             age_scaled = unique(cite_data$age_scaled),
                             open_access="0",
                             ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_code_and_data <- data.frame(r_scripts_available ="yes",
                             data_available = "yes",
                             age_scaled = unique(cite_data$age_scaled),
                             open_access="0",
                             ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_code_and_publication <- data.frame(r_scripts_available ="yes",
                                   data_available = "no",
                                   age_scaled = unique(cite_data$age_scaled),
                                   open_access="1",
                                   ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_data_and_publication <- data.frame(r_scripts_available ="no",
                                          data_available = "yes",
                                          age_scaled = unique(cite_data$age_scaled),
                                          open_access="1",
                                          ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  
  
  
  
  
  predicted_data <-
    bind_rows(  
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open$age_scaled,
                 Access = "Fully open",
                 citations = predict(object = pref_model,
                                     newdata = fully_open,
                                     se.fit = TRUE,
                                     type="response")), #NOTE: This type=response is VERY IMPORTANT!
       
      data.frame(year=2010:2022,
                 age_scaled = fully_closed$age_scaled,
                 Access = "Fully closed",
                 citations = predict(object = pref_model,
                                     newdata = fully_closed,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_code$age_scaled,
                 Access = "Open code",
                 citations = predict(object = pref_model,
                                     newdata = open_code,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_pub$age_scaled,
                 Access = "Open publication",
                 citations = predict(object = pref_model,
                                     newdata = open_pub,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_data$age_scaled,
                 Access = "Open data",
                 citations = predict(object = pref_model,
                                     newdata = open_pub,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_data$age_scaled,
                 Access = "Open code and data",
                 citations = predict(object = pref_model,
                                     newdata = open_code_and_data,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_data$age_scaled,
                 Access = "Open code and publication",
                 citations = predict(object = pref_model,
                                     newdata = open_code_and_publication,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_data$age_scaled,
                 Access = "Open data and publication",
                 citations = predict(object = pref_model,
                                     newdata = open_data_and_publication,
                                     se.fit = TRUE,
                                     type="response"))

    )
  
  
  predicted_data$age <- predicted_data$age_scaled*age_scaling$scale + age_scaling$center
  
  predicted_data %>%
    mutate(citations = citations.fit,
           citations_se = citations.se.fit)%>%
    select(-citations.residual.scale) -> predicted_data
  
  

  predicted_data %>%
  ggplot(mapping = aes(x=`age`,
                         y=citations,
                         color=Access
                       ))+
    geom_line(size=1.5)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(#limits = c(0.2,170),
                       #breaks = seq(0,100, 10),
                       minor_breaks = seq(0,800, 10),
                       expand = c(0, 0)
                       ,trans = "log10"
                       )+
    ylab("\nCumulative Citations")+
    xlab("\nYears Since Publication")+
    scale_color_manual(values = 
                         c('Fully open' = "#ff6db6",
                                    'Open code and data' = "#24ff24",
                                    'Open code and publication' = "#490092",
                                    'Open data and publication' = "#006ddb",
                                    'Open data' =  "#ffff6d",
                                    'Open publication' = "#b66dff",
                                    'Fully closed' = "#924900",
                                    'Open code' = "#009292"),
                       breaks=c('Fully open',
                                         "Open code and data",
                                         "Open code and publication",
                                         "Open data and publication",
                                         "Open data",
                                         'Open publication',
                                         'Fully closed',
                                         'Open code'))+
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(vjust=3,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) -> fig2
  fig2
  
  
  ggsave(plot = fig2, filename = "figures/figure2.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig2, filename = "figures/figure2.jpg",
         width = 10,height = 5,units = "in",dpi = 600)

# When does open science double the amount of closed?
  
  predicted_data %>%
    select(Access, citations, age) %>%
    pivot_wider(names_from = Access,values_from = citations)->pred_wide
  
  pred_wide %>%
    dplyr::filter(`Fully open` > (`Fully closed`*2)) #3 years
  
  pred_wide %>%
    dplyr::filter(`Open code and data` > (`Fully closed`*2)) #11 years
  
  pred_wide %>%
    dplyr::filter(`Open code and publication` > (`Fully closed`*2)) #11 years
  


#############  
  
  # Predicted data for plotting
  
  quantile(cite_data$ImpactFactor,probs = 0.1) # ~1.3 is the low IF (~ INTERNATIONAL JOURNAL OF PRIMATOLOGY, NEW ZEALAND JOURNAL OF ECOLOGY)
  quantile(cite_data$ImpactFactor,probs = 0.9) # ~4.7 is the low IF (~ Ecology, Biological Conservation)
  
  
  
  fully_open_low_if <- data.frame(r_scripts_available ="yes",
                           age_scaled = unique(cite_data$age_scaled),
                           open_access="1",
                           data_available = "yes",
                           ImpactFactor_scaled = as.numeric(quantile(cite_data$ImpactFactor_scaled,probs = 0.1)))
  

  fully_open_high_if <- data.frame(r_scripts_available ="yes",
                                  age_scaled = unique(cite_data$age_scaled),
                                  open_access="1",
                                  data_available = "yes",
                                  ImpactFactor_scaled = as.numeric(quantile(cite_data$ImpactFactor_scaled,probs = 0.9)))
  
  open_code_low_if <- data.frame(r_scripts_available ="yes",
                          age_scaled = unique(cite_data$age_scaled),
                          open_access="0",
                          data_available = "no",
                          ImpactFactor_scaled =quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  open_pub_low_if <- data.frame(r_scripts_available ="no",
                         age_scaled = unique(cite_data$age_scaled),
                         open_access="1",
                         data_available = "no",
                         ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  open_pub_and_data_low_if <- data.frame(r_scripts_available ="no",
                                age_scaled = unique(cite_data$age_scaled),
                                open_access="1",
                                data_available = "yes",
                                ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  open_pub_and_code_low_if <- data.frame(r_scripts_available ="yes",
                                         age_scaled = unique(cite_data$age_scaled),
                                         open_access="1",
                                         data_available = "no",
                                         ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  open_data_and_code_low_if <- data.frame(r_scripts_available ="yes",
                                         age_scaled = unique(cite_data$age_scaled),
                                         open_access="0",
                                         data_available = "yes",
                                         ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  fully_closed_high_if <- data.frame(r_scripts_available ="no",
                             age_scaled = unique(cite_data$age_scaled),
                             open_access="0",
                             data_available = "no",
                             ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.9)|>as.numeric())
  
  fully_closed_low_if <- data.frame(r_scripts_available ="no",
                                     age_scaled = unique(cite_data$age_scaled),
                                     open_access="0",
                                    data_available = "no",
                                    ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  predicted_data_if <-
    bind_rows(  
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_low_if$age_scaled,
                 Access = "Fully open",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                            newdata = fully_open_low_if,
                                         se.fit = TRUE,
                                         type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_high_if$age_scaled,
                 Access = "Fully open",
                 Impact = "High",
                 citations = predict(object = pref_model,
                                            newdata = fully_open_high_if,
                                            se.fit = TRUE,
                                            type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_high_if$age_scaled,
                 Access = "Open code and data",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                     newdata = open_data_and_code_low_if,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_high_if$age_scaled,
                 Access = "Open code and publication",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                     newdata = open_pub_and_code_low_if,
                                     se.fit = TRUE,
                                     type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_high_if$age_scaled,
                 Access = "Open data and publication",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                     newdata = open_pub_and_data_low_if,
                                     se.fit = TRUE,
                                     type="response")),
      
      
      
      
      data.frame(year=2010:2022,
                 age_scaled = fully_closed_high_if$age_scaled,
                 Access = "Fully closed",
                 Impact = "High",
                 citations = predict(object = pref_model,
                                            newdata = fully_closed_high_if,
                                         se.fit = TRUE,
                                         type="response")),
      data.frame(year=2010:2022,
                 age_scaled = fully_closed_low_if$age_scaled,
                 Access = "Fully closed",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                            newdata = fully_closed_low_if,
                                         se.fit = TRUE,
                                         type="response")),
      
      
      
      data.frame(year=2010:2022,
                 age_scaled = open_code_low_if$age_scaled,
                 Access = "Open code",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                            newdata = open_code_low_if,
                                         se.fit = TRUE,
                                         type="response")),
      
      data.frame(year=2010:2022,
                 age_scaled = open_pub_low_if$age_scaled,
                 Access = "Open publication",
                 Impact = "Low",
                 citations = predict(object = pref_model,
                                            newdata = open_pub_low_if,
                                            se.fit = TRUE,
                                            type="response"))
    ) 
  
  predicted_data_if$age <- predicted_data_if$age_scaled*age_scaling$scale + age_scaling$center

  predicted_data_if %>%
    mutate(citations = citations.fit) -> predicted_data_if
  
  
  predicted_data_if %>%
    dplyr::filter((Impact == "Low" & Access == "Fully open")|
                    (Impact =="Low"  & Access == "Open code and data")|
                    (Impact == "Low" & Access == "Open code and publication")|
                    (Impact == "Low" & Access == "Open data and publication")|
                    (Impact == "High" & Access == "Fully closed"))%>%
    mutate(Scenario = case_when(Impact == "Low" & Access == "Fully open" ~ 'Fully open,\n low IF',
                                Impact == "Low"  & Access == "Open code and data" ~ 'Open code and data,\n low IF',
                                Impact == "Low" & Access == "Open code and publication" ~'Open code and pub.,\n low IF',
                                Impact == "Low" & Access == "Open data and publication" ~ 'Open data and pub.,\n low IF',
                                Impact == "High" & Access == "Fully closed" ~'Fully closed,\n high IF')) %>%
    mutate(Scenario = factor(x=Scenario,levels=c('Fully open,\n low IF',
                                           'Open code and data,\n low IF',
                                           'Open code and pub.,\n low IF',
                                           'Fully closed,\n high IF',
                                           'Open data and pub.,\n low IF'))) %>%
  ggplot(mapping = aes(x=`age`,y=citations,color=Scenario))+
    geom_line(mapping = aes(size = Scenario))+
    scale_size_manual(values = c(.75,.75,.75,1.5,.75))+
    # geom_ribbon(mapping = aes(ymin=citations_lower,
    #                           ymax=citations_upper,
    #                           fill=Access,color=NULL),
    #             alpha=0.25)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(#limits = c(0,120),
                       breaks = seq(0,550, 100),
                       minor_breaks = seq(0,550,10),
                       trans = "log10",
                       expand = c(0, 0))+
    ylab("Cumulative Citations")+
    xlab("Years Since Publication")+
    # scale_color_discrete(breaks=c('Fully open,\n low IF',
    #                               'Open code and data,\n low IF',
    #                               'Open code and pub.,\n low IF',
    #                               'Fully closed,\n high IF',
    #                               'Open data and pub.,\n low IF'
    #                               ))+
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=15),
          axis.title.y = element_text(vjust=2,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15)) -> fig3
  fig3

  ggsave(plot = fig3, filename = "figures/figure3.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig3, filename = "figures/figure3.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  

  #' predicted_data_if %>%
  #'   filter(Access %in% c("Fully open", "Fully closed")) %>%
  #'   ggplot(mapping = aes(x=`age`,
  #'                        y=citations,
  #'                        lty=Impact,
  #'                        color=Access))+
  #'   geom_line(size=0.75)+
  #'   # geom_ribbon(mapping = aes(ymin=citations_lower,
  #'   #                           ymax=citations_upper,
  #'   #                           fill=Access,
  #'   #                           color=NULL),
  #'   #             alpha=0.25)+
  #'   scale_x_continuous(limits = c(1,13),
  #'                      breaks = seq(1,13, 1),
  #'                      minor_breaks = NULL,
  #'                      expand = c(0, 0))+
  #'   scale_y_continuous(#limits = c(0,120),
  #'                      #breaks = seq(0,120, 10),
  #'                      #minor_breaks = NULL,
  #'                      trans = "log10",
  #'                       expand = c(0, 0))+
  #'   ylab("Cumulative Citations")+
  #'   xlab("Age")+
  #'   scale_color_discrete(breaks=c('Fully open',
  #'                                 #'Open code',
  #'                                 #'Open publication',
  #'                                 'Fully closed'),
  #'                        type = c("#F8766D","#7CAE00"))+
  #'   scale_fill_discrete(breaks=c('Fully open',
  #'                                #'Open code',
  #'                                #'Open publication',
  #'                                'Fully closed'),
  #'                       type = c("#F8766D","#7CAE00"))+
  #'   
  #'   theme_bw()+
  #'   theme(axis.text.x = element_text(vjust = 0.9, size=20),
  #'         axis.text.y = element_text(size=20),
  #'         axis.title.y = element_text(vjust=2,size=20),
  #'         axis.title.x = element_text(size=20),
  #'         legend.text = element_text(size=20),
  #'         legend.title = element_text(size=15)) -> fig3b
  #' 
  #' fig3b  

  # ggsave(plot = fig3b, filename = "figures/figure3b.svg",
  #        width = 10,height = 5,units = "in",dpi = 600)
  # 
  # ggsave(plot = fig3b, filename = "figures/figure3b.jpg",
  #        width = 10,height = 5,units = "in",dpi = 600)
  
#############

  # IF citations
  
  predicted_data_if %>%
    select(year,Access,Impact,citations)%>%
    mutate(age = 2023-year) %>%
    mutate(trt = paste(Access,Impact) )%>%
    dplyr::select(trt,age,citations)%>%
    pivot_wider(names_from = trt,values_from = citations)->pred_if_wide
    

  
#############
  
  # How many more citations do you get from fully open vs fully closed?
  
    predicted_data %>%
    filter(age==13,
           Access %in% c("Fully open", "Fully closed")) %>%
    pull(citations) %>%
    diff() #727 more citations after 13 years for going fully open vs fully closed
  
  predicted_data %>%
    #filter(age==13)%>%,
    filter(Access %in% c("Open code and data", "Fully closed","Fully open")) %>%
    select(year,age,Access,citations)%>%
    pivot_wider(names_from = Access,values_from = citations)
  
  
    
###############################  
      
############################################################################
    
  # Alternate versions of figure 1 that I aren't in the ms, but look cool
    
    
    
    line_hex <- "#F990E8"
    point_hex <- "#bf00ff"
    trend_hex <- "#0BD2D3"
    
    cite_data %>%
      group_by(year) %>%
      summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
                n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
                prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available),
                pct_scripts_available = prop_scripts_available *100) %>%
      ungroup() %>%
      mutate(year=as.integer(year)) %>%
      ggplot(aes(x=year,
                 y=pct_scripts_available))+
      #geom_line()+
      geom_point(color=point_hex)+
      ylab("Papers with R Scripts Available (percent)")+
      xlab("Year")+
      scale_x_continuous(limits = c(2009.8, 2022.2),
                         breaks = seq(2010, 2022, 1),
                         expand = c(0,0))+
      scale_y_continuous(limits=c(0,30),
                         breaks = seq(0,30,5),
                         expand = c(0,0))+
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9))+
      geom_smooth(method = "lm",se = FALSE,lty=2,color=trend_hex)+
      stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                     after_stat(rr.label),
                                     after_stat(p.value.label),
                                     sep = "*\", \"*")))+
      theme_bw()+
      theme(
        panel.grid = element_line(colour=line_hex),
        panel.border = element_rect(colour=line_hex),
        axis.ticks = element_line(colour=line_hex),
      )-> fig1_neon
    
    
    cite_data %>%
      group_by(year) %>%
      summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
                n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
                prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available),
                pct_scripts_available = prop_scripts_available *100) %>%
      ungroup() %>%
      mutate(year=as.integer(year)) %>%
      ggplot(aes(x=year,
                 y=pct_scripts_available))+
      #geom_line()+
      geom_point(color=point_hex)+
      ylab("Papers with R Scripts Available (percent)")+
      xlab("Year")+
      scale_x_continuous(limits = c(2009.8, 2022.2),
                         breaks = seq(2010, 2022, 1),
                         expand = c(0,0))+
      scale_y_continuous(limits=c(0,30),
                         breaks = seq(0,30,5),
                         expand = c(0,0))+
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9))+
      geom_smooth(method = "lm",se = FALSE,lty=2,color=trend_hex)+
      stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                     after_stat(rr.label),
                                     after_stat(p.value.label),
                                     sep = "*\", \"*")),color="white")+
      theme_bw()+
      theme(
        panel.grid = element_line(colour=line_hex),
        panel.border = element_rect(colour=line_hex),
        axis.ticks = element_line(colour=line_hex),
        panel.background = element_rect(fill="black")
      ) -> fig1_dark
    
    
    
    ggsave(plot = fig1_neon, filename = "figures/figure1_neon.svg",
           width = 10,height = 5,units = "in",dpi = 600)
    
    ggsave(plot = fig1_neon, filename = "figures/figure1_neon.jpg",
           width = 10,height = 5,units = "in",dpi = 600)
    
    
    ggsave(plot = fig1_dark, filename = "figures/figure1_dark.svg",
           width = 10,height = 5,units = "in",dpi = 600)
    
    ggsave(plot = fig1_dark, filename = "figures/figure1_dark.jpg",
           width = 10,height = 5,units = "in",dpi = 600)
    
###########################################
    
  # code to check what packages I've used
    
    library(questionr)
    packages_used <- questionr::qscan(list.files(pattern = ".R",
                                                 recursive = TRUE,
                                                 full.names = TRUE),
                                      load = FALSE) %>%
      unlist() %>%
      as.character()%>%
      unique()
  
    packages_used
    
    packageVersion("stringdist") #0.9.12
    packageVersion("tidyverse") #2.0.0
    packageVersion("googledrive") #2.1.1
    packageVersion("googlesheets4") #1.1.1
    
    packageVersion("bbmle") #1.0.25.1
    packageVersion("MuMIn") #1.47.5     
    packageVersion("rscopus") #0.6.6
    packageVersion("rsq") #2.6
    packageVersion("scholar") #0.2.4
    packageVersion("stats") #4.3.0

    packageVersion("ggplot2") #3.4.4
    packageVersion("ggpmisc") #0.5.5
    
    packageVersion("questionr") #0.7.8

