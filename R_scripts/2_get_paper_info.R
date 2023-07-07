# in this script, the goal is to extract information from the manually-populated google sheet

# Data to be gathered includes:

# 1) Number of papers with vs without scripts available over time
# 2) A google sheet listing the packages used by each of the available scripts.  This will be used to assess correspondence between packages used and cited
    # if possible, this should include both libraries called and dependencies, and should distinguish between the two

################################################################################

# Load libraries
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(ggpmisc)
library(bbmle)

# Pull the current version of the google sheet

  #cite_data <- read_sheet("1pYB_oJt-Sx__OKJdmlgEpBmDFLmGLxh9Rddp9qKNooE")
  
# Limit the data to the comprehensive sampling at the beginning (to avoid any biases due to more freely-available journals)

  # cite_data %>%
  #   slice_head(n = 1001) -> cite_data #1001 samples so that each paper gets 77 samples (77 samples per year * 13 years = 1001)

# Save the data so I don't have to re-download it
  
  # saveRDS(object = cite_data,file = "data/cite_data.RDS")
  cite_data <- readRDS("data/cite_data.RDS")
  

# Papers with scripts included over time

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
  geom_point()+
  ylab("Papers with R Scripts Available (percent)")+
  xlab("Year")+
  scale_x_continuous(limits = c(2009.8, 2022.2),
                     breaks = seq(2010, 2022, 1),
                     expand = c(0,0))+
  scale_y_continuous(limits=c(0,30),
                     breaks = seq(0,30,5),
                     expand = c(0,0))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9))+
  geom_smooth(method = "lm",se = FALSE,lty=2,color="grey")+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label),
                                 after_stat(p.value.label),
                                 sep = "*\", \"*")))+
  theme_bw()

cite_data %>%
  group_by(year) %>%
  summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
            n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
            prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available),
            pct_scripts_available = prop_scripts_available *100) %>%
  ungroup() %>%
  mutate(year=as.integer(year)) %>%
  lm(data = .,prop_scripts_available ~ year) %>%
  summary()


#comparing AIC of linear vs exp
cite_data %>%
  group_by(year) %>%
  summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
            n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
            prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available),
            pct_scripts_available = prop_scripts_available *100) %>%
  ungroup() %>%
  mutate(year=as.integer(year))-> prop_data

prop_m1 <- lm(data = prop_data%>%
                mutate(year = year-2010), prop_scripts_available ~ year)


prop_m2 <- nls(data = prop_data%>%
                 mutate(year = year-2010),
               formula = prop_scripts_available ~ a*exp(r*year), 
               start = list(a = 1, r = 0.01))

bbmle::AICtab(prop_m1,prop_m2) # model comparable, so picking the less-complex

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

  ##########################
  
  #Add impact factor
  # Get impact factor
  
  #remotes::install_github('jkeirstead/scholar')
  library(scholar)  
  
  # needed_journals <- unique(cite_data$journal)
  # impact_factors <- scholar::get_impactfactor(journals = needed_journals,max.distance = 0.01)
  # impact_factors <- cbind(needed_journals,impact_factors)
  
  #the scholar package is currently returning some questionable matches well above the max.distance.
  #I'll toss these out.
  
  library(stringdist)
  
  # impact_factors$dist <-   stringdist(a = tolower(impact_factors$needed_journals),b = tolower(impact_factors$Journal))
  
  #write as a csv to manually update the missing or incorrect data
  
  # write.csv(x = impact_factors,
  #           file = "data/manual_downloads/impact_factors.csv",row.names = FALSE)
  
  impact_factors <- read.csv(file = "data/manual_downloads/impact_factors.csv",sep = ";")
  
  cite_data %>%
    left_join(y = impact_factors,by = c("journal" = "needed_journals")) -> cite_data
  
  
  ######################
  
  # Modeling citation rate
  
  cite_data %>%
    mutate(citations = as.numeric(citations)) -> cite_data
  
  cite_data %>%
    mutate(log_cites = log10(as.numeric(citations)+1))->
    cite_data
  
  cite_data %>%
    mutate(age = 2023-year) -> cite_data
  
  cite_data$age_scaled = scale(cite_data$age)
  cite_data$ImpactFactor_scaled = scale(cite_data$ImpactFactor)
  
  age_scaling = data.frame(scale =    attr(cite_data$age_scaled,"scaled:scale"),
                           center=   attr(cite_data$age_scaled,"scaled:center"))
  
  if_scaling = data.frame(scale =    attr(cite_data$ImpactFactor_scaled,"scaled:scale"),
                          center=   attr(cite_data$ImpactFactor_scaled,"scaled:center"))
  
  cite_data$age_scaled <- as.numeric(cite_data$age_scaled)
  cite_data$ImpactFactor_scaled <- as.numeric(cite_data$ImpactFactor_scaled)
  
  # models
  m1 <- lm(data = cite_data,
           formula = log_cites ~ 
             ImpactFactor_scaled*age_scaled )
  
  m2 <- lm(data = cite_data,
           formula = log_cites ~ 
             ImpactFactor_scaled*age_scaled +
             r_scripts_available*age_scaled )
  
  
  m3 <- lm(data = cite_data,
           formula = log_cites ~ 
             ImpactFactor_scaled*age_scaled +
             r_scripts_available*age_scaled+
             open_access*age_scaled )
  
  m4 <- lm(data = cite_data,
           formula = log_cites ~
             open_access*r_scripts_available +
             age_scaled*ImpactFactor_scaled +
             age_scaled*r_scripts_available)
  
  
  m5 <- lm(data = cite_data,
           formula = log_cites ~ 
             ImpactFactor_scaled*age_scaled +
             r_scripts_available*age_scaled +
             open_access*age_scaled +
             open_access*r_scripts_available ) #include NS age x open access 
  
  
  pref_model <- m4
  
  
  colnames(cite_data)
  AICtab(m1,m2,m3,m4,m5)
  summary(m5)
  summary(m4)
  
  
  ################################################
  summary(m4)
  
  mean(cite_data$ImpactFactor)
  median(cite_data$ImpactFactor)
  mean(cite_data$ImpactFactor_scaled)
  cite_data$ImpactFactor_scaled
  
  ?scale
  
  attr(cite_data$ImpactFactor_scaled,"scaled:scale")
  attr(cite_data$ImpactFactor_scaled,"scaled:center")
  
  mean(cite_data$ImpactFactor)
  mean(cite_data$ImpactFactor_scaled)*attr(cite_data$ImpactFactor_scaled,"scaled:scale")+attr(cite_data$ImpactFactor_scaled,"scaled:center")
  
  r1 <- d4 * attr(d4, 'scaled:scale')[col(d4)] + attr(d4, 'scaled:center')[col(d4)]
  
  
  fully_open <- data.frame(r_scripts_available ="yes",
                           age_scaled = unique(cite_data$age_scaled),
                           open_access="1",
                           ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_code <- data.frame(r_scripts_available ="yes",
                          age_scaled = unique(cite_data$age_scaled),
                          open_access="0",
                          ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  open_pub <- data.frame(r_scripts_available ="no",
                         age_scaled = unique(cite_data$age_scaled),
                         open_access="1",
                         ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  fully_closed <- data.frame(r_scripts_available ="no",
                             age_scaled = unique(cite_data$age_scaled),
                             open_access="0",
                             ImpactFactor_scaled = mean(cite_data$ImpactFactor_scaled))
  
  
  
  predicted_data <-
    bind_rows(  
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open$age_scaled,
                 access = "fully open",
                 log_citations = predict.lm(object = pref_model,newdata = fully_open)),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_closed$age_scaled,
                 access = "fully closed",
                 log_citations = predict.lm(object = pref_model,newdata = fully_closed)),
      
      data.frame(year=2010:2022,
                 age_scaled = open_code$age_scaled,
                 access = "open code",
                 log_citations = predict.lm(object = pref_model,newdata = open_code)),
      
      data.frame(year=2010:2022,
                 age_scaled = open_pub$age_scaled,
                 access = "open publication",
                 log_citations = predict.lm(object = pref_model,newdata = open_pub))
    )
  
  
  predicted_data$age <- predicted_data$age_scaled*age_scaling$scale + age_scaling$center
  
  predicted_data %>%
    mutate(citations = (10^log_citations)-1) -> predicted_data
  
  predicted_data %>%
    ggplot(mapping = aes(x=`age`,y=citations,color=access))+
    geom_line(size=1.5)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(0,100),
                       breaks = seq(0,100, 10),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    ylab("Predicted Cumulative Citations")+
    xlab("Age")+
    scale_color_discrete(breaks=c('fully open',
                                  'open code',
                                  'open publication',
                                  'fully closed'))+
    theme_bw()
  

#############

# Where is the code being stored?
  
  cite_data %>%
    filter(r_scripts_available == "yes")%>%
    select(`code location`,`code format`) -> code_storage
  
#Let's lump the appendix with the CI
  code_storage <-
  code_storage %>%
    mutate(`code location` = case_when(`code location`=="appendix" ~ "SI",
                                       `code location`!="appendix" ~ `code location`))
  
#Let's lump the rmd with R
  code_storage <-
    code_storage %>%
    mutate(`code format` = case_when(`code format`=="rmd" ~ "R",
                                       `code format`!="rmd" ~ `code format`))
  
    
  code_storage %>%
    group_by(`code location`)%>%
    summarise(counts= n())%>%
    mutate(percent = counts / sum(counts)*100)

  code_storage %>%
    group_by(`code format`)%>%
    summarise(counts= n())%>%
    mutate(percent = counts / sum(counts)*100)
  
  table(cite_data$journal)
  
  