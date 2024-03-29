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

# Save the data so I don't have to re-download it
  
  # saveRDS(object = cite_data,file = "data/cite_data.RDS")
  cite_data <- readRDS("data/cite_data.RDS") %>% ungroup()

  
######################################################################################
  
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
    mutate(sig = case_when(Availability %in% c("Open Publication", "Open Code") ~ 2,
                           Availability %in% c("Open Data") ~ 1)) -> temp
  
  temp %>%
    ggplot(aes(x=year,
               y=value,
               color = Availability))+
    #geom_line()+
    geom_point(size=3)+
    geom_smooth(mapping = aes(lty = as.character(sig)),method = "lm",se = TRUE,show.legend = FALSE)+
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
    stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                   after_stat(rr.label),
                                   after_stat(p.value.label),
                                   sep = "*\", \"*")))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9,size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(angle = 90, vjust=3,size=20),
          axis.title.x = element_text(size=20),
          title = element_text(size=20),
          legend.title=element_blank(),
          legend.text = element_text(size=20))->fig1_rev
  
  fig1_rev
  
  ggsave(plot = fig1_rev, filename = "figures/figure1_rev.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig1_rev, filename = "figures/figure1_rev.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  
  
######################################################################################  

# Figure 1 (origina)  
      
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
              prop_scripts_available = n_scripts_available /
                (n_scripts_available + n_scripts_not_available),
              pct_scripts_available = prop_scripts_available *100) %>%
    ungroup() %>%
    mutate(year=as.integer(year)) %>%
    ggplot(aes(x=year,
               y=pct_scripts_available))+
    #geom_line()+
    geom_smooth(method = "lm",se = TRUE,lty=2,color="grey")+
    geom_point(size=3)+
    ylab("\n Code-sharing papers")+
    #ggtitle("Percentage of Papers with R Scripts Available")+
    #ylab(NULL)+
    xlab("\nYear")+
    scale_x_continuous(limits = c(2009.7, 2022.3),
                       breaks = seq(2010, 2022, 1),
                       expand = c(0,0),
                       minor_breaks = NULL)+
    scale_y_continuous(limits=c(-2,15),
                       breaks = seq(0,15,5),
                       expand = c(0,1),
                       minor_breaks = seq(0,30,1),
                       labels = c("0%","5%","10%","15%"))+
    # stat_poly_eq(aes(label = paste(after_stat(eq.label),
    #                                after_stat(rr.label),
    #                                after_stat(p.value.label),
    #                                sep = "*\", \"*")),size=5)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=.9,size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(angle = 90, vjust=3,size=20),
          axis.title.x = element_text(size=20),
          title = element_text(size=20)) -> fig1
  fig1
  ggsave(plot = fig1, filename = "figures/figure1.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig1, filename = "figures/figure1.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  
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

#######################################################################
  

  # Papers with data as a function of the year, model as proportion data
  
    cite_data %>%
      mutate(r_scripts_available_binary = case_when(r_scripts_available == "yes" ~ 1,
                                                    r_scripts_available == "no" ~ 0))%>%
      mutate(year = year-2010) %>%
      glm(formula = r_scripts_available_binary ~ year,
          family = "binomial") -> cite_v_year_binary
  
  summary(cite_v_year_binary)
  


  new_binary <- data.frame(year = 0:12,
                           r_scripts_available_binary = 1)
  
  binary_logit_preds <- predict(object = cite_v_year_binary,
          newdata = new_binary)
  

  prob.predictions <- 1 / (1 + exp(-binary_logit_preds))
  

  plot()

  lm(prob.predictions~new_binary$year)
  
    
  
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
  
  impact_factors <- read.csv(file = "data/manual_downloads/impact_factors.csv",sep = ";")
  
  cite_data %>%
    left_join(y = impact_factors,
              by = c("journal" = "needed_journals")) -> cite_data
  
  
  ######################
  
  # Modeling citation rate
  
    cite_data %>%
      mutate(citations = as.numeric(citations)) -> cite_data
    
    cite_data %>%
      mutate(log_cites = log10(as.numeric(citations)+1))->
      cite_data
    
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
  
  
  AICtab(m1,m2,m3,m4,m5)
  summary(m5)
  summary(m4)
  pref_model <- m4 #selecting this because it is simpler (also has lower AIC)
  
  
################################################

  # Predicted data for plotting
  
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
                 Access = "Fully open",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_open,
                                            interval = "confidence",
                                            level = 0.95)),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_closed$age_scaled,
                 Access = "Fully closed",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_closed,
                                            interval = "confidence",
                                            level = 0.95)),
      
      data.frame(year=2010:2022,
                 age_scaled = open_code$age_scaled,
                 Access = "Open code",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = open_code,
                                            interval = "confidence",
                                            level = 0.95)),
      
      data.frame(year=2010:2022,
                 age_scaled = open_pub$age_scaled,
                 Access = "Open publication",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = open_pub,
                                            interval = "confidence",
                                            level = 0.95))
    )
  
  
  predicted_data$age <- predicted_data$age_scaled*age_scaling$scale + age_scaling$center
  
  predicted_data %>%
    mutate(citations = (10^log_citations.fit)-1,
           citations_upper = (10^log_citations.upr)-1,
           citations_lower = (10^log_citations.lwr)-1) -> predicted_data
  
  #add access column
  
  cite_data %>%
    mutate(Access = case_when(open_access == "1" &
                                r_scripts_available == "yes" ~ "Fully open",
                              open_access == "0" &
                                r_scripts_available == "yes" ~ "Open code",
                              open_access == "1" &
                                r_scripts_available == "no" ~ "Open publication",
                              open_access == "0" &
                                r_scripts_available == "no" ~ "Fully closed"
    )
    ) ->cite_data
  
  
  # predicted_data %>%
  #   ggplot(mapping = aes(x=`age`,y=citations,color=access))+
  #   geom_line(size=1.5)+
  #   scale_x_continuous(limits = c(1,13),
  #                      breaks = seq(1,13, 1),
  #                      minor_breaks = NULL,
  #                      expand = c(0, 0))+
  #   scale_y_continuous(limits = c(0,100),
  #                      breaks = seq(0,100, 10),
  #                      minor_breaks = NULL,
  #                      expand = c(0, 0))+
  #   ylab("\nPredicted Cumulative Citations")+
  #   xlab("\nAge (Years)")+
  #   scale_color_discrete(breaks=c('fully open',
  #                                 'open code',
  #                                 'open publication',
  #                                 'fully closed'))+
  #   theme_bw()+
  #   theme(axis.text.x = element_text(vjust = 0.9, size=13),
  #         axis.text.y = element_text(size=14),
  #         axis.title.y = element_text(vjust=3,size=14),
  #         axis.title.x = element_text(size=13),
  #         legend.text = element_text(size=13),
  #         legend.title = element_text(size=13)) -> fig2
  
  
  predicted_data %>%
    ggplot(mapping = aes(x=`age`,
                         y=citations,
                         color=Access))+
    geom_boxplot(data = cite_data,
                 mapping = aes(x=age,
                               y=citations,
                               color=Access,
                               group = interaction(Access,age)))+
    geom_line(size=1.5)+
    # geom_ribbon(mapping = aes(ymin=citations_lower,
    #                           ymax=citations_upper,
    #                           fill=Access),alpha=0.5)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(1,170),
                       #breaks = seq(0,100, 10),
                       minor_breaks = seq(0,170, 10),
                       expand = c(0, 0)
                       ,trans = "log10"
    )+
    ylab("\nCumulative Citations")+
    xlab("\nYears Since Publication")+
    scale_color_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'))+
    scale_fill_discrete(breaks=c('Fully open',
                                 'Open code',
                                 'Open publication',
                                 'Fully closed'))+
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=13),
          axis.text.y = element_text(size=14),
          axis.title.y = element_text(vjust=3,size=14),
          axis.title.x = element_text(size=13),
          legend.text = element_text(size=13),
          legend.title = element_text(size=13)) -> fig2
  fig2
  
  
  ggsave(plot = fig2, filename = "figures/figure2.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig2, filename = "figures/figure2.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  
  
  
  predicted_data %>%
    ggplot(mapping = aes(x=`age`,
                         y=citations,
                         color=Access))+
    # geom_boxplot(data = cite_data,
    #              mapping = aes(x=age,
    #                            y=citations,
    #                            color=access,
    #                            group = interaction(access,age)))+
    geom_line(size=1.5)+
    geom_ribbon(mapping = aes(ymin=citations_lower,
                              ymax=citations_upper,
                              fill=Access,color=NULL),alpha=0.25)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(0.2,170),
                       #breaks = seq(0,100, 10),
                       minor_breaks = seq(0,170, 10),
                       expand = c(0, 0)
                       #,trans = "log10"
                       )+
    ylab("\nCumulative Citations")+
    xlab("\nYears Since Publication")+
    scale_color_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'))+
    scale_fill_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'))+
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(vjust=3,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) -> fig2b
  fig2b
  
  
  ggsave(plot = fig2b, filename = "figures/figure2b.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig2b, filename = "figures/figure2b.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  

  
  #trying to make a version of figure 2 where its easier to tell open pub and fully closed apart
  predicted_data %>%
    ggplot(mapping = aes(x=`age`,
                         y=citations,
                         color=Access))+
    # geom_boxplot(data = cite_data,
    #              mapping = aes(x=age,
    #                            y=citations,
    #                            color=access,
    #                            group = interaction(access,age)))+
    geom_line(size=0.75)+
    geom_ribbon(mapping = aes(ymin=citations_lower,
                              ymax=citations_upper,
                              fill=Access,color=NULL),alpha=0.25)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(0.2,170),
                       #breaks = seq(0,100, 10),
                       minor_breaks = seq(0,170, 10),
                       expand = c(0, 0)
                       #,trans = "log10"
    )+
    ylab("\nCumulative Citations")+
    xlab("\nYears Since Publication")+
    scale_color_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'))+
    scale_fill_discrete(breaks=c('Fully open',
                                 'Open code',
                                 'Open publication',
                                 'Fully closed'))+
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(vjust=3,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20)) -> fig2c
  fig2c
  
  
  ggsave(plot = fig2c, filename = "figures/figure2c.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig2c, filename = "figures/figure2c.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
    
  
  
  
  
#############  
  
  # Predicted data for plotting
  
  fully_open_low_if <- data.frame(r_scripts_available ="yes",
                           age_scaled = unique(cite_data$age_scaled),
                           open_access="1",
                           ImpactFactor_scaled = as.numeric(quantile(cite_data$ImpactFactor_scaled,probs = 0.1)))
  
  fully_open_high_if <- data.frame(r_scripts_available ="yes",
                                  age_scaled = unique(cite_data$age_scaled),
                                  open_access="1",
                                  ImpactFactor_scaled = as.numeric(quantile(cite_data$ImpactFactor_scaled,probs = 0.9)))
  
  open_code_low_if <- data.frame(r_scripts_available ="yes",
                          age_scaled = unique(cite_data$age_scaled),
                          open_access="0",
                          ImpactFactor_scaled =quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  open_pub_low_if <- data.frame(r_scripts_available ="no",
                         age_scaled = unique(cite_data$age_scaled),
                         open_access="1",
                         ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  
  fully_closed_high_if <- data.frame(r_scripts_available ="no",
                             age_scaled = unique(cite_data$age_scaled),
                             open_access="0",
                             ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.9)|>as.numeric())
  
  fully_closed_low_if <- data.frame(r_scripts_available ="no",
                                     age_scaled = unique(cite_data$age_scaled),
                                     open_access="0",
                                     ImpactFactor_scaled = quantile(cite_data$ImpactFactor_scaled,probs = 0.1)|>as.numeric())
  

  
  predicted_data_if <-
    bind_rows(  
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_low_if$age_scaled,
                 Access = "Fully open",
                 Impact = "Low",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_open_low_if,
                                            interval = "confidence",
                                            level = 0.95)),
      
      data.frame(year=2010:2022,
                 age_scaled = fully_open_high_if$age_scaled,
                 Access = "Fully open",
                 Impact = "High",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_open_high_if,
                                            interval = "confidence",
                                            level = 0.95)),
      
      
      data.frame(year=2010:2022,
                 age_scaled = fully_closed_high_if$age_scaled,
                 Access = "Fully closed",
                 Impact = "High",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_closed_high_if,
                                            interval = "confidence",
                                            level = 0.95)),
      data.frame(year=2010:2022,
                 age_scaled = fully_closed_low_if$age_scaled,
                 Access = "Fully closed",
                 Impact = "Low",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = fully_closed_low_if,
                                            interval = "confidence",
                                            level = 0.95)),
      
      
      
      data.frame(year=2010:2022,
                 age_scaled = open_code_low_if$age_scaled,
                 Access = "Open code",
                 Impact = "Low",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = open_code_low_if,
                                            interval = "confidence",
                                            level = 0.95)),
      
      data.frame(year=2010:2022,
                 age_scaled = open_pub_low_if$age_scaled,
                 Access = "Open publication",
                 Impact = "Low",
                 log_citations = predict.lm(object = pref_model,
                                            newdata = open_pub_low_if,
                                            interval = "confidence",
                                            level = 0.95))
    )
  
  
  predicted_data_if$age <- predicted_data_if$age_scaled*age_scaling$scale + age_scaling$center
  
  predicted_data_if %>%
    mutate(citations = (10^log_citations.fit)-1,
           citations_lower = (10^log_citations.lwr)-1,
           citations_upper = (10^log_citations.upr)-1) -> predicted_data_if
  

  predicted_data_if %>%
    filter(!(Impact == "High" & Access == "Fully open"),
           !(Impact == "Low" & Access == "Fully closed"))%>%
    ggplot(mapping = aes(x=`age`,y=citations,color=Access))+
    geom_line(size=0.75)+
    geom_ribbon(mapping = aes(ymin=citations_lower,
                              ymax=citations_upper,
                              fill=Access,color=NULL),
                alpha=0.25)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    scale_y_continuous(limits = c(0,120),
                       breaks = seq(0,120, 10),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    ylab("Predicted Cumulative Citations")+
    xlab("Age")+
    scale_color_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'),
                         labels = c('Fully open,\n low IF',
                                    'Open code,\n low IF',
                                    'Open publication,\n low IF',
                                    'Fully closed,\n high IF'))+
    scale_fill_discrete(breaks=c('Fully open',
                                  'Open code',
                                  'Open publication',
                                  'Fully closed'),
                        labels = c('Fully open,\n low IF',
                                   'Open code,\n low IF',
                                   'Open publication,\n low IF',
                                   'Fully closed,\n high IF'))+
    
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(vjust=3,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=15)) -> fig3
  fig3

  
  
  predicted_data_if %>%
    filter(Access %in% c("Fully open","Fully closed"))%>%
    ggplot(mapping = aes(x=`age`,
                         y=citations,
                         lty=Impact,
                         color=Access))+
    geom_line(size=0.75)+
    geom_ribbon(mapping = aes(ymin=citations_lower,
                              ymax=citations_upper,
                              fill=Access,
                              color=NULL),
                alpha=0.25)+
    scale_x_continuous(limits = c(1,13),
                       breaks = seq(1,13, 1),
                       minor_breaks = NULL,
                       expand = c(0, 0))+
    # scale_y_continuous(limits = c(0,120),
    #                    breaks = seq(0,120, 10),
    #                    minor_breaks = NULL,
    #                    expand = c(0, 0))+
    ylab("Cumulative Citations")+
    xlab("Age")+
    scale_color_discrete(breaks=c('Fully open',
                                  #'Open code',
                                  #'Open publication',
                                  'Fully closed'),
                         type = c("#F8766D","#7CAE00"))+
    scale_fill_discrete(breaks=c('Fully open',
                                 #'Open code',
                                 #'Open publication',
                                 'Fully closed'),
                        type = c("#F8766D","#7CAE00"))+
    
    theme_bw()+
    theme(axis.text.x = element_text(vjust = 0.9, size=20),
          axis.text.y = element_text(size=20),
          axis.title.y = element_text(vjust=2,size=20),
          axis.title.x = element_text(size=20),
          legend.text = element_text(size=20),
          legend.title = element_text(size=15)) -> fig3b
  
  fig3b  

  ggsave(plot = fig3b, filename = "figures/figure3b.svg",
         width = 10,height = 5,units = "in",dpi = 600)
  
  ggsave(plot = fig3b, filename = "figures/figure3b.jpg",
         width = 10,height = 5,units = "in",dpi = 600)
  
#############
  
  # How many more citations do you get from fully open vs fully closed?
  
    predicted_data %>%
    filter(age==13,
           Access %in% c("Fully open", "Fully closed")) %>%
    pull(citations)%>%
    diff() #56 more citations after 13 years for going fully open vs fully closed
  

  
    
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
    

