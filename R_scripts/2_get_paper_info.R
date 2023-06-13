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

# Pull the current version of the google sheet

cite_data <- read_sheet("1pYB_oJt-Sx__OKJdmlgEpBmDFLmGLxh9Rddp9qKNooE")

# Limit the data to the comprehensive sampling at the beginning (to avoid any biases due to more freely-available journals)

cite_data %>%
  slice_head(n = 1001) -> cite_data #1001 samples so that each paper gets 77 samples (77 samples per year * 13 years = 1001)


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
  scale_y_continuous(limits=c(0,100),
                     breaks = seq(0,100,10),
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
  
  
# Citation rate
  
library(ggpubr)  
  
  cite_data %>%
    mutate(citations = as.numeric(citations)) -> cite_data
  
  cite_data %>%
    filter(!is.na(r_scripts_available))%>%
    ggplot(mapping = aes(x=r_scripts_available,y = citations, group = r_scripts_available))+
    geom_boxplot()+
    stat_compare_means(vjust = +1)+
    facet_wrap(~year,scales = "free")

  cite_data %>%
    filter(!is.na(r_scripts_available))%>%
    ggplot(mapping = aes(x=r_scripts_available,y = citations, group = r_scripts_available))+
    geom_boxplot()+
    stat_compare_means(method = "anova",
                       vjust = +1)+
    facet_wrap(~year,scales = "free")+
    xlab("R scripts available?")
  
  cite_data %>%
    mutate(log_cites = log10(as.numeric(citations)+1))->
    cite_data
    
  
  library(bbmle)

  m5 <- lm(data = cite_data,
           formula = log_cites ~ r_scripts_available*year + open_access*r_scripts_available + open_access*year)
  
  m4 <- lm(data = cite_data,
           formula = log_cites ~ r_scripts_available*year + open_access*year)
  
  m3 <- lm(data = cite_data,
           formula = log_cites ~ r_scripts_available*year + open_access*r_scripts_available)
  
  m2 <- lm(data = cite_data,
           formula = log_cites ~ r_scripts_available*year + open_access)
  
  m1 <- lm(data = cite_data,
           formula = log_cites ~ r_scripts_available*year)
  
  AICtab(m1,m2,m3,m4,m5)
  
  #since models 4 and 5 have equivalent aics, we'll go with the more parsimonious

  pref_model <- m4
  
  summary(pref_model)

  #Open access impacts likelihood of sharing?
library(tidyverse)  
  
  cite_data %>%
    dplyr::filter(r_scripts_available %in% c("yes","no"))%>%
    group_by(year,open_access) %>%
    summarise(n = n(),
              n_scripts_available = sum(na.omit(r_scripts_available=="yes"))
              )%>%
    mutate(frac_w_scripts = n_scripts_available/n)%>%
    ggplot(mapping = aes(x=year,y=frac_w_scripts,fill=open_access))+
    geom_bar(stat = "identity",position = position_dodge())+
    scale_x_continuous(limits = c(2010, 2022),
                       breaks = seq(2010, 2022, 1),minor_breaks = NULL)
  
  
  cite_data %>%
    dplyr::filter(r_scripts_available %in% c("yes","no"))%>%
    group_by(year,open_access) %>%
    summarise(n = n(),
              n_scripts_available = sum(na.omit(r_scripts_available=="yes"))
    )%>%
    mutate(frac_w_scripts = n_scripts_available/n)%>%
    group_by(open_access)%>%
    summarise(mean_frac_w_scripts = mean(frac_w_scripts))
  
#######################
  
  #Plotting preferred model
  

  full_open <- data.frame(r_scripts_available ="yes",
                          year = 2010:2022,
                          open_access="1")
    
  open_code <- data.frame(r_scripts_available ="yes",
                          year = 2010:2022,
                          open_access="0")
  
  open_pub <- data.frame(r_scripts_available ="no",
                          year = 2010:2022,
                          open_access="1")
  
  full_closed <- data.frame(r_scripts_available ="no",
                         year = 2010:2022,
                         open_access="0")
  
  
  
  predicted_data <-
        bind_rows(  
      
        data.frame(year=2010:2022,
                   access = "full open",
                   citations = predict.lm(object = pref_model,newdata = full_open)),
        
        data.frame(year=2010:2022,
                   access = "full closed",
                   citations = predict.lm(object = pref_model,newdata = full_closed)),
        
        data.frame(year=2010:2022,
                   access = "open code",
                   citations = predict.lm(object = pref_model,newdata = open_code)),
        
        data.frame(year=2010:2022,
                   access = "open publication",
                   citations = predict.lm(object = pref_model,newdata = open_pub))
        )

  
  #Transform citations back into original units

    predicted_data %>%
      mutate(citations = citations -1)%>%
      mutate(citations = 10^citations) -> predicted_data
  
  predicted_data %>%
    mutate('Years Since Publication' = abs(year-2022))%>%
  ggplot(mapping = aes(x=`Years Since Publication`,y=citations,color=access))+
    geom_line(size=1.5)+
    scale_x_continuous(limits = c(0,12),
                       breaks = seq(0,12, 1),minor_breaks = NULL)+
    ylab("Predicted Citations")+
    scale_color_discrete(breaks=c('full open',
                                 'open code',
                                 'open publication',
                                 'full closed'))+
    theme_bw()+
    scale_x_continuous(limits = c(0,12), expand = c(0, 0)) 



#############

  
  #cites are roughly normal after log transforming  
  cite_data %>%
    filter(year == 2020) %>%
    mutate(citations = as.numeric(citations))%>%
    mutate(citations=citations+1)|>
    pull(citations)|>
    log()%>%
    hist()
  
