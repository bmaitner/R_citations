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
  slice_head(n = 750) -> cite_data

# Papers with scripts included over time

cite_data %>%
  group_by(year) %>%
  summarise(n_scripts_available = sum(na.omit(r_scripts_available=="yes")),
            n_scripts_not_available = sum(na.omit(r_scripts_available=="no")),
            prop_scripts_available = n_scripts_available / (n_scripts_available + n_scripts_not_available)) %>%
  ungroup() %>%
  mutate(year=as.integer(year)) %>%
  ggplot(aes(x=year,
             y=prop_scripts_available))+
  geom_line()+
  ylab("Proportion of Papers with R Scripts Available")+
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, 1))+
  scale_y_continuous(limits=c(0,1),breaks = seq(0,1,0.1))+
  geom_smooth(method = "lm")+
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))


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
  
  
