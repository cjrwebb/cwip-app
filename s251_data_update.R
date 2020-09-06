# Section 251 Data Merge 

library(tidyverse)
library(readxl)
library(janitor)

# read in data
s251_2011 <- read_xls("data/Section251_outturns/s251ye2011.xls", skip = 3) %>%
  clean_names()

s251_2012 <- read_xlsx("data/Section251_outturns/s251ye2012.xlsx", skip = 3) %>%
  clean_names()

s251_2013 <- read_xlsx("data/Section251_outturns/s251ye2013.xlsx", skip = 3) %>%
  clean_names()

s251_2014 <- readODS::read_ods("data/Section251_outturns/s251ye2014.ods") %>%
  as_tibble() %>%
  clean_names()

s251_2015 <- read_xlsx("data/Section251_outturns/s251ye2015.xlsx") %>%
  clean_names()

s251_2016 <- read_xlsx("data/Section251_outturns/s251ye2016.xlsx") %>%
  clean_names()

s251_2017 <- read_xlsx("data/Section251_outturns/s251ye2017.xlsx") %>%
  clean_names()
  
s251_2018 <- read_xlsx("data/Section251_outturns/s251ye2018.xlsx") %>%
  clean_names()

s251_2019 <- read_xlsx("data/Section251_outturns/s251ye2019.xlsx") %>%
  clean_names()



# Make sure LA keys work


# try to match all categories



# Merge different years


# Add denominators