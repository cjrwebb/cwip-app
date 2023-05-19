library(tidyverse)
library(janitor)
library(patchwork)

# cla data
cla2018_21 <- read_csv("data/ssda903-cla/2018-2021/data/cla_number_and_rate_per_10k_children.csv")
min(cla2018_21$time_period)
# year ending 31 MAR 2018 - year ending 31 MAR 2021

cla_2017 <- read_csv("data/ssda903-cla/SFR50_2017_UnderlyingData/SFR50_CLA2017.csv")
cla_2016 <- read_csv("data/ssda903-cla/SFR41_2016_UD/SFR41_CLA2016.csv")
cla_2015 <- read_csv("data/ssda903-cla/SFR34_2015_UnderlyingData/SFR34_CLA2015.csv")
cla_2014 <- read_csv("data/ssda903-cla/UD_SFR36_2014/SFR36_CLA2014.csv")
cla_2013 <- read_csv("data/ssda903-cla/SFR36_UnderlyingData/SFR36_CLA2013.csv")
cla_2012 <- read_csv("data/ssda903-cla/CLA-2012/SFR20_CLA2012.csv")
cla_2011 <- read_csv("data/ssda903-cla/underlying_20data_20sfr212011/SFR21_CLA.csv")
cla_2010 <- readxl::read_xls("data/ssda903-cla/sfr27-2010amv5.xls", sheet = 6, skip = 6)

unique(cla2018_21$geographic_level)
unique(cla2018_21$population_count)
unique(cla2018_21$population_estimate)

cla2018_21 <- cla2018_21 %>%
  filter(geographic_level == "Local authority") %>%
  select(time_period, old_la_code, new_la_code, la_name, population_count, number)

cla2018_21 <- cla2018_21 %>%
  pivot_wider(names_from = population_count, values_from = number) %>%
  rename(cla_cease = `Children ceasing to be looked after each year`,
         at31_cla = `Children looked after at 31 March each year`,
         cla_start = `Children starting to be looked after each year`)


# CLA 2017 tidy and join

cla_2017 <- cla_2017 %>%
  janitor::clean_names()

cla_2017 <- cla_2017 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2017) %>%
  mutate(time_period = 2017, .before = new_la_code)

cla_2017

cla_start_2017 <- read_csv("data/ssda903-cla/SFR50_2017_UnderlyingData/SFR50_ADM2017.csv")

cla_start_2017 <- cla_start_2017 %>%
  clean_names()

cla_start_2017 <- cla_start_2017 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2017) %>%
  mutate(time_period = 2017, .before = new_la_code)

cla_cease_2017 <- read_csv("data/ssda903-cla/SFR50_2017_UnderlyingData/SFR50_CEA2017.csv")

cla_cease_2017 <- cla_cease_2017 %>%
  clean_names()

cla_cease_2017 <- cla_cease_2017 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease2017) %>%
  mutate(time_period = 2017, .before = new_la_code)

cla_2017 <- left_join(cla_2017, cla_start_2017, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2017, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))


# CLA 2016 tidy and join

cla_2016 <- cla_2016 %>%
  janitor::clean_names()

cla_2016 <- cla_2016 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2016) %>%
  mutate(time_period = 2016, .before = new_la_code)

cla_2016

cla_start_2016 <- read_csv("data/ssda903-cla/SFR41_2016_UD/SFR41_ADM2016.csv")

cla_start_2016 <- cla_start_2016 %>%
  clean_names()

cla_start_2016 <- cla_start_2016 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2016) %>%
  mutate(time_period = 2016, .before = new_la_code)

cla_cease_2016 <- read_csv("data/ssda903-cla/SFR41_2016_UD/SFR41_CEA2016.csv")

cla_cease_2016 <- cla_cease_2016 %>%
  clean_names()

cla_cease_2016 <- cla_cease_2016 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2016, .before = new_la_code)

cla_2016 <- left_join(cla_2016, cla_start_2016, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2016, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2016

# cla_ 2015 tidy and join

cla_2015 <- cla_2015 %>%
  janitor::clean_names()

cla_2015 <- cla_2015 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2015) %>%
  mutate(time_period = 2015, .before = new_la_code)

cla_2015

cla_start_2015 <- read_csv("data/ssda903-cla/SFR34_2015_UnderlyingData/SFR34_ADM2015.csv")

cla_start_2015 <- cla_start_2015 %>%
  clean_names()

cla_start_2015 <- cla_start_2015 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2014) %>%
  mutate(time_period = 2015, .before = new_la_code)

cla_cease_2015 <- read_csv("data/ssda903-cla/SFR34_2015_UnderlyingData/SFR34_CEA2015.csv")

cla_cease_2015 <- cla_cease_2015 %>%
  clean_names()

cla_cease_2015 <- cla_cease_2015 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2015, .before = new_la_code)

cla_2015 <- left_join(cla_2015, cla_start_2015, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2015, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2015

### CLA 2014 tidy and join

cla_2014 <- cla_2014 %>%
  janitor::clean_names()

cla_2014 <- cla_2014 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2014) %>%
  mutate(time_period = 2014, .before = new_la_code)

cla_2014

cla_start_2014 <- read_csv("data/ssda903-cla/UD_SFR36_2014/SFR36_ADM2014.csv")

cla_start_2014 <- cla_start_2014 %>%
  clean_names()

cla_start_2014 <- cla_start_2014 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2014) %>%
  mutate(time_period = 2014, .before = new_la_code)

cla_cease_2014 <- read_csv("data/ssda903-cla/UD_SFR36_2014/SFR36_CEA2014.csv")

cla_cease_2014 <- cla_cease_2014 %>%
  clean_names()

cla_cease_2014 <- cla_cease_2014 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2014, .before = new_la_code)

cla_2014 <- left_join(cla_2014, cla_start_2014, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2014, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2014

# CLA 2013 tidy and join

cla_2013 <- cla_2013 %>%
  janitor::clean_names()

cla_2013 <- cla_2013 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2013) %>%
  mutate(time_period = 2013, .before = new_la_code)

cla_2013

cla_start_2013 <- read_csv("data/ssda903-cla/SFR36_UnderlyingData/SFR36_ADM2013.csv")

cla_start_2013 <- cla_start_2013 %>%
  clean_names()

cla_start_2013 <- cla_start_2013 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2013) %>%
  mutate(time_period = 2013, .before = new_la_code)

cla_cease_2013 <- read_csv("data/ssda903-cla/SFR36_UnderlyingData/SFR36_CEA2013.csv")

cla_cease_2013 <- cla_cease_2013 %>%
  clean_names()

cla_cease_2013 <- cla_cease_2013 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2013, .before = new_la_code)

cla_2013 <- left_join(cla_2013, cla_start_2013, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2013, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2013

# CLA 2012 tidy and join

cla_2012 <- cla_2012 %>%
  janitor::clean_names()

cla_2012 <- cla_2012 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2012) %>%
  mutate(time_period = 2012, .before = new_la_code)

cla_2012

cla_start_2012 <- read_csv("data/ssda903-cla/CLA-2012/SFR20_ADM2012.csv")

cla_start_2012 <- cla_start_2012 %>%
  clean_names()

cla_start_2012 <- cla_start_2012 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2012) %>%
  mutate(time_period = 2012, .before = new_la_code)

cla_cease_2012 <- read_csv("data/ssda903-cla/CLA-2012/SFR_CEA2012.csv")

cla_cease_2012 <- cla_cease_2012 %>%
  clean_names()

cla_cease_2012 <- cla_cease_2012 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2012, .before = new_la_code)

cla_2012 <- left_join(cla_2012, cla_start_2012, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2012, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2012

# CLA 2011 tidy and join 

cla_2011 <- cla_2011 %>%
  janitor::clean_names()

cla_2011 <- cla_2011 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         at31_cla = cla_mar2011) %>%
  mutate(time_period = 2011, .before = new_la_code)

cla_2011

cla_start_2011 <- read_csv("data/ssda903-cla/underlying_20data_20sfr212011/SFE21_ADM.csv")

cla_start_2011 <- cla_start_2011 %>%
  clean_names()

cla_start_2011 <- cla_start_2011 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_start = cla_started2011) %>%
  mutate(time_period = 2011, .before = new_la_code)

cla_cease_2011 <- read_csv("data/ssda903-cla/underlying_20data_20sfr212011/SFR21_CEA.csv")

cla_cease_2011 <- cla_cease_2011 %>%
  clean_names()

cla_cease_2011 <- cla_cease_2011 %>%
  filter(geog_l == "LA") %>%
  select(new_la_code = new_geog_code, 
         old_la_code = geog_c, 
         la_name = geog_n, 
         cla_cease = cla_cease) %>%
  mutate(time_period = 2011, .before = new_la_code)

cla_2011 <- left_join(cla_2011, cla_start_2011, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  left_join(., cla_cease_2011, by = c("time_period", "old_la_code", "new_la_code", "la_name")) %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.))

cla_2011

# CLA 2010 Tidying and Joining - UPDATE, Check definitely Ns not rates

cla_2010 <- cla_2010 %>%
  dplyr::select(la_name = 3, at31_cla = 8) %>%
  mutate(time_period = 2010, .before = la_name)

cla_2010 <- cla_2010 %>%
  drop_na()

cla_2010 <- cla_2010 %>%
  mutate(la_name = str_remove_all(la_name, pattern = "[0-9]")) %>%
  mutate(la_join = tolower(la_name))

# cla_2010 starting
cla_start_2010 <- readxl::read_xls("data/ssda903-cla/sfr27-2010startedv3.xls", sheet = 2, skip = 5) %>%
  clean_names() %>%
  select(la_name = x1, cla_start = x2010) %>%
  mutate(time_period = 2010, .before = la_name) %>%
  drop_na() %>%
  mutate(la_name = str_remove_all(la_name, pattern = "[0-9]")) %>%
  mutate(la_name = str_replace(la_name, pattern = "&", replacement = "and")) %>%
  mutate(la_join = tolower(la_name))

cla_start_2010

# cla_2010 cease

cla_cease_2010 <- readxl::read_xls("data/ssda903-cla/sfr27-2010ceasedv3.xls", sheet = "LAD1", skip = 5) %>%
  clean_names() %>%
  select(la_name = x1, cla_cease = x2010) %>%
  mutate(time_period = 2010, .before = la_name) %>%
  drop_na() %>%
  mutate(la_name = str_remove_all(la_name, pattern = "[0-9]")) %>%
  mutate(la_name = str_replace(la_name, pattern = "&", replacement = "and")) %>%
  mutate(la_join = tolower(la_name))

anti_join(cla_2010, cla_start_2010, by = c("time_period", "la_join"))
anti_join(cla_start_2010, cla_start_2010, by = c("time_period", "la_join"))

left_join(cla_2010, cla_start_2010 %>% select(-la_name), by = c("time_period", "la_join")) %>%
  anti_join(cla_cease_2010 %>% select(-la_name), by = c("time_period", "la_join"))

cla_2010 <- left_join(cla_2010, cla_start_2010 %>% select(-la_name), by = c("time_period", "la_join")) %>%
  left_join(cla_cease_2010 %>% select(-la_name), by = c("time_period", "la_join"))



anti_join(cla_2011 %>% mutate(la_join = tolower(la_name)), 
          cla_2010, 
          by = "la_join") %>% .$la_join %>% sort(.)

sort(unique(cla_2010$la_join))

cla_2010 <- cla_2010 %>%
  mutate(
    la_join = case_when(la_join == "bedford borough" ~ "bedford",
                        la_join == "medway towns" ~ "medway",
                        la_join == "st helens" ~ "st. helens",
                        TRUE ~ la_join)
  )


cla_2010 <- left_join(cla_2010,
                      cla_2011 %>% mutate(la_join = tolower(la_name)) %>% select(la_join, fixed_la_name = la_name, new_la_code, old_la_code),
                      by = "la_join") %>%
  drop_na() %>%
  select(time_period, new_la_code, old_la_code, la_name = fixed_la_name, at31_cla, cla_start, cla_cease)

cla2018_21 <- cla2018_21 %>%
  arrange(time_period)

cla_2010 <- cla_2010 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2011 <- cla_2011 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2012 <- cla_2012 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2013 <- cla_2013 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2014 <- cla_2014 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2015 <- cla_2015 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2016 <- cla_2016 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla_2017 <- cla_2017 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))
cla2018_21 <- cla2018_21 %>%
  mutate_at(vars(old_la_code, at31_cla, cla_start, cla_cease), ~as.numeric(.))

# Change Bournemouth etc. codes
cla2018_21 <- cla2018_21 %>%
  filter(!ifelse(la_name %in% c("Bournemouth, Christchurch and Poole") & time_period < 2020, 1, 0)==1) %>%
  filter(!ifelse(la_name %in% c("Bournemouth") & time_period > 2019, 1, 0)==1) %>%
  filter(!ifelse(la_name %in% c("Poole") & time_period > 2019, 1, 0)==1) %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000028" ~ "E06000058",
                            new_la_code == "E06000029" ~ "E06000058",
                            TRUE ~ new_la_code),
    la_name = ifelse(new_la_code %in% "E06000058", "Bournemouth, Christchurch and Poole", la_name)
  ) %>%
  group_by(new_la_code, time_period, la_name) %>%
  summarise_at(vars(cla_cease, at31_cla, cla_start), sum)



cla_10_21 <- bind_rows(cla_2010, cla_2011, cla_2012, cla_2013, cla_2014, cla_2015, cla_2016, cla_2017, cla2018_21)

unique(cla_10_21$new_la_code) # some codes will need fixing.

cla_10_21 <- cla_10_21 %>%
  mutate_at(vars(at31_cla:cla_cease), ~as.numeric(.) )


# Add CLA 2022 data
cla_22 <- read_csv("data/ssda903-cla/children-looked-after-in-england-including-adoptions_2022/data/cla_number_and_rate_per_10k_children.csv")

cla_22 <- cla_22 %>%
  filter(time_period == 2022) %>%
  filter(geographic_level == "Local authority") %>%
  select(time_period, old_la_code, new_la_code, la_name, population_count, number)

cla_22 <- cla_22 %>%
  pivot_wider(names_from = population_count, values_from = number) %>%
  rename(cla_cease = `Children ceasing to be looked after each year`,
         at31_cla = `Children looked after at 31 March each year`,
         cla_start = `Children starting to be looked after each year`)

# check codes
anti_join(cla_10_21 %>% filter(time_period == 2021), cla_22, by = c("new_la_code")) # no mismatches

cla_22 <- cla_22 %>% mutate_at(vars(cla_cease:cla_start), as.numeric)
cla_22 <- cla_22 %>% drop_na() # drop old LA names

cla_10_22 <- bind_rows(cla_10_21, cla_22)


### CIN
# Children in Need
# numbers represent year ending
cin13_21 <- read_csv("data/cin-census/2020-21/characteristics-of-children-in-need_2021/data/b1_children_in_need_2013_to_2021.csv")

cin13_21 <- cin13_21 %>% 
  filter(geographic_level == "Local authority", country_name == "England") %>%
  select(time_period, old_la_code, new_la_code, la_name, 
         Anypoint_episodes, Started_episodes, At31_episodes) %>%
  clean_names()

cin12 <- read_csv("data/cin-census/2011-12/sfr27-2012udv3/SFR27-2012ud.csv") %>%
  filter(geog_l == "LA") %>%
  select(old_la_code = geog_c, la_name = geog_n, anypoint_episodes = CINThro1112,
         started_episodes  = CINStarting1112, at31_episodes = CINAt310312) %>%
  add_column(time_period = 2012, .before = "old_la_code")

# add new LA code through lookup
old_new_la_code <- cin13_21 %>% select(old_la_code, new_la_code) %>% filter(!duplicated(old_la_code))

cin_12 <- left_join(cin12, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .after = old_la_code)

cin_11 <- read_csv("data/cin-census/2010-11/osr26-2011ud.csv") %>% 
  filter(geog_l == "LA") %>%
  select(old_la_code = geog_c, la_name = geog_n, anypoint_episodes = CINThro1011,
         started_episodes  = CINStarting1011, at31_episodes = CINAt310311) %>%
  add_column(time_period = 2011, .before = "old_la_code")

cin_11 <- left_join(cin_11, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .after = old_la_code)

cin_10 <- read_csv("data/cin-census/2009-10/osr28-2010udv4/osr28-2010ud.csv") %>% 
  filter(geog_l == "LA") %>%
  select(old_la_code = geog_c, la_name = geog_n, anypoint_episodes = CINThro0910,
         started_episodes  = CINStarting0910, at31_episodes = CINAt310310) %>%
  add_column(time_period = 2010, .before = "old_la_code")

cin_10 <- left_join(cin_10, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .after = old_la_code)

# tidy up missing categories

cin13_21 <- cin13_21 %>% mutate_at(vars(anypoint_episodes:at31_episodes), ~parse_number(.))
cin_12 <-    cin_12    %>% mutate_at(vars(anypoint_episodes:at31_episodes), ~parse_number(.))
cin_11 <-   cin_11   %>% mutate_at(vars(anypoint_episodes:at31_episodes), ~parse_number(.))
cin_10 <-   cin_10   %>% mutate_at(vars(anypoint_episodes:at31_episodes), ~parse_number(.))

# join cin numbers data
cin_dat <- bind_rows(cin_10, cin_11, cin_12, cin13_21)
cin_dat

# Add 2022 CIN data
cin_22 <- read_csv("data/cin-census/2021-22/data/b1_children_in_need_2013_to_2022.csv")

cin_22 <- cin_22 %>% 
  filter(time_period == 2022) %>%
  filter(geographic_level == "Local authority", country_name == "England") %>%
  select(time_period, old_la_code, new_la_code, la_name, 
         Anypoint_episodes, Started_episodes, At31_episodes) %>%
  clean_names() %>% 
  mutate_at(vars(anypoint_episodes:at31_episodes), as.numeric) # Hackney missing

# Check matches
anti_join(cin_dat %>% filter(time_period == 2021), cin_22, by = c("new_la_code"))

# merge the two Northamptonshire
cin_22 <- cin_22 %>%
  mutate(
    la_name = case_when(new_la_code == "E06000061" ~ "Northamptonshire", # Merge Northamptonshires together
                        new_la_code == "E06000062" ~ "Northamptonshire",
                        TRUE ~ la_name),
    old_la_code = case_when(new_la_code == "E06000061" ~ 928, # Merge Northamptonshires together
                            new_la_code == "E06000062" ~ 928,
                            TRUE ~ old_la_code),
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021", # Merge Northamptonshires together
                            new_la_code == "E06000062" ~ "E10000021",
                                 TRUE ~ new_la_code)
         ) %>%
  group_by(new_la_code, la_name, old_la_code) %>%
  summarise_at(vars(anypoint_episodes:at31_episodes), sum) %>%
  ungroup() %>%
  mutate(time_period = 2022)

cin_dat <- bind_rows(cin_dat, cin_22)


# Add referrals 2009 - 2022

# 2013 to 2022
ref_13_22 <- read_csv("data/cin-census/2021-22/data/c1_children_in_need_referrals_and_rereferrals_2013_to_2022.csv") %>%
  filter(geographic_level == "Local authority") %>%
  select(time_period, old_la_code, new_la_code, la_name, 
         referrals_n = Referrals, rereferrals_n = Re_referrals, referrals_nfa_n = No_Further_Action,
         referrals_nin_n = Not_in_Need, children_referred_n = Referrals_child, children_rereferred_n = Re_referrals_child
         ) %>%
  mutate_at(vars(referrals_n:children_referred_n), as.numeric)

# Referrals1213 ReferralsWithin12mths1213 ReferralsNFA1213 ReferralsIANotInNeed1213
ref_12 <- read_csv("data/cin-census/2011-12/sfr27-2012udv3/SFR27-2012ud.csv") %>%
  select(old_la_code = geog_c, 
         la_name = geog_n, 
         referrals_n = Referrals1112, 
         rereferrals_n = ReferralsWithin12mths1112,
         referrals_nfa_n = ReferralsNFA112,
         referrals_nin_n = ReferralsIANotInNeed1112) %>%
  mutate(time_period = 2012, .before = referrals_n) %>%
  slice(-1) %>%
  mutate_at(vars(referrals_n:referrals_nin_n), parse_number)

anti_join(ref_12, old_new_la_code, by = "old_la_code")
ref_12 <- left_join(ref_12, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .before = old_la_code)

# check for mismatches and then join
anti_join(ref_13_22, ref_12, by = "new_la_code")

# referrals 2010-11
ref_11 <- read_csv("data/cin-census/2010-11/osr26-2011ud.csv") %>%
  select(old_la_code = geog_c, 
         la_name = geog_n, 
         referrals_n = Referrals1011, 
         rereferrals_n = ReferralsWithin12mths1011) %>%
  mutate(time_period = 2011, .before = referrals_n) %>%
  slice(-1) %>%
  mutate_at(vars(referrals_n:rereferrals_n), parse_number)

anti_join(ref_11, old_new_la_code, by = "old_la_code")
ref_11 <- left_join(ref_11, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .before = old_la_code)

# referrals 2010
ref_10 <- read_csv("data/cin-census/2009-10/osr28-2010udv4/osr28-2010ud.csv") %>%
  select(old_la_code = geog_c, 
         la_name = geog_n, 
         referrals_n = Referrals0910) %>%
  mutate(time_period = 2010, .before = referrals_n) %>%
  slice(-1)

anti_join(ref_10, old_new_la_code, by = "old_la_code")
ref_10 <- left_join(ref_10, old_new_la_code, by = "old_la_code") %>%
  relocate(new_la_code, .before = old_la_code)

ref_10_22 <- bind_rows(ref_13_22, ref_12, ref_11, ref_10)




# update Dorset, North Northamptonshire, West Northamptonshire, Bournemouth codes

# Update Dorset code and merge North and West Northamptonshire
ref_10_22 <- ref_10_22 %>%
  mutate(
    new_la_code = ifelse(new_la_code == "E06000059", "E10000009", new_la_code)
  )

ref_10_22 %>% filter(str_detect(la_name, "Northamptonshire")) %>% print(n=1000)
ref_10_22 %>% filter(str_detect(la_name, "Bournemouth|Poole")) %>% print(n=1000) 
ref_10_22 %>% filter(str_detect(la_name, "Dorset")) %>% print(n=1000)

ref_10_22 <- ref_10_22 %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            new_la_code == "E06000028" ~ "E06000058",
                            new_la_code == "E06000029" ~ "E06000058",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire", 
                        new_la_code == "E06000058" ~ "Bournemouth, Christchurch and Poole", 
                        TRUE ~ la_name)
  )



ref_10_22 <- ref_10_22 %>%
  mutate_at(vars(referrals_n:children_rereferred_n), as.numeric) %>%
  group_by(new_la_code, time_period) %>%
  summarise_at(vars(referrals_n:children_rereferred_n), sum)


# percent of referrals re-referred, percent NFAd, percent Not in Need, 
ref_10_22 <- ref_10_22 %>%
  mutate(
    pc_referrals_rereferral = (rereferrals_n / referrals_n) * 100,
    pc_referalls_nfa = (referrals_nfa_n / referrals_n) * 100,
    pc_referrals_nin = (referrals_nin_n / referrals_n) * 100,
    pc_children_rereferrred = (children_rereferred_n / children_referred_n) * 100
  ) %>%
  ungroup()


# Merge with cin_dat
cin_dat

anti_join(cin_dat, ref_10_22, by = c("new_la_code", "time_period"))

cin_dat <- left_join(cin_dat, ref_10_22, by = c("new_la_code", "time_period"))

# SEND

send_10_22 <- read_csv("data/send/education-health-and-care-plans_2022/data/sen2_age_caseload.csv") %>%
  filter(geographic_level == "Local authority" & characteristic_age == "Total") %>%
  select(new_la_code, la_name, time_period, 
         send_statem_n = Total_st, 
         ehcp_n = Total_ehc,
         sendst_or_ehcp_n = Total_all) %>% 
  mutate_at(vars(send_statem_n:sendst_or_ehcp_n), as.numeric)

anti_join(send_10_22, cin_dat, by = c("new_la_code", "time_period"))

send_10_22 <- send_10_22 %>%
  mutate(
    new_la_code = ifelse(new_la_code == "E06000059", "E10000009", new_la_code)
  )


send_10_22 <- send_10_22 %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            new_la_code == "E06000028" ~ "E06000058",
                            new_la_code == "E06000029" ~ "E06000058",
                            new_la_code == "E10000002" ~ "E06000060",
                            new_la_code == "E08000020" ~ "E08000037",
                            new_la_code == "E06000048" ~ "E06000057",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire", 
                        new_la_code == "E06000058" ~ "Bournemouth, Christchurch and Poole", 
                        TRUE ~ la_name)
  )


send_10_22 <- send_10_22 %>%
  group_by(new_la_code, time_period) %>%
  summarise_at(vars(send_statem_n:sendst_or_ehcp_n), sum) %>% ungroup()


cin_dat <- left_join(cin_dat, send_10_22 , by = c("new_la_code", "time_period"))


# Child population
read_csv("data/child-population/nomis_popest.csv") %>%
  mutate(row_n = row_number(), .before = 1) %>%
  clean_names() %>%
  filter(str_detect(population_estimates_local_authority_based_by_single_year_of_age, "Age "))

# each part of the table

age0_4 <- read_csv("data/child-population/nomis_popest.csv", skip = 5) %>%
  mutate(row_n = row_number() + 4, .before = 1) %>%
  filter(row_n %in% (3+2):(225-4))

age_5_9 <- read_csv("data/child-population/nomis_popest.csv", skip = 5) %>%
  mutate(row_n = row_number() + 4, .before = 1) %>%
  filter(row_n %in% (225+2):(447-4)) 

age_10_14 <- read_csv("data/child-population/nomis_popest.csv", skip = 5) %>%
  mutate(row_n = row_number() + 4, .before = 1) %>%
  filter(row_n %in% (447+2):(669-4)) 

age_15 <- read_csv("data/child-population/nomis_popest.csv", skip = 5) %>%
  mutate(row_n = row_number() + 4, .before = 1) %>%
  filter(row_n %in% (669+2):(891-4)) 

age_16 <- read_csv("data/child-population/nomis_popest.csv", skip = 5) %>%
  mutate(row_n = row_number() + 4, .before = 1) %>%
  filter(row_n %in% (891+2):max(row_n))

pop_wide <- as_tibble(cbind(age0_4$`local authority: county / unitary (as of April 2021)`, 
                            age0_4$mnemonic,
                            age0_4[4:15] + age_5_9[4:15] + age_10_14[4:15] + age_15[4:15] + age_16[4:15])) %>%
  clean_names() %>%
  rename(la_name = 1, new_la_code = 2) %>%
  rename_at(vars(x2009:x2020), ~str_replace(., "x", "mid_")) 

pop_long <- pop_wide %>%
  pivot_longer(cols = mid_2009:mid_2020, names_to = "time_period", values_to = "population_0_16") %>%
  mutate(time_period = parse_number(time_period) + 1)

# Add 2021-2 data
# Can add together West and North Northamptonshire using this data (so may not need LAD for 2022)

age_0_4_22 <- read_csv("data/child-population/2021/popest-0-4.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2,
    mid_2021 = 3
  )

age_5_9_22 <- read_csv("data/child-population/2021/popest-5-9.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2,
    mid_2021 = 3
  )

age_10_14_22 <- read_csv("data/child-population/2021/popest-10-14.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2,
    mid_2021 = 3
  )

age_15_22 <- read_csv("data/child-population/2021/popest-15.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2,
    mid_2021 = 3
  )

age_16_22 <- read_csv("data/child-population/2021/popest-16.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2,
    mid_2021 = 3
  )

pop_0_16_22 <- bind_rows(age_0_4_22, age_5_9_22, age_10_14_22, age_15_22, age_16_22) %>%
  group_by(la_name, new_la_code) %>%
  summarise(population_0_16 = sum(mid_2021)) %>%
  arrange(new_la_code) %>%
  mutate(time_period = 2022, .before = population_0_16)

anti_join(pop_long, pop_0_16_22, by = c("la_name", "new_la_code")) %>% print(n = 100)
anti_join(pop_0_16_22, pop_long, by = c("la_name", "new_la_code"))

# Dorset and Northamptonshire numbers not included - add at end after fixing previous years



# original source population data cannot be used for Dorset, Poole, Bournemouth, Northamptonshire
# therefore, population estimates from district level should be used

# Child population - at LAD level
ladpop0_4 <- read_csv("data/child-population/popest-0-4.csv", skip = 9) %>%
  clean_names() %>%
  select(-x15) %>%
  drop_na() %>%
  rename(la = year) %>%
  filter(!la %in% c("Total"))

ladpop5_9 <- read_csv("data/child-population/popest-5-9.csv", skip = 9) %>%
  clean_names() %>%
  select(-x15) %>%
  drop_na() %>%
  rename(la = year) %>%
  filter(!la %in% c("Total"))

ladpop10_14 <- read_csv("data/child-population/popest-10-14.csv", skip = 9) %>%
  clean_names() %>%
  select(-x15) %>%
  drop_na() %>%
  rename(la = year) %>%
  filter(!la %in% c("Total"))

ladpop15 <- read_csv("data/child-population/popest-15.csv", skip = 9) %>%
  clean_names() %>%
  select(-x15) %>%
  drop_na() %>%
  rename(la = year) %>%
  filter(!la %in% c("Total"))

ladpop16 <- read_csv("data/child-population/popest-16.csv", skip = 9) %>%
  clean_names() %>%
  select(-x15) %>%
  drop_na() %>%
  rename(la = year) %>%
  filter(!la %in% c("Total"))


ladpop_0_16 <- as_tibble(cbind(ladpop16$la, ladpop0_4[2:14] + ladpop5_9[2:14] + ladpop10_14[2:14] + ladpop15[2:14] + ladpop16[2:14]))

ladpop_wide <- ladpop_0_16 %>%
  clean_names() %>%
  rename(la_name = 1)

ladpop_long <- ladpop_wide %>%
  pivot_longer(cols = mid_2008:mid_2020, names_to = "time_period", values_to = "population_0_16") %>%
  mutate(time_period = parse_number(time_period)+1)


# combine North Northamptonshire and West Northamptonshire into Northamptonshire
# Combine Poole and Bournemouth into Bournemouth, Christchurch and Poole

mergers_pop <- ladpop_long %>%
  filter(la_name %in% c("Dorset", "Bournemouth, Christchurch and Poole", 
                        "North Northamptonshire", "West Northamptonshire"))

mergers_pop <- mergers_pop %>%
  mutate(la_name = case_when(la_name == "North Northamptonshire" ~ "Northamptonshire",
                             la_name == "West Northamptonshire" ~ "Northamptonshire",
                             TRUE ~ la_name)) %>%
  group_by(la_name, time_period) %>%
  summarise(la_name = first(la_name), time_period = first(time_period), population_0_16 = sum(population_0_16)) %>%
  ungroup()

# add Dorset and Northamptonshire combined data to pop_long
mergers_pop <- mergers_pop %>%
  mutate(
    # Bournemouth, CC, Poole = E06000058
    # Northamptonshire = E10000021
    # Dorset = E10000009
    new_la_code = case_when(la_name == "Bournemouth, Christchurch and Poole" ~ "E06000058",
                            la_name == "Northamptonshire" ~ "E10000021",
                            la_name == "Dorset" ~ "E10000009"), .before = la_name
  ) 
pop_long <- bind_rows(pop_long, mergers_pop)


anti_join(pop_long, pop_0_16_22, by = c("la_name", "new_la_code")) %>% print(n = 100)
anti_join(pop_0_16_22, pop_long, by = c("la_name", "new_la_code"))

# In 2022: Dorset already present under both codes
# Combine both Northamptonshire
# B, C and P present under both

pop_0_16_22 <- pop_0_16_22 %>%
  ungroup() %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name)
  ) %>%
  group_by(new_la_code, la_name) %>%
  summarise(time_period = 2022, population_0_16 = sum(population_0_16)) %>%
  ungroup() %>%
  mutate(
    # Bournemouth, CC, Poole = E06000058
    # Northamptonshire = E10000021
    # Dorset = E10000009
    new_la_code = case_when(la_name == "Bournemouth, Christchurch and Poole" ~ "E06000058",
                            la_name == "Dorset" ~ "E10000009",
                            TRUE ~ new_la_code), 
    .before = la_name
  ) 


# Add 2022 data for population
pop_long <- bind_rows(pop_long, pop_0_16_22)



# Combine Bournemouth, Christchurch, and Poole in CIN data
cin_dat <- cin_dat %>%
  mutate(la_name = case_when(la_name == "Poole" ~ "Bournemouth, Christchurch and Poole",
                             la_name == "Bournemouth" ~ "Bournemouth, Christchurch and Poole",
                             TRUE ~ la_name),
         new_la_code = case_when(la_name == "Bournemouth, Christchurch and Poole" ~ "E06000058",
                                 TRUE ~ new_la_code)
  ) %>%
  group_by(new_la_code, time_period) %>%
  summarise(old_la_code = first(old_la_code),
            la_name = first(la_name),
            anypoint_episodes = sum(anypoint_episodes),
            started_episodes = sum(started_episodes),
            at31_episodes = sum(at31_episodes),
            referrals_n = sum(referrals_n),
            rereferrals_n = sum(rereferrals_n),
            referrals_nfa_n = sum(referrals_nfa_n),
            referrals_nin_n = sum(referrals_nin_n),
            children_referred_n = sum(children_referred_n),
            children_rereferred_n = sum(children_rereferred_n),
            pc_referrals_rereferral = (rereferrals_n / referrals_n) * 100,
            pc_referalls_nfa = (referrals_nfa_n / referrals_n) * 100,
            pc_referrals_nin = (referrals_nin_n / referrals_n) * 100,
            pc_children_rereferrred = (children_rereferred_n / children_referred_n) * 100,
            send_statem_n = sum(send_statem_n),
            ehcp_n = sum(ehcp_n),
            sendst_or_ehcp_n = sum(sendst_or_ehcp_n)
            ) 



# merge with NOMIS population data (will be missing Dorset and Northamptonshire)
# Replace Dorset's code with its old one
cin_dat <- cin_dat %>%
  mutate(new_la_code = case_when(new_la_code == "E06000059" ~ "E10000009",
                                 TRUE ~ new_la_code)) %>%
  ungroup()

anti_join(cin_dat, pop_long, by = c("time_period", "new_la_code"))

cin_dat <- left_join(cin_dat, pop_long %>% select(-la_name), by = c("time_period", "new_la_code"), multiple = "any")

# Duplicates checked - both identical

# check population
cin_dat %>%
  ggplot() +
  geom_line(aes(x = time_period, y = population_0_16, group = new_la_code))
# trends look strange for latest data. Redownload from NOMIS for all years, in case of change in methodology


age_0_4_09_21 <- read_csv("data/child-population/2009-2021/popest-0-4.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2
  ) %>%
  rename_at(vars(`2009`:`2021`), ~paste0("mid_", seq(2009, 2021, 1)))

age_5_9_09_21 <- read_csv("data/child-population/2009-2021/popest-5-9.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2
  ) %>%
  rename_at(vars(`2009`:`2021`), ~paste0("mid_", seq(2009, 2021, 1)))

age_10_14_09_21 <- read_csv("data/child-population/2009-2021/popest-10-14.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2
  ) %>%
  rename_at(vars(`2009`:`2021`), ~paste0("mid_", seq(2009, 2021, 1)))

age_15_09_21 <- read_csv("data/child-population/2009-2021/popest-15.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2
  ) %>%
  rename_at(vars(`2009`:`2021`), ~paste0("mid_", seq(2009, 2021, 1)))

age_16_09_21 <- read_csv("data/child-population/2009-2021/popest-16.csv", skip = 5) %>%
  rename(
    la_name = 1,
    new_la_code = 2
  ) %>%
  rename_at(vars(`2009`:`2021`), ~paste0("mid_", seq(2009, 2021, 1)))


# Merge populations
new_pop_long <- bind_rows(age_0_4_09_21, age_5_9_09_21, age_10_14_09_21, age_15_09_21, age_16_09_21) %>%
  group_by(la_name, new_la_code) %>%
  summarise_at(vars(mid_2009:mid_2021), sum) %>%
  ungroup() %>%
  pivot_longer(cols = mid_2009:mid_2021, names_to = "time_period", values_to = "population_0_16") %>%
  mutate(time_period = parse_number(time_period) + 1)


cin_dat <- cin_dat %>% select(-population_0_16)
anti_join(cin_dat, new_pop_long %>% select(-la_name), by = c("new_la_code", "time_period")) %>% print(n=100)
anti_join(new_pop_long %>% filter(str_detect(new_la_code, "E")), cin_dat , by = c("new_la_code", "time_period")) %>% print(n=100)

# Merge West and North Northamptonshire and update codes. Dorset -> E10000009, Northamptonshire -> E10000021

new_pop_long <- new_pop_long %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000059" ~ "E10000009",
                            # Northamptonshire
                            new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name)
  ) %>%
  group_by(new_la_code, la_name, time_period) %>%
  summarise(population_0_16 = sum(population_0_16)) %>%
  ungroup()

# Add population

popproj <- left_join(cin_dat, new_pop_long %>% select(-la_name), by = c("new_la_code", "time_period")) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = population_0_16, group = new_la_code), size = 0.1) +
  ggtitle("Population estimates (before rebasing)") +
  scale_x_continuous(breaks = seq(2009, 2022, 1))


# ADMIN-BASED POPULATION ESTIMATES
ltla_utla_lu <- read_csv("data/ltla-utla-lookup/ltla-utla-lookup.csv")

admin_pop <- readxl::read_xlsx("data/child-population/population-estimates-by-admin/apbefromdpmfeb23.xlsx", sheet = 2)

admin_pop <- admin_pop %>%
  filter(age %in% 0:16) %>%
  group_by(ladcode21, laname_21, time) %>%
  summarise_at(vars(mean:upper), sum)

unique(admin_pop$ladcode21)

# remove wales
admin_pop <- admin_pop %>% 
  ungroup() %>%
  filter(str_detect(ladcode21, "E"))

anti_join(admin_pop, ltla_utla_lu %>% select(-LTLA21NM, -FID),
          by = c("ladcode21" = "LTLA21CD"))


admin_pop <- left_join(admin_pop, ltla_utla_lu %>% select(-LTLA21NM, -FID),
          by = c("ladcode21" = "LTLA21CD"))

admin_pop <- admin_pop %>%
  group_by(UTLA21CD, UTLA21NM, time) %>%
  summarise(
    population_0_16 = sum(mean),
    population_0_16_lower = sum(lower),
    population_0_16_upper = sum(upper)
  )

admin_pop <- admin_pop %>% ungroup()

pop_from_admin <-admin_pop %>%
  ggplot() +
  geom_line(aes(x = time, y = population_0_16, group = UTLA21CD), size = 0.1) +
  ggtitle("Population from admin data") +
  scale_x_continuous(breaks = seq(2011, 2022, 1))


popproj + pop_from_admin

# update names to be consistent for joining

admin_pop <- admin_pop %>%
  rename(
    new_la_code = UTLA21CD,
    la_name = UTLA21NM,
    time_period = time
  )

# Add 2010 ye from population projection estimates

new_pop_long_2010 <- new_pop_long %>%
  filter(time_period == 2010)

new_pop_long_2010 <- new_pop_long_2010 %>%
  filter(str_detect(new_la_code, "E"))

admin_pop <- bind_rows(admin_pop, new_pop_long_2010)


# Check for any mergers and add to existing dataset
anti_join(cin_dat, admin_pop %>% select(-la_name), by = c("new_la_code", "time_period")) %>% print(n=100)

# Update Dorset code and merge North and West Northamptonshire
admin_pop <- admin_pop %>%
  mutate(
    new_la_code = ifelse(new_la_code == "E06000059", "E10000009", new_la_code)
  )

admin_pop %>% filter(str_detect(la_name, "Northamptonshire")) %>% print(n=1000)

admin_pop <- admin_pop %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = ifelse(new_la_code == "E10000021", "Northamptonshire", la_name)
  )

admin_pop <- admin_pop %>%
  group_by(new_la_code, la_name, time_period) %>%
  summarise_at(vars(population_0_16:population_0_16_upper), sum)

# Get admin estimates population 5-16 as well (for SEND statistics)

admin_pop_5_16 <- readxl::read_xlsx("data/child-population/population-estimates-by-admin/apbefromdpmfeb23.xlsx", sheet = 2)

admin_pop_5_16 <- admin_pop_5_16 %>%
  filter(age %in% 5:16) %>%
  group_by(ladcode21, laname_21, time) %>%
  summarise_at(vars(mean:upper), sum)

admin_pop_5_16 <- admin_pop_5_16 %>% 
  ungroup() %>%
  filter(str_detect(ladcode21, "E"))

admin_pop_5_16 <- left_join(admin_pop_5_16, ltla_utla_lu %>% select(-LTLA21NM, -FID),
                       by = c("ladcode21" = "LTLA21CD"))

admin_pop_5_16 <- admin_pop_5_16 %>%
  group_by(UTLA21CD, UTLA21NM, time) %>%
  summarise(
    population_5_16 = sum(mean),
    population_5_16_lower = sum(lower),
    population_5_16_upper = sum(upper)
  )

admin_pop_5_16 <- admin_pop_5_16 %>% ungroup()


# update names to be consistent for joining

admin_pop_5_16 <- admin_pop_5_16 %>%
  rename(
    new_la_code = UTLA21CD,
    la_name = UTLA21NM,
    time_period = time
  )

# Add 2010 ye from population projection estimates

pop_5_16_proj <- bind_rows(age_5_9_09_21, age_10_14_09_21, age_15_09_21, age_16_09_21) %>%
  group_by(la_name, new_la_code) %>%
  summarise_at(vars(mid_2009:mid_2021), sum) %>%
  ungroup() %>%
  pivot_longer(cols = mid_2009:mid_2021, names_to = "time_period", values_to = "population_5_16") %>%
  mutate(time_period = parse_number(time_period) + 1)

pop_5_16_proj <- pop_5_16_proj %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000059" ~ "E10000009",
                            # Northamptonshire
                            new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name)
  ) %>%
  group_by(new_la_code, la_name, time_period) %>%
  summarise(population_5_16 = sum(population_5_16)) %>%
  ungroup()


pop_5_16_proj_2010 <- pop_5_16_proj %>%
  filter(time_period == 2010)

pop_5_16_proj_2010 <- pop_5_16_proj_2010 %>%
  filter(str_detect(new_la_code, "E"))

admin_pop_5_16 <- bind_rows(admin_pop_5_16, pop_5_16_proj_2010)


# Check for any mergers and add to existing dataset
anti_join(cin_dat, admin_pop_5_16 %>% select(-la_name), by = c("new_la_code", "time_period")) %>% print(n=100)

# Update Dorset code and merge North and West Northamptonshire
admin_pop_5_16 <- admin_pop_5_16 %>%
  mutate(
    new_la_code = ifelse(new_la_code == "E06000059", "E10000009", new_la_code)
  )

admin_pop_5_16 %>% filter(str_detect(la_name, "Northamptonshire")) %>% print(n=1000)

admin_pop_5_16 <- admin_pop_5_16 %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = ifelse(new_la_code == "E10000021", "Northamptonshire", la_name)
  )

admin_pop_5_16 <- admin_pop_5_16 %>%
  group_by(new_la_code, la_name, time_period) %>%
  summarise_at(vars(population_5_16:population_5_16_upper), sum)

admin_pop_5_16 <- admin_pop_5_16 %>% ungroup()

anti_join(admin_pop, admin_pop_5_16, by = c("new_la_code", "la_name", "time_period"))

admin_pop <- left_join(admin_pop, admin_pop_5_16, by = c("new_la_code", "la_name", "time_period"))

admin_pop <- admin_pop %>% ungroup()

# * Population estimates before 2022 may not have been retrospectively updated yet
# - replaced with estimates from admin data and dynamic modelling
cin_dat <- left_join(cin_dat, admin_pop %>% select(-la_name), by = c("new_la_code", "time_period")) 

cin_dat %>%
  filter(is.na(population_0_16))

cin_dat %>%
  filter(is.na(population_5_16))

# missing time period of population for one LA

# calculate CIN episode rates per 10,000 population
# referral rate per 10,000 
# referrals non-re-referred per 10,000
# SEND statements per 10,000 age 5 -16
# EHCP statements per 10,000 age 5 -16
# SEND or EHCP per 10,000 age 5 -16
cin_dat <- cin_dat %>%
  mutate(
    any_cin_ep_rate_10000 = (anypoint_episodes / population_0_16)*10000,
    start_cin_ep_rate_10000 = (started_episodes / population_0_16)*10000,
    at31_cin_ep_rate_10000 = (at31_episodes / population_0_16)*10000,
    referrals_rate_10000 = (referrals_n / population_0_16)*10000,
    children_referred_rate_10000 = (children_referred_n / population_0_16)*10000,
    referrals_not_reref_rate_10000 = ( (referrals_n - rereferrals_n) / population_0_16)*10000,
    send_statem_rate_10000 = (send_statem_n / population_5_16)*10000,
    ehcp_rate_10000 = (ehcp_n / population_5_16)*10000,
    send_or_ehcp_rate_10000 = (sendst_or_ehcp_n / population_5_16)*10000,
  )




################ RELATIVE LOW INCOME
rel_pov_0_16 <- read_csv("data/low-income/children-rel-low-inc-utla-2015-21.csv", skip = 8) %>%
  janitor::clean_names() %>%
  select(year, x2014_15, x2015_16, x2016_17, x2017_18, x2018_19, x2019_20, x2020_21_p) %>%
  drop_na() %>%
  rename(la_name = year) %>%
  mutate_at(vars(x2014_15:x2020_21_p), ~parse_number(.))

rel_pov_wide <- rel_pov_0_16 %>%
  rename(ye2015 = 2, ye2016 = 3, ye2017 = 4, ye2018 = 5, ye2019 = 6, ye2020=7, ye2021=8)

rel_pov_long <- rel_pov_wide %>%
  pivot_longer(cols = ye2015:ye2021, names_to = "year_ending", values_to = "rel_pov_chil") %>%
  mutate(time_period = parse_number(year_ending), .before = rel_pov_chil) %>%
  select(-year_ending)

rel_pov_long

# Update data with 2022
rel_pov_long_0_4 <- read_csv("data/low-income/children-relative-low-income-0-4-2015-22-code.csv", skip = 9) %>%
  clean_names() %>%
  select(where(~!all(is.na(.)))) %>%
  drop_na() %>%
  pivot_longer(-1, names_to = "time_period", values_to = "n_poverty") %>%
  mutate(time_period = as.numeric(str_extract(time_period, "[2][0][0-9][0-9]")) + 1,
         n_poverty = parse_number(n_poverty)) %>%
  rename(new_la_code = year)

rel_pov_long_5_10 <- read_csv("data/low-income/children-relative-low-income-5-10-2015-22-code.csv", skip = 9) %>%
  clean_names() %>%
  select(where(~!all(is.na(.)))) %>%
  drop_na() %>%
  pivot_longer(-1, names_to = "time_period", values_to = "n_poverty") %>%
  mutate(time_period = as.numeric(str_extract(time_period, "[2][0][0-9][0-9]")) + 1,
         n_poverty = parse_number(n_poverty)) %>%
  rename(new_la_code = year)

rel_pov_long_11_15 <- read_csv("data/low-income/children-relative-low-income-11-15-2015-22-code.csv", skip = 9) %>%
  clean_names() %>%
  select(where(~!all(is.na(.)))) %>%
  drop_na() %>%
  pivot_longer(-1, names_to = "time_period", values_to = "n_poverty") %>%
  mutate(time_period = as.numeric(str_extract(time_period, "[2][0][0-9][0-9]")) + 1,
         n_poverty = parse_number(n_poverty)) %>%
  rename(new_la_code = year)

rel_pov_long_16 <- read_csv("data/low-income/children-relative-low-income-16-2015-22-code.csv", skip = 9) %>%
  clean_names() %>%
  select(where(~!all(is.na(.)))) %>%
  drop_na() %>%
  pivot_longer(-1, names_to = "time_period", values_to = "n_poverty") %>%
  mutate(time_period = as.numeric(str_extract(time_period, "[2][0][0-9][0-9]")) + 1,
         n_poverty = parse_number(n_poverty)) %>%
  rename(new_la_code = year)

rel_pov_long <- bind_rows(rel_pov_long_0_4, rel_pov_long_5_10, rel_pov_long_11_15, rel_pov_long_16) %>%
  group_by(new_la_code, time_period) %>%
  summarise(n_poverty = sum(n_poverty))

rel_pov_long %>%
  filter(n_poverty < 1e6) %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = n_poverty), linewidth = 0.1)


# add population data and LA names
anti_join(rel_pov_long, admin_pop, by = c("new_la_code", "time_period")) %>%
  filter(str_detect(new_la_code, "E"))


# Update code for Dorset and merge Northamptonshires for 2022
rel_pov_long <- rel_pov_long %>%
  ungroup() %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000059" ~ "E10000009",
                            new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code)
  ) %>%
  group_by(new_la_code, time_period) %>%
  summarise(n_poverty = sum(n_poverty)) %>%
  ungroup()


rel_pov_long <- left_join(rel_pov_long, admin_pop, by = c("new_la_code", "time_period"), multiple = "any") %>%
  relocate(la_name, .before = time_period) %>%
  mutate(rel_pov_rate100 = (n_poverty / population_0_16) * 100) %>%
  rename(rel_pov_chil = n_poverty)

rel_pov_long %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = rel_pov_rate100), linewidth = 0.1)



# add population data and la_codes
# legacy from pre 2022 data
# anti_join(rel_pov_long %>% mutate(la_join = str_remove_all(str_remove_all(tolower(la_name), " "), "[0-9]") ), 
#           pop_long  %>% mutate(la_join = str_remove_all(str_remove_all(tolower(la_name), " "), "[0-9]") ), 
#           by = c("la_join", "time_period"))
# 
# rel_pov_long <- left_join(rel_pov_long %>% mutate(la_join = str_remove_all(str_remove_all(tolower(la_name), " "), "[0-9]") ) %>% select(-la_name), 
#                           pop_long  %>% mutate(la_join = str_remove_all(str_remove_all(tolower(la_name), " "), "[0-9]") ), 
#                           by = c("la_join", "time_period")) %>%
#   relocate(new_la_code, la_name, .before = time_period) %>%
#   select(-la_join) %>%
#   mutate(
#     rel_pov_rate100 = (rel_pov_chil / population_0_16) * 100
#   )





# Make la codes consistent over time for Gateshead and Northumberland

# combine any bournemouth and poole


anti_join(cin_dat, 
          cla_10_22 %>% select(-la_name, -old_la_code), 
          by = c("new_la_code", "time_period")) 

anti_join(cla_10_22, 
          cin_dat %>% select(-la_name, -old_la_code), 
          by = c("new_la_code", "time_period")) 

# Isles of Scilly just from missing data (censored)

# change inconsistent LA codes to be consistent all years
cla_10_22 <- cla_10_22 %>%
  mutate(new_la_code = case_when(la_name == "Gateshead" ~ "E08000037",
                                 la_name == "Northumberland" ~ "E06000057",
                                 la_name == "Buckinghamshire" ~ "E06000060",
                                 la_name == "Dorset" ~ "E10000009",
                                 TRUE ~ new_la_code))

cla_10_22 <- cla_10_22 %>%
  mutate(la_name    = case_when(la_name == "Bournemouth" ~ "Bournemouth, Christchurch and Poole",
                                la_name == "Poole" ~ "Bournemouth, Christchurch and Poole",
                                la_name == "Herefordshire, County of" ~ "Herefordshire",
                                la_name == "Kingston Upon Hull, City of" ~ "Kingston upon Hull, City of",
                                la_name == "County Durham" ~ "Durham",
                                TRUE ~ la_name),
         new_la_code = case_when(la_name == "Bournemouth" ~ "E06000058",
                                 la_name == "Poole" ~ "E06000058",
                                 la_name == "Bournemouth, Christchurch and Poole" ~ "E06000058",
                                 TRUE ~ new_la_code)
  ) %>%
  group_by(time_period, new_la_code, la_name) %>%
  summarise(
    old_la_code = first(old_la_code),
    at31_cla = sum(at31_cla),
    cla_start = sum(cla_start),
    cla_cease = sum(cla_cease),
  ) %>%
  ungroup() 

# In CLA data, combine West and North Northamptonshire
cla_10_22 <- cla_10_22 %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code),
    la_name = case_when(new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name)
  ) %>%
  group_by(time_period, new_la_code, la_name) %>%
  summarise_at(vars(old_la_code:cla_cease), sum)


# should only be Isles of Scilly from early years
anti_join(cin_dat, 
          cla_10_22 %>% select(-la_name, -old_la_code), 
          by = c("new_la_code", "time_period")) 



merged_data <- left_join(cin_dat, 
                         cla_10_22 %>% select(-la_name, -old_la_code), 
                         by = c("new_la_code", "time_period")) 

anti_join(merged_data %>% filter(time_period > 2014), rel_pov_long, by = c("new_la_code", "time_period"))

anti_join(merged_data %>% filter(time_period > 2014), rel_pov_long, by = c("new_la_code", "time_period"))
anti_join(rel_pov_long, merged_data %>% filter(time_period > 2014), by = c("new_la_code", "time_period")) # missing Scotland/NI


# Add relative child poverty numbers from 2011, 2012, 2013, and 2014

# 2014
lowinc_2014 <- readxl::read_xlsx("data/low-income/pre-2015/low-income-2014.xlsx", sheet = "Children", skip = 2) %>%
  clean_names()

lowinc_2014 <- lowinc_2014 %>%
  select(utla11cd = x1, utla11nm = x3, pov_0_4 = x0_4_10, pov_5_10 = x5_10_11, pov_11_15 = x11_15_12, pov_16_19 = x16_19_13) %>%
  filter(!is.na(utla11nm))

lowinc_2014 <- lowinc_2014 %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2014, .before = utla11cd)

# 2013
lowinc_2013 <- readxl::read_xls("data/low-income/pre-2015/low-income-2013.xls", sheet = "Children", skip = 2) %>%
  clean_names()

lowinc_2013 <- lowinc_2013 %>%
  select(utla11cd = x1, utla11nm = x3, pov_0_4 = x0_4_10, pov_5_10 = x5_10_11, pov_11_15 = x11_15_12, pov_16_19 = x16_19_13) %>%
  filter(!is.na(utla11nm))

lowinc_2013 <- lowinc_2013 %>%
  mutate_at(vars(pov_0_4:pov_16_19), ~as.numeric(.)) %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2013, .before = utla11cd)

# 2012
lowinc_2012 <- readxl::read_xls("data/low-income/pre-2015/low-income-2012.xls", sheet = "Children (rounded)", skip = 3) %>%
  clean_names()

lowinc_2012 <- lowinc_2012 %>%
  select(utla11cd = x1, utla11nm = x3, pov_0_4 = x0_4_10, pov_5_10 = x5_10_11, pov_11_15 = x11_15_12, pov_16_19 = x16_19_13) %>%
  filter(!is.na(utla11nm))

lowinc_2012 <- lowinc_2012 %>%
  mutate_at(vars(pov_0_4:pov_16_19), ~as.numeric(.)) %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2012, .before = utla11cd)

# 2011
lowinc_2011 <- readxl::read_xls("data/low-income/pre-2015/low-income-2011.xls", sheet = "Children", skip = 2) %>%
  clean_names()

lowinc_2011 <- lowinc_2011 %>%
  select(utla11cd = x1, utla11nm = x3, pov_0_4 = x0_4_10, pov_5_10 = x5_10_11, pov_11_15 = x11_15_12, pov_16_19 = x16_19_13) %>%
  filter(!is.na(utla11nm))

lowinc_2011 <- lowinc_2011 %>%
  mutate_at(vars(pov_0_4:pov_16_19), ~as.numeric(.)) %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2011, .before = utla11cd)

rel_pov_long

# fix codes between pre 2015 data and post-
anti_join(lowinc_2014, rel_pov_long %>% filter(time_period == 2015), by = c("utla11cd" = "new_la_code"))
anti_join(rel_pov_long %>% filter(time_period == 2015), lowinc_2014, by = c("new_la_code" = "utla11cd"))

rel_pov_long <- rel_pov_long %>%
  mutate(
    new_la_code = case_when(new_la_code == "E10000009" ~ "E06000059",
                            TRUE ~ new_la_code)
  )


# Update Northumberland, Buckinhamshire and Gateshead codes
# Merge and aggregate Bournemouth and Poole 

lowinc_2014 <- lowinc_2014 %>%
  mutate(
    utla11cd = case_when(# Gateshead
      utla11cd == "E08000020" ~ "E08000037",
      # Northumberland
      utla11cd == "E06000048" ~ "E06000057",
      # Dorset
      utla11cd == "E10000009" ~ "E06000059",
      # Buckinghamshire
      utla11cd == "E10000002" ~ "E06000060",
      # Bournemouth and Poole
      utla11cd == "E06000028" ~ "E06000058",
      utla11cd == "E06000029" ~ "E06000058",
      TRUE ~ utla11cd
    ),
    utla11nm = case_when(utla11cd == "E06000058" ~ "Bournemouth, Christchurch and Poole",
                         TRUE ~ utla11nm)
  ) %>%
  group_by(utla11cd, utla11nm, time_period) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup()

anti_join(lowinc_2013, rel_pov_long %>% filter(time_period == 2015), by = c("utla11cd" = "new_la_code"))
anti_join(rel_pov_long %>% filter(time_period == 2015), lowinc_2013, by = c("new_la_code" = "utla11cd"))

lowinc_2013 <- lowinc_2013 %>%
  mutate(
    utla11cd = case_when(# Gateshead
      utla11cd == "E08000020" ~ "E08000037",
      # Northumberland
      utla11cd == "E06000048" ~ "E06000057",
      # Dorset
      utla11cd == "E10000009" ~ "E06000059",
      # Buckinghamshire
      utla11cd == "E10000002" ~ "E06000060",
      # Bournemouth and Poole
      utla11cd == "E06000028" ~ "E06000058",
      utla11cd == "E06000029" ~ "E06000058",
      TRUE ~ utla11cd
    ),
    utla11nm = case_when(utla11cd == "E06000058" ~ "Bournemouth, Christchurch and Poole",
                         TRUE ~ utla11nm)
  ) %>%
  group_by(utla11cd, utla11nm, time_period) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup()

anti_join(lowinc_2012, rel_pov_long %>% filter(time_period == 2015), by = c("utla11cd" = "new_la_code"))
anti_join(rel_pov_long %>% filter(time_period == 2015), lowinc_2012, by = c("new_la_code" = "utla11cd"))

lowinc_2012 <- lowinc_2012 %>%
  mutate(
    utla11cd = case_when(# Gateshead
      utla11cd == "E08000020" ~ "E08000037",
      # Northumberland
      utla11cd == "E06000048" ~ "E06000057",
      # Dorset
      utla11cd == "E10000009" ~ "E06000059",
      # Buckinghamshire
      utla11cd == "E10000002" ~ "E06000060",
      # Bournemouth and Poole
      utla11cd == "E06000028" ~ "E06000058",
      utla11cd == "E06000029" ~ "E06000058",
      TRUE ~ utla11cd
    ),
    utla11nm = case_when(utla11cd == "E06000058" ~ "Bournemouth, Christchurch and Poole",
                         TRUE ~ utla11nm)
  ) %>%
  group_by(utla11cd, utla11nm, time_period) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup()

anti_join(lowinc_2011, rel_pov_long %>% filter(time_period == 2015), by = c("utla11cd" = "new_la_code"))
anti_join(rel_pov_long %>% filter(time_period == 2015), lowinc_2011, by = c("new_la_code" = "utla11cd"))

lowinc_2011 <- lowinc_2011 %>%
  mutate(
    utla11cd = case_when(# Gateshead
      utla11cd == "E08000020" ~ "E08000037",
      # Northumberland
      utla11cd == "E06000048" ~ "E06000057",
      # Dorset
      utla11cd == "E10000009" ~ "E06000059",
      # Buckinghamshire
      utla11cd == "E10000002" ~ "E06000060",
      # Bournemouth and Poole
      utla11cd == "E06000028" ~ "E06000058",
      utla11cd == "E06000029" ~ "E06000058",
      TRUE ~ utla11cd
    ),
    utla11nm = case_when(utla11cd == "E06000058" ~ "Bournemouth, Christchurch and Poole",
                         TRUE ~ utla11nm)
  ) %>%
  group_by(utla11cd, utla11nm, time_period) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup()

# Merge pre-2015 and post-2015 data, re-add population
rel_pov_long

lowinc_2011_m <- lowinc_2011 %>%
  select(new_la_code = utla11cd, la_name = utla11nm, time_period, rel_pov_chil = pov_chil)
lowinc_2012_m <- lowinc_2012 %>%
  select(new_la_code = utla11cd, la_name = utla11nm, time_period, rel_pov_chil = pov_chil)
lowinc_2013_m <- lowinc_2013 %>%
  select(new_la_code = utla11cd, la_name = utla11nm, time_period, rel_pov_chil = pov_chil)
lowinc_2014_m <- lowinc_2014 %>%
  select(new_la_code = utla11cd, la_name = utla11nm, time_period, rel_pov_chil = pov_chil)

rel_pov_long <- rel_pov_long %>%
  select(-population_0_16, -rel_pov_rate100)

rel_pov_long <- rel_pov_long %>% select(-population_0_16_lower:-population_5_16_upper)

rel_pov_long_merged <- bind_rows(rel_pov_long, lowinc_2014_m, lowinc_2013_m, lowinc_2012_m, lowinc_2011_m) %>% arrange(new_la_code, time_period)

rel_pov_long_merged

# join population data
anti_join(rel_pov_long_merged, admin_pop, by = c("new_la_code", "time_period")) %>%
  group_by(new_la_code, la_name) %>%
  slice(1) %>% .$la_name %>% unique(.)

rel_pov_long_merged <- rel_pov_long_merged %>%
  mutate(new_la_code = case_when(
    new_la_code == "E06000059" ~ "E10000009",
    TRUE ~ new_la_code
  ))

rel_pov_long_merged <- left_join(rel_pov_long_merged, admin_pop %>% select(new_la_code, time_period, population_0_16), 
                                 by = c("new_la_code", "time_period"), multiple = "any") %>%
  mutate(
    rel_pov_rate100 = (rel_pov_chil / population_0_16)*100
  )


rel_pov_long <- rel_pov_long %>%
  mutate(new_la_code = case_when(
    new_la_code == "E06000059" ~ "E10000009",
    TRUE ~ new_la_code
  ))

rel_pov_long <- left_join(rel_pov_long, admin_pop %>% select(new_la_code, time_period, population_0_16), 
                          by = c("new_la_code", "time_period")) %>%
  mutate(
    rel_pov_rate100 = (rel_pov_chil / population_0_16)*100
  )

rel_pov_long %>%
  ggplot() +
  geom_line(aes(x = time_period, y = rel_pov_rate100, group = new_la_code), size = 0.2)

rel_pov_long_merged %>%
  ggplot() +
  geom_line(aes(x = time_period, y = rel_pov_rate100, group = new_la_code), size = 0.2)


# Clear Differences in Methodology between the two groups, so should be analysed seperately

# CLIF 2011-2016 methodology
# CLIF 2015-2021 methodology

lowinc_2015 <- readxl::read_xlsx("data/low-income/pre-2015/low-income-2015.xlsx", sheet = "Children", skip = 7) %>%
  clean_names()

lowinc_2015 <- lowinc_2015 %>%
  select(utla11cd = x1, utla11nm = x2, pov_0_4 = x0_4_8, pov_5_10 = x5_10_9, pov_11_15 = x11_15_10, pov_16_19 = x16_19_11) %>%
  filter(!is.na(utla11nm))

lowinc_2015 <- lowinc_2015 %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2015, .before = utla11cd)


# 2016 - in ltla so needs aggregating to utla
lowinc_2016 <- readxl::read_xlsx("data/low-income/pre-2015/low-income-2016.xlsx", sheet = "Children", skip = 7) %>%
  clean_names()

lowinc_2016 <- lowinc_2016 %>%
  select(ltla11cd = x1, ltla11nm = x2, pov_0_4 = x0_4_8, pov_5_10 = x5_10_9, pov_11_15 = x11_15_10, pov_16_19 = x16_19_11) %>%
  filter(!is.na(ltla11nm))

lowinc_2016 <- lowinc_2016 %>%
  mutate(pov_chil = rowSums(across(pov_0_4:pov_11_15))) %>%
  mutate(time_period = 2016, .before = ltla11cd)

# Aggregate 2016 to utla
ltla_utla_lu <- read_csv("data/ltla-utla-lookup/ltla-utla-lookup.csv") %>% clean_names()

anti_join(lowinc_2016, ltla_utla_lu, by = c("ltla11cd" = "ltla21cd"))

lowinc_2016 <- lowinc_2016 %>% 
  mutate(
    ltla11cd = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      ltla11cd == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      ltla11cd == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      ltla11cd == "E06000048" ~ "E06000057",
      # Mid Bedfordshire -> Central Bedfordshire
      ltla11cd == "E07000001" ~ "E06000056",
      # South Bedfordshire -> Central Bedfordshire
      ltla11cd == "E07000003" ~ "E06000056",
      # Ayesbury Vale (Buckinghamshire)
      ltla11cd == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      ltla11cd == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      ltla11cd == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      ltla11cd == "E07000007" ~ "E06000060",
      # Chester -> Chesire West and Chester
      ltla11cd == "E07000013" ~ "E06000050",
      # Congleton -> Chesire East
      ltla11cd == "E07000014" ~ "E06000049",
      # Crewe and Nantwich -> Chesire East
      ltla11cd == "E07000015" ~ "E06000049",
      # Ellesmere Port and Neston -> Chesire West and Chester
      ltla11cd == "E07000016" ~ "E06000050",
      # Macclesfield -> Chesire East
      ltla11cd == "E07000017" ~ "E06000049",
      # Vale Royal -> Cheshire West and Chester
      ltla11cd == "E07000018" ~ "E06000050",
      # Caradon -> Cornwall 
      ltla11cd == "E07000019" ~ "E06000052",
      # Carrick -> Cornwall 
      ltla11cd == "E07000020" ~ "E06000052",
      # Kerrier -> Cornwall
      ltla11cd == "E07000021" ~ "E06000052",
      # North Cornwall -> Cornwall
      ltla11cd == "E07000022" ~ "E06000052",
      # Penwith -> Cornwall
      ltla11cd == "E07000023" ~ "E06000052",
      # Restormel -> Cornwall
      ltla11cd == "E07000024" ~ "E06000052",
      # Isles of Scilly -> Isles of Scilly new
      ltla11cd == "E07000025" ~ "E06000053",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      ltla11cd == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      ltla11cd == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      ltla11cd == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      ltla11cd == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      ltla11cd == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      ltla11cd == "E07000053" ~ "E06000059",
      # Chester-le-street -> County Durham
      ltla11cd == "E07000054" ~ "E06000047",
      # Derwentside -> County Durham
      ltla11cd == "E07000055" ~ "E06000047",
      # Durham -> County Durham
      ltla11cd == "E07000056" ~ "E06000047",
      # Easington -> County Durham
      ltla11cd == "E07000057" ~ "E06000047",
      # Sedgefield -> County Durham
      ltla11cd == "E07000058" ~ "E06000047",
      # Teesdale -> County Durham
      ltla11cd == "E07000059" ~ "E06000047",
      # Wear Valley -> County Durham
      ltla11cd == "E07000060" ~ "E06000047",
      # East Hertfordshire -> East Hertfordshire
      ltla11cd == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      ltla11cd == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      ltla11cd == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      ltla11cd == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      ltla11cd == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      ltla11cd == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      ltla11cd == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      ltla11cd == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      ltla11cd == "E07000156" ~ "E06000061",
      # Alnwick -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000157" ~ "E06000057",
      # Berwick-upon-Tweed -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000158" ~ "E06000057",
      # Blyth Valley -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000159" ~ "E06000057",
      # Castle Morpeth -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000160" ~ "E06000057",
      # Tynedale -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000161" ~ "E06000057",
      # Wansbeck -> Northumberland -> Northumberland (new)
      ltla11cd == "E07000162" ~ "E06000057",
      # Bridgnorth -> Shropshire 
      ltla11cd == "E07000182" ~ "E06000051",
      # North Shropshire -> Shropshire
      ltla11cd == "E07000183" ~ "E06000051",
      # Oswestry -> Shropshire
      ltla11cd == "E07000184" ~ "E06000051",
      # Shrewsbury and Atcham -> Shropshire
      ltla11cd == "E07000185" ~ "E06000051",
      # South Shropshire -> Shropshire
      ltla11cd == "E07000186" ~ "E06000051",
      # Taunton Deane -> Somerset West and Taunton
      ltla11cd == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      ltla11cd == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      ltla11cd == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      ltla11cd == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      ltla11cd == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      ltla11cd == "E07000206" ~ "E07000244",
      # Kennet -> Wiltshire
      ltla11cd == "E07000230" ~ "E06000054",
      # North Wiltshire -> Wiltshire
      ltla11cd == "E07000231" ~ "E06000054",
      # Salisbury -> Wiltshire
      ltla11cd == "E07000232" ~ "E06000054",
      # West Wiltshire -> Wiltshire
      ltla11cd == "E07000233" ~ "E06000054",
      # Gateshead -> Gateshead (new)
      ltla11cd == "E08000020" ~ "E08000037",
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ ltla11cd
    )
  )

lowinc_2016 <- left_join(lowinc_2016, ltla_utla_lu %>% select(-fid, -ltla21nm), by = c("ltla11cd" = "ltla21cd"))

lowinc_2016 <- lowinc_2016 %>%
  filter(!is.na(utla21cd)) %>%
  filter(str_detect(utla21cd, "E")) %>%
  group_by(utla21cd, utla21nm) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup() %>%
  rename(utla11cd = utla21cd, utla11nm = utla21nm) %>%
  mutate(time_period = 2016, .before = pov_0_4)


clif_11_16 <- bind_rows(lowinc_2011, lowinc_2012, lowinc_2013, lowinc_2014, lowinc_2015, lowinc_2016)

anti_join(clif_11_16, admin_pop, by = c("utla11cd" = "new_la_code", "time_period"))

# update Buckinghamshire code and combine Bournemouth and Poole
# Update Dorset code 
# merge north and west northamptonshire

clif_11_16 <- clif_11_16 %>%
  mutate(
    utla11cd = case_when(
      utla11cd == "E06000059" ~ "E10000009",
      utla11cd == "E06000061" ~ "E10000021",
      utla11cd == "E06000062" ~ "E10000021",
      utla11cd == "E10000002" ~ "E06000060",
      utla11cd == "E06000028" ~ "E06000058",
      utla11cd == "E06000029" ~ "E06000058",
      TRUE ~ utla11cd
    ),
    utla11nm = case_when(
      utla11cd == "E06000058" ~ "Bournemouth, Christchurch and Poole",
      utla11cd == "E10000021" ~ "Northamptonshire",
      TRUE ~ utla11nm
    )
  ) %>%
  group_by(utla11cd, utla11nm, time_period) %>%
  summarise_at(vars(pov_0_4:pov_chil), sum) %>%
  ungroup()

clif_11_16 <- left_join(clif_11_16, admin_pop %>% select(new_la_code, time_period, population_0_16), 
                        by = c("utla11cd" = "new_la_code", "time_period"),
                        multiple = "any") %>%
  mutate(
    clif_11_16_cpov_rate = (pov_chil / population_0_16)*100
  )


# join to merged data

# remove duplicates
rel_pov_long <- rel_pov_long[!duplicated(rel_pov_long),] 


merged_data <- left_join(merged_data %>% ungroup(), rel_pov_long %>% select(-la_name, -population_0_16), by = c("new_la_code", "time_period"))

anti_join(merged_data %>% ungroup() %>% filter(time_period > 2010 & time_period < 2016), clif_11_16, by = c("new_la_code" = "utla11cd", "time_period"))

# change Dorset code
clif_11_16 <- clif_11_16 %>%
  mutate(utla11cd = case_when(
    utla11cd == "E06000059" ~ "E10000009",
    TRUE ~ utla11cd
  ))


merged_data <- left_join(merged_data %>% ungroup(), 
                         clif_11_16 %>% ungroup %>% select(utla11cd, time_period, clif1116_pov_chil = pov_chil, clif_11_16_cpov_rate), 
                         by = c("new_la_code" = "utla11cd", "time_period"))


ggplot(merged_data) +
  geom_line(aes(x = time_period, y = clif_11_16_cpov_rate, group = new_la_code), size = 0.2)


# Dorset is included twice with old and new code, current join will carry over full Dorset data



names(merged_data)

merged_data <- merged_data %>%
  relocate(population_0_16:population_5_16_upper, .before = anypoint_episodes) %>%
  mutate(at_31_cla_rate10000 = (at31_cla/population_0_16)*10000,
         cla_start_rate10000 = (cla_start/population_0_16)*10000,
         cla_cease_rate10000 = (cla_cease/population_0_16)*10000, .before = rel_pov_chil
  )




# Add Child Protection Plan Rates 2013-2021 - Updated to 2022


cpp_13_21 <- read_csv("data/cin-census/2020-21/characteristics-of-children-in-need_2021/data/d1_child_protection_plans_2013_to_2021.csv") 


unique(cpp_13_21$Category)
unique(cpp_13_21$CPP_At31)

names(cpp_13_21)
unique(cpp_13_21$time_period)

cpp_13_21 <- cpp_13_21 %>% 
  filter(geographic_level=="Local authority") %>%
  select(time_period, old_la_code, new_la_code, la_name, 
         # Only unique children starting and unique children at 31 are comparable before 2013
         cpp_start = CPP_child_start, 
         at31_cpp = CPP_At31) %>%
  mutate_at(vars(cpp_start:at31_cpp), ~parse_number(.))

cpp_13_21 

# Sum up Bournemouth and Poole to add to Bournemouth, Christchurch and Poole
cpp_13_21 <- cpp_13_21 %>%
  mutate(
    new_la_code = case_when(new_la_code %in% c("E06000028", "E06000029") ~ "E06000058", 
                            new_la_code == "E06000059" ~ "E10000009", 
                            TRUE ~ new_la_code ),
    old_la_code = ifelse(new_la_code == "E06000058", 839, old_la_code ),
    la_name = ifelse(new_la_code == "E06000058", "Bournemouth, Christchurch and Poole", la_name )
  ) %>%
  # Change Dorset old code
  mutate(old_la_code = ifelse(old_la_code == 835, 838, old_la_code )) %>%
  group_by(time_period, old_la_code, new_la_code, la_name) %>%
  summarise(cpp_start = sum(cpp_start),
            at31_cpp = sum(at31_cpp)) %>%
  ungroup() 


cpp_13_22 <- read_csv("data/cin-census/2021-22/data/d1_child_protection_plans_2013_to_2022.csv") 

unique(cpp_13_22$Category)
unique(cpp_13_22$CPP_At31)

names(cpp_13_22)
unique(cpp_13_22$time_period)

cpp_13_22 <- cpp_13_22 %>% 
  filter(geographic_level=="Local authority") %>%
  select(time_period, old_la_code, new_la_code, la_name, 
         # Only unique children starting and unique children at 31 are comparable before 2013
         cpp_start = CPP_child_start, 
         at31_cpp = CPP_At31) %>%
  mutate_at(vars(cpp_start:at31_cpp), ~parse_number(.))

cpp_13_22 

# Sum up Bournemouth and Poole to add to Bournemouth, Christchurch and Poole
# Merge North and West Northamptonshire
cpp_13_22 <- cpp_13_22 %>%
  mutate(
    new_la_code = case_when(new_la_code %in% c("E06000028", "E06000029") ~ "E06000058", 
                            new_la_code == "E06000059" ~ "E10000009", 
                            new_la_code == "E06000061" ~ "E10000021",
                            new_la_code == "E06000062" ~ "E10000021",
                            TRUE ~ new_la_code ),
    old_la_code = ifelse(new_la_code == "E06000058", 839, old_la_code ),
    la_name = case_when(new_la_code == "E06000058" ~ "Bournemouth, Christchurch and Poole", 
                        new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name )
  ) %>%
  # Change Dorset old code
  mutate(old_la_code = case_when(old_la_code == 835 ~ 838, 
                                 old_la_code %in% c(940, 941) ~ 928,
                                 TRUE ~ old_la_code)) %>%
  group_by(time_period, new_la_code, old_la_code, la_name) %>%
  summarise(cpp_start = sum(cpp_start),
            at31_cpp = sum(at31_cpp)) %>%
  ungroup() 




# Child protection plans 2012
cpp_12 <- read_csv(file = "data/cin-census/2011-12/sfr27-2012udv3/SFR27-2012ud.csv")

names(cpp_12)

cpp_12 <- cpp_12 %>%
  filter(geog_l == "LA") %>%
  select(old_la_code = geog_c, la_name = geog_n, cpp_start = CPPStart1112, at31_cpp = CPPAt310312) %>%
  mutate(time_period = 2012, .before = old_la_code)

anti_join(cpp_13_22, cpp_12 %>% select(-la_name), by = c("old_la_code") )

# Change Dorset (make new LA code) and Bournemouth/Poole (aggregate)
cpp_12 <- cpp_12 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code)
  ) %>%
  mutate(
    la_name = case_when(old_la_code == 839 ~ "Bournemouth, Christchurch and Poole",
                        TRUE ~ la_name)
  ) %>%
  mutate_at(vars(cpp_start, at31_cpp), ~parse_number(.)) %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise(cpp_start = sum(cpp_start),
            at31_cpp = sum(at31_cpp)
  ) %>%
  ungroup()

anti_join(cpp_13_22, cpp_12 %>% select(-la_name), by = c("old_la_code") )

cpp_12_22 <- bind_rows(cpp_13_22, cpp_12)

# CPP 2011
# Read data, get columns, check matches with cpp13_21, fix matches

cpp_11 <- read_csv(file = "data/cin-census/2010-11/osr26-2011ud.csv")


cpp_11 <- cpp_11 %>%
  filter(geog_l == "LA") %>%
  # Note: In 2011 the CPP "In" category has the same description as CPP start, so assumed to be the same
  select(old_la_code = geog_c, la_name = geog_n, cpp_start = CPPIn1011, at31_cpp = CPPAt310311) %>%
  mutate(time_period = 2011, .before = old_la_code)

anti_join(cpp_12_22, cpp_11 %>% select(-la_name), by = c("old_la_code") )

# Change Dorset (make new LA code) and Bournemouth/Poole (aggregate)
cpp_11 <- cpp_11 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code)
  ) %>%
  mutate(
    la_name = case_when(old_la_code == 839 ~ "Bournemouth, Christchurch and Poole",
                        TRUE ~ la_name)
  ) %>%
  mutate_at(vars(cpp_start, at31_cpp), ~as.numeric(.)) %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise(cpp_start = sum(cpp_start),
            at31_cpp = sum(at31_cpp)
  ) %>%
  ungroup()

anti_join(cpp_12_22, cpp_11 %>% select(-la_name), by = c("old_la_code") )
left_join(cpp_12_22, cpp_11 %>% select(-la_name), by = c("old_la_code") )

cpp_11_22 <- bind_rows(cpp_12_22, cpp_11)

# CPP 2009-2010
cpp_10 <- read_csv(file = "data/cin-census/2009-10/osr28-2010udv4/osr28-2010ud.csv")

cpp_10 <- cpp_10 %>%
  filter(geog_l == "LA") %>%
  # Note: In 2011 the CPP "In" category has the same description as CPP start, so assumed to be the same
  select(old_la_code = geog_c, la_name = geog_n, cpp_start = CPPIn0910, at31_cpp = CPPAt310310) %>%
  mutate(time_period = 2010, .before = old_la_code)

anti_join(cpp_11_22, cpp_10 %>% select(-la_name), by = c("old_la_code") ) 

cpp_10 <- cpp_10 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code)
  ) %>%
  mutate(
    la_name = case_when(old_la_code == 839 ~ "Bournemouth, Christchurch and Poole",
                        TRUE ~ la_name)
  ) %>%
  mutate_at(vars(cpp_start, at31_cpp), ~as.numeric(.)) %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise(cpp_start = sum(cpp_start),
            at31_cpp = sum(at31_cpp)
  ) %>%
  ungroup()

anti_join(cpp_11_22, cpp_10 %>% select(-la_name), by = c("old_la_code") )
left_join(cpp_11_22, cpp_10 %>% select(-la_name), by = c("old_la_code") )

cpp_10_22 <- bind_rows(cpp_11_22, cpp_10)

cpp_10_22

# Add new LA codes to old records of CPP

old_code_new_code_lu <- cpp_10_22 %>%
  group_by(old_la_code, new_la_code) %>% summarise() %>% drop_na()

cpp_10_12 <- cpp_10_22 %>% 
  filter(is.na(new_la_code)) %>% 
  select(-new_la_code) %>% 
  left_join(old_code_new_code_lu, by = "old_la_code") %>%
  relocate(new_la_code, .before = la_name)

cpp_10_22 <- bind_rows(cpp_10_12, cpp_10_22 %>% filter(!is.na(new_la_code))) %>% arrange(time_period)

# add to merged data
anti_join(merged_data, cpp_10_22 %>% select(-la_name, -old_la_code), by = c("new_la_code", "time_period"))


merged_data <- left_join(merged_data, cpp_10_22 %>% select(-la_name, -old_la_code), by = c("time_period", "new_la_code"))

merged_data <- merged_data %>%
  mutate(cpp_start_rate10000 = (cpp_start / population_0_16)*10000,
         at31_cpp_rate10000  = (at31_cpp  / population_0_16)*10000
  )


########## Section 251 expenditure per child data


# 2016 - 2021
# total_expenditure              |  Total gross expenditure (£ million)
s251_16_21 <- read_csv("data/s251/2020-21-la-and-school-expenditure/data/s251outturn_cypservices.csv") 
unique(s251_16_21$geographic_level)
unique(s251_16_21$category_of_expenditure)
unique(s251_16_21$description_of_expenditure)
unique(s251_16_21$description_of_expenditure[str_detect(s251_16_21$description_of_expenditure, "Total")])


s251_16_21 <- s251_16_21 %>%
  filter(geographic_level == "Local authority") %>%
  filter(category_of_expenditure %in% c("Children looked after", 
                                        "Family support servcies", # typo in DfE file
                                        "Other children and family services",
                                        "Safeguarding children and young peoples services",
                                        "Services for young people",
                                        "Sure Start and children aged under 5",
                                        "Youth justice")) %>%
  # Remove totals and CERA
  filter(!description_of_expenditure %in% c("4.0.1 Capital Expenditure from Revenue (CERA) (Childrens and young people services)",
                                            "3.1.11 Total Children Looked After",
                                            "3.4.6 Total Family Support Services",
                                            "3.3.4 Total Safeguarding Children and Young Peoples Services",
                                            "3.5.3 Total Services for young people",
                                            "3.0.5 Total Sure Start childrens centres and other spend on children under 5",
                                            "5.0.2 Total Children and Young Peoples Services Expenditure (excluding CERA)",
                                            "5.0.3 Total Children and Young Peoples Services Expenditure (including CERA)")) %>%
  select(time_period, old_la_code, new_la_code, la_name, category_of_expenditure, description_of_expenditure, total_expenditure) %>%
  group_by(time_period, old_la_code, new_la_code, la_name, category_of_expenditure) %>%
  summarise(total_expenditure = sum(total_expenditure)) %>%
  ungroup() %>%
  mutate(time_period = as.numeric(substr(as.character(time_period), start = 1, stop = 4))+1 ) %>%
  pivot_wider(names_from = category_of_expenditure, values_from = total_expenditure) %>%
  rename(gspend_cla = `Children looked after`,
         gspend_fss = `Family support servcies`,
         gspend_oth = `Other children and family services`,
         gspend_saf = `Safeguarding children and young peoples services`,
         gspend_yps = `Services for young people`,
         gspend_ss5 = `Sure Start and children aged under 5`,
         gspend_yjs = `Youth justice`) %>%
  mutate(
    gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)
  ) %>% 
  # Convert into £, rather than £millions
  mutate_at(vars(gspend_cla:gspend_noncla_nonsg), ~ . * 1e6)

min(s251_16_21$time_period)
max(s251_16_21$time_period)

# Update to 2022 data
s251_16_22 <- read_csv("data/s251/2021-22/la-and-school-expenditure_2021-22/data/s251_childrens_young_peoples_services_la_regional_national3.csv") %>%
  filter(geographic_level == "Local authority") %>% 
  filter(category_of_expenditure %in% c("Children looked after", 
                                        "Family support services", 
                                        "Other children and family services",
                                        "Safeguarding children and young peoples services",
                                        "Services for young people",
                                        "Sure Start and children aged under 5",
                                        "Youth justice")) %>%
  # Remove totals and CERA
  filter(!description_of_expenditure %in% c("4.0.1 Capital Expenditure from Revenue (CERA) (Childrens and young people services)",
                                            "3.1.11 Total Children Looked After",
                                            "3.4.6 Total Family Support Services",
                                            "3.3.4 Total Safeguarding Children and Young Peoples Services",
                                            "3.5.3 Total Services for young people",
                                            "3.0.5 Total Sure Start childrens centres and other spend on children under 5",
                                            "5.0.2 Total Children and Young Peoples Services Expenditure (excluding CERA)",
                                            "5.0.3 Total Children and Young Peoples Services Expenditure (including CERA)")) %>%
  select(time_period, old_la_code, new_la_code, la_name, category_of_expenditure, description_of_expenditure, total_expenditure) %>%
  # remove breakdown of fostering service and keep total (since it's not available for most LA/years)
  filter(!description_of_expenditure %in% c("3.1.2a Fostering services (excluding fees and allowances for LA foster carers)", 
                                            "3.1.2b Fostering services (fees and allowances for LA foster carers)")) %>%
  mutate(total_expenditure = parse_number(total_expenditure)) %>%
  group_by(time_period, old_la_code, new_la_code, la_name, category_of_expenditure) %>%
  summarise(total_expenditure = sum(total_expenditure)) %>%
  ungroup() %>%
  mutate(time_period = as.numeric(substr(as.character(time_period), start = 1, stop = 4))+1 ) %>%
  pivot_wider(names_from = category_of_expenditure, values_from = total_expenditure) %>%
  rename(gspend_cla = `Children looked after`,
         gspend_fss = `Family support services`,
         gspend_oth = `Other children and family services`,
         gspend_saf = `Safeguarding children and young peoples services`,
         gspend_yps = `Services for young people`,
         gspend_ss5 = `Sure Start and children aged under 5`,
         gspend_yjs = `Youth justice`) %>%
  mutate(
    gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)
  ) 


# 2015
s251_2015 <- readxl::read_xlsx("data/s251/2014-15/SR48-Tables.xlsx", sheet = "Table 6", skip = 5) %>%
  clean_names() %>%
  rename(old_la_code = 1, la_name = 2, 
         gspend_ss5 = 3,
         gspend_cla = 4,
         gspend_oth = 5,
         gspend_saf = 6,
         gspend_fss = 7,
         gspend_yps = 8,
         gspend_yjs = 9
  ) %>%
  select(-10, -11) %>%
  drop_na() %>%
  mutate(gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)) %>%
  mutate_at(vars(gspend_ss5:gspend_noncla_nonsg), ~. * 1000) %>%
  mutate(time_period = 2015, .before = old_la_code) %>%
  mutate(old_la_code = as.numeric(old_la_code))

# columns
# c("Sure Start Children's Centres and early years",
#   "Children looked after",
#   "Other children's and families services",
#   "Safeguarding children and young people's services",
#   "Family support services",
#   "Services for young people",
#   "Youth justice",
#   "Capital Expenditure from Revenue (CERA) (4) (Children's and young people's services)",
#   "Total spending by LA on children and young people's services (including CERA) (4)")


anti_join(s251_16_21, s251_2015, by = c("old_la_code"))

anti_join(s251_16_22, s251_2015, by = c("old_la_code"))

# Sum Bournemouth and Poole in 2016-2021 data and update codes
s251_16_21 <- s251_16_21 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    # change new_la_code for Bournemouth and Poole to be Bournemouth, Christchurch and Poole
    new_la_code = case_when(new_la_code %in% c("E06000028", "E06000029") ~ "E06000058", 
                            new_la_code == "E06000059" ~ "E10000009", 
                            TRUE ~ new_la_code ),
    # Change LA names to Bournemouth, Christchurch, and Poole for group summing
    la_name = ifelse(new_la_code == "E06000058", "Bournemouth, Christchurch and Poole", la_name )
  ) %>%
  group_by(time_period, old_la_code, new_la_code, la_name) %>%
  summarise_at(vars(gspend_cla:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()


# Sum Bournemouth and Poole in 2016-2022 data and update codes, merge Northamptonshires
s251_16_22 <- s251_16_22 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      # Northamptonshires
      old_la_code %in% c(940, 941) ~ 928,
      TRUE ~ old_la_code),
    # change new_la_code for Bournemouth and Poole to be Bournemouth, Christchurch and Poole
    new_la_code = case_when(new_la_code %in% c("E06000028", "E06000029") ~ "E06000058", 
                            new_la_code == "E06000059" ~ "E10000009", 
                            new_la_code %in% c("E06000061", "E06000062") ~ "E10000021",
                            TRUE ~ new_la_code ),
    # Change LA names to Bournemouth, Christchurch, and Poole for group summing
    la_name = case_when(new_la_code == "E06000058" ~ "Bournemouth, Christchurch and Poole", 
                        new_la_code == "E10000021" ~ "Northamptonshire",
                        TRUE ~ la_name )
  ) %>%
  group_by(time_period, old_la_code, new_la_code, la_name) %>%
  summarise_at(vars(gspend_cla:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()


# Sum Bournemouth and Poole in 2015 data and update codes for Dorset

s251_2015 <- s251_2015 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    la_name = ifelse(old_la_code == 839, "Bournemouth, Christchurch and Poole", la_name )
  )

# sum up Bournemouth, Christchurch and Poole
s251_2015 <- s251_2015 %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise_at(vars(gspend_ss5:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()


anti_join(s251_16_21, s251_2015, by = c("old_la_code"))
anti_join(s251_16_22, s251_2015, by = c("old_la_code"))

left_join(s251_16_21, s251_2015, by = c("old_la_code"))
left_join(s251_16_22, s251_2015, by = c("old_la_code"))

# add new_la_codes
s251_2015 <- left_join(s251_2015,
                       s251_16_22 %>% filter(time_period == 2016) %>% select(old_la_code, new_la_code), 
                       by = c("old_la_code")) %>%
  relocate(new_la_code, .before = la_name)

anti_join(s251_16_21, s251_2015, by = c("old_la_code", "new_la_code"))
anti_join(s251_16_22, s251_2015, by = c("old_la_code", "new_la_code"))

s251_15_22 <- bind_rows(s251_16_22, s251_2015)

# 2014

s251_14 <- readxl::read_xlsx("data/s251/2013-14/SR52-2014Tables.xlsx", sheet = "Table 6", skip = 4) %>%
  clean_names() %>%
  rename(old_la_code = 1, la_name = 2, 
         gspend_ss5 = 3,
         gspend_cla = 4,
         gspend_oth = 5,
         gspend_saf = 6,
         gspend_fss = 7,
         gspend_yps = 8,
         gspend_yjs = 9
  ) %>%
  select(-10, -11) %>%
  drop_na() %>%
  mutate(gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)) %>%
  mutate_at(vars(gspend_ss5:gspend_noncla_nonsg), ~. * 1000) %>%
  mutate(time_period = 2014, .before = old_la_code) %>%
  mutate(old_la_code = as.numeric(old_la_code))

anti_join(s251_15_22, s251_14, by = c("old_la_code"))

s251_14 <- s251_14 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    la_name = ifelse(old_la_code == 839, "Bournemouth, Christchurch and Poole", la_name )
  )

s251_14 <- s251_14 %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise_at(vars(gspend_ss5:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()

anti_join(s251_15_22, s251_14, by = c("old_la_code"))

s251_14 <- left_join(s251_14,
                     s251_15_22 %>% filter(time_period == 2016) %>% select(old_la_code, new_la_code), 
                     by = c("old_la_code")) %>%
  relocate(new_la_code, .before = la_name)

anti_join(s251_15_22, s251_14, by = c("old_la_code", "new_la_code"))

s251_14_22 <- bind_rows(s251_15_22, s251_14)

# Spending 2013
s251_13 <- readxl::read_xls("data/s251/2012-13/SR54-2013Tables.xls", sheet = "Table 6", skip = 4) %>%
  clean_names() %>%
  select(-1, -4, -5) %>%
  rename(old_la_code = 1, 
         la_name = 2, 
         gspend_ss5 = 3,
         gspend_cla = 4,
         gspend_oth = 5,
         gspend_saf = 6,
         gspend_fss = 7,
         gspend_yps = 8,
         gspend_yjs = 9
  ) %>%
  select(-10, -11) %>%
  drop_na() %>%
  mutate_at(vars(gspend_ss5:gspend_yjs), ~as.numeric(.)) %>%
  mutate(gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)) %>%
  mutate_at(vars(gspend_ss5:gspend_noncla_nonsg), ~. * 1000) %>%
  mutate(time_period = 2013, .before = old_la_code) %>%
  mutate(old_la_code = as.numeric(old_la_code))

anti_join(s251_14_22, s251_13, by = c("old_la_code"))

s251_13 <- s251_13 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    la_name = ifelse(old_la_code == 839, "Bournemouth, Christchurch and Poole", la_name )
  )

s251_13 <- s251_13 %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise_at(vars(gspend_ss5:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()

anti_join(s251_14_22, s251_13, by = c("old_la_code"))

s251_13 <- left_join(s251_13,
                     s251_14_22 %>% filter(time_period == 2016) %>% select(old_la_code, new_la_code), 
                     by = c("old_la_code")) %>%
  relocate(new_la_code, .before = la_name)

anti_join(s251_14_22, s251_13, by = c("old_la_code", "new_la_code"))

s251_13_22 <- bind_rows(s251_14_22, s251_13)

# Spend 2012
s251_12 <- readxl::read_xls("data/s251/2011-12/sfr07-2013v3.xls", sheet = "Table 6", skip = 4) %>%
  clean_names() %>%
  select(-1, -4, -5) %>%
  rename(old_la_code = 1, 
         la_name = 2, 
         gspend_yjs = 3,
         gspend_ss5 = 4,
         gspend_cla = 5,
         gspend_saf = 6,
         gspend_fss = 7,
         gspend_oth = 8,
         gspend_strat = 9,
         gspend_yps = 10
  ) %>%
  select(-capital_expenditure_from_revenue_cera_5_childrens_and_young_peoples_services, 
         -total_spending_by_la_on_education_children_and_young_peoples_services_including_cera_5) %>%
  drop_na() %>%
  mutate_at(vars(gspend_yjs:gspend_yps), ~as.numeric(.)) %>%
  mutate(gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)) %>%
  # Combine safety and strategy into safeguarding
  mutate(gspend_saf = gspend_saf + gspend_strat, .before = gspend_fss) %>%
  select(-gspend_strat) %>%
  mutate_at(vars(gspend_yjs:gspend_noncla_nonsg), ~. * 1000) %>%
  mutate(time_period = 2012, .before = old_la_code) %>%
  mutate(old_la_code = as.numeric(old_la_code)) %>%
  relocate(time_period, old_la_code, la_name, names(s251_13_22)[5:12])

anti_join(s251_13_22, s251_12, by = c("old_la_code"))

s251_12 <- s251_12 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    la_name = ifelse(old_la_code == 839, "Bournemouth, Christchurch and Poole", la_name )
  )

s251_12 <- s251_12 %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise_at(vars(gspend_cla:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()

anti_join(s251_13_22, s251_12, by = c("old_la_code"))

s251_12 <- left_join(s251_12,
                     s251_13_22 %>% filter(time_period == 2016) %>% select(old_la_code, new_la_code), 
                     by = c("old_la_code")) %>%
  relocate(new_la_code, .before = la_name)

anti_join(s251_13_22, s251_12, by = c("old_la_code", "new_la_code"))

s251_12_22 <- bind_rows(s251_13_22, s251_12)

# Spend 2011 
s251_11 <- readxl::read_xls("data/s251/2010-11/osr03-2012v3-3.xls", sheet = "Table 6", skip = 4) %>%
  clean_names() %>%
  select(-1, -4, -5) %>%
  rename(old_la_code = 1, 
         la_name = 2, 
         gspend_yjs = 3,
         gspend_ss5 = 4,
         gspend_cla = 5,
         gspend_saf = 6,
         gspend_fss = 7,
         gspend_oth = 8,
         gspend_strat = 9,
         gspend_com = 10,
         gspend_tru = 11,
         gspend_yps = 12
  ) %>%
  select(-capital_expenditure_from_revenue_cera_5_childrens_and_young_peoples_services, 
         -total_spending_by_la_on_education_children_and_young_peoples_services_including_cera_5) %>%
  drop_na() %>%
  mutate_at(vars(gspend_yjs:gspend_yps), ~as.numeric(.)) %>%
  # combine trust fund top ups into other
  mutate(gspend_oth  = rowSums(across(c(gspend_oth, gspend_tru)), na.rm = TRUE)) %>%
  select(-gspend_tru) %>%
  relocate(gspend_oth, .after = gspend_fss) %>%
  mutate(gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)), na.rm = TRUE)) %>%
  # Combine safety, strategy and commissioning into safeguarding
  mutate(gspend_saf = rowSums(across(c(gspend_saf, gspend_strat, gspend_com)), na.rm = TRUE), .before = gspend_fss) %>%
  select(-gspend_strat, -gspend_com) %>%
  mutate_at(vars(gspend_yjs:gspend_noncla_nonsg), ~. * 1000) %>%
  mutate(time_period = 2011, .before = old_la_code) %>%
  mutate(old_la_code = as.numeric(old_la_code)) %>%
  relocate(time_period, old_la_code, la_name, names(s251_13_22)[5:12])


anti_join(s251_12_22, s251_11, by = c("old_la_code"))

s251_11 <- s251_11 %>%
  mutate(
    old_la_code = case_when(
      # Bournemouth, Christchurch and Poole
      old_la_code == 837 ~ 839,
      old_la_code == 836 ~ 839,
      # Dorset
      old_la_code == 835 ~ 838,
      TRUE ~ old_la_code),
    la_name = ifelse(old_la_code == 839, "Bournemouth, Christchurch and Poole", la_name )
  )

s251_11 <- s251_11 %>%
  group_by(time_period, old_la_code, la_name) %>%
  summarise_at(vars(gspend_cla:gspend_noncla_nonsg), ~sum(.)) %>%
  ungroup()

anti_join(s251_12_22, s251_11, by = c("old_la_code"))

s251_11 <- left_join(s251_11,
                     s251_12_22 %>% filter(time_period == 2016) %>% select(old_la_code, new_la_code), 
                     by = c("old_la_code")) %>%
  relocate(new_la_code, .before = la_name)

anti_join(s251_12_22, s251_11, by = c("old_la_code", "new_la_code"))

s251_11_22 <- bind_rows(s251_12_22, s251_11)

# Spend 2010
s251_2010 <- readxl::read_xls("data/s251/2009-10/outturn detailed report table a1 2009 10.xls", skip = 3) %>%
  clean_names() %>%
  select(lea:s52_line_reference, total_expenditure = total_expenditure_k)

unique(s251_2010$s52_line_reference)
total_cats <- unique(s251_2010$s52_line_reference[str_detect(s251_2010$s52_line_reference, "Total|TOTAL")])[1:6]

s251_2010 <- s251_2010 %>%
  filter(s52_line_reference %in% total_cats)

# Add spend 2010 by taking Sure Start spend from RO data

ro10 <- readxl::read_xls("data/ro-laref/2009-10/special/RG.xls", skip = 8, sheet = 3)

unique(ro10$Class)
ro10 <- ro10 %>% clean_names()

ro10 <- ro10 %>% select(e_code, local_authority, sure_start_early_years_and_childcare_grant) %>% filter(!is.na(sure_start_early_years_and_childcare_grant))

# in 1000s

# Council Tax Receipts can be used as a lookup between 2009-10 "E-Codes" and ONS LTLA codes
# E-code lookup from CT receipts
ct_ecodes <- readxl::read_xlsx("data/ct-receipts/CTR_Table__live__-_Band_D_2022-23.xlsx", sheet = "inc PPs", skip = 2)
ct_ecodes <- ct_ecodes %>%
  clean_names() %>% select(code, ons_code, authority)

anti_join(ro10, ct_ecodes, by = c("e_code" = "code")) #%>% view() # only police authorities don't match
ro10 <- left_join(ro10, ct_ecodes %>% select(-authority), by = c("e_code" = "code")) %>%
  relocate(ons_code, .after = e_code)


# aggregate from LAD to UTLA using lookup
ro10


# need combined LTLA first, then UTLA lookup?

# mostly police authorities and 0s; need to update ONS codes for Buckinghamshire, Poole, Bournemouth and Dorset on LHS and aggregate
anti_join(ro10, ltla_utla_lu, by = c("ons_code" = "ltla21cd")) %>%
  anti_join(., ltla_utla_lu, by = c("ons_code" = "utla21cd")) #%>% view()

ltla_utla_lu <- ltla_utla_lu %>%
  add_row(fid = NA, ltla21cd = "E10000021", ltla21nm = "Northamptonshire", utla21cd = "E10000021", utla21nm = "Northamptonshire")

# Buckinghamshire -> E06000060
# Bournemouth UA, Poole UA -> E06000058
# Dorset -> E06000059
ro10 <- ro10 %>%
  mutate(
    ons_code = case_when(ons_code == "E10000002" ~ "E06000060",
                         ons_code == "E06000028" ~ "E06000058",
                         ons_code == "E06000029" ~ "E06000058",
                         ons_code == "E10000009" ~ "E06000059",
                         TRUE ~ ons_code)
  )


# Double join - if value not in ltla join take utla join
ro10 <- left_join(ro10, ltla_utla_lu %>% select(-ltla21nm, -fid), by = c("ons_code" = "ltla21cd")) 
ss5_2010 <- left_join(ro10, ltla_utla_lu %>% select(-ltla21cd, -ltla21nm, -fid, utla21cd2 = utla21cd), by = c("ons_code" = "utla21cd2"), keep = TRUE, suffix = c("", ".2")) %>%
  mutate(
    utla21cd = ifelse(is.na(utla21cd), utla21cd2, utla21cd),
    utla21nm = ifelse(is.na(utla21nm), utla21nm.2, utla21nm)
  ) %>% 
  # Remove duplicates
  group_by(e_code, ons_code, local_authority) %>% 
  slice(1) %>%
  select(-utla21cd2, -utla21nm.2) %>%
  # some mismatches but all mismatches are 0
  filter(!is.na(utla21cd)) %>% 
  ungroup() %>%
  group_by(utla21cd, utla21nm) %>%
  summarise(gspend_ss5 = sum(sure_start_early_years_and_childcare_grant)*1000 )



# 2010 spend is missing Sure Start and under 5s spend so added from RO RG outturns

## Add Sure Start and Early Years from Revenue Outcomes Special Grant for 2009-10,
## can go back to 2008-09 but CIN Census only goes to 2009-10

# find LEA to ONS lookups
s251_2010 


# Can use CLA2010 as lookup
lea_ons_code_lu <- cla_2010 %>% select(old_la_code, new_la_code)

s251_2010 <- s251_2010 %>%
  select(-line) %>% 
  pivot_wider(names_from = s52_line_reference, values_from = total_expenditure) %>%
  rename(
    gspend_yjs = `Total Youth Justice`,
    gspend_cla = `Total Children Looked After`,
    gspend_saf = `Total Children and Young People's Safety`,
    gspend_fss = `Total Family Support Services`,
    gspend_oth = `Total Other Children's and Families Services`,
    gspend_strat = `Total Children's Services Strategy` #combine with safeguarding like 2011
    # Missing Services for Young People - classed as 'other' if totals are about right
  )

# join sure start and under 5 spend and lookup

anti_join(s251_2010, lea_ons_code_lu, by = c("lea" = "old_la_code")) # missing Isles of Scilly 
s251_2010 <- left_join(s251_2010, lea_ons_code_lu, by = c("lea" = "old_la_code")) %>% 
  relocate(new_la_code, .after = lea) %>% 
  rename(old_la_code = lea,
         la_name = lea_name) %>%
  mutate(time_period = 2010, .before = old_la_code)

s251_2010 <- s251_2010 %>% 
  mutate(new_la_code = ifelse(la_name == "Isles Of Scilly", "E06000053", new_la_code))

anti_join(s251_2010, ss5_2010, by = c("new_la_code" = "utla21cd"))

# Update ss5_2010 utla21code for Gateshead, Buckinhamshire, Dorset, Poole, 
# Bournemouth, Northamptonshire, Nothumberland
# Combine Dorset and Poole in s251_2010 data

ss5_2010 <- ss5_2010 %>%
  mutate(
    utla21cd =    case_when(utla21cd    == "E08000037" ~ "E08000020",
                            utla21cd    == "E06000060" ~ "E10000002",
                            utla21cd    == "E06000059" ~ "E10000009",
                            utla21cd    == "E06000057" ~ "E06000048",
                            TRUE ~ utla21cd
    )
  )

s251_2010 <- s251_2010 %>%
  mutate(
    new_la_code = case_when( new_la_code == "E06000028" ~ "E06000058",
                             new_la_code == "E06000029" ~ "E06000058",
                             TRUE ~ new_la_code),
    la_name = ifelse(new_la_code == "E06000058", "Bournemouth, Christchurch and Poole", la_name)
  ) %>%
  group_by(time_period, new_la_code, la_name) %>%
  summarise_at(vars(gspend_yjs:gspend_strat), ~sum(.)) %>%
  ungroup() %>%
  mutate_at(vars(gspend_yjs:gspend_strat), ~ifelse(. == 0, NA, .)) 

# Stockton on Tees no table A1 data for 2010 - unknown reason
s251_2010 <- left_join(s251_2010, ss5_2010 %>% select(-utla21nm), by = c("new_la_code" = "utla21cd"))


# No Young Peoples Services spending
# Best match seems to be lines 96, 97 and 98 from Table A sheet two 
# data (Youth and Community: Positive Actions, Youth Work, and Connexions)
# e.g. Hackney YPS 2011:    11791000, Hackney 96,97,98:    11200322 (+5.2%)
# e.g. Greenwhich YPS 2011: 10900000, Greenwich 96,97,98:  8222006  (+32.5%)
# e.g. Camden YPS 2011:     6441620,  Camden 96,97,98:     7131733  (-9.7%)
# e.g. Doncaster YPS 2011:  6647000,  Doncaster 96,97,98:  7196023  (-8%)
# e.g. Gateshead YPS2011:   6958000,  Gateshead 96,97,98:  4393864  (+58%)
# e.g. Hartlepool YPS2011:  2969000,  Hartlepool 96,97,98: 2609613  (+13.7%)
# Avg: 15.28

# included in services for young people from 2011:

# Universal services for young people (including youth work, positive activities and  information, advice and guidance)
# Targeted services for young people (including youth work, positive activities and information, advice and guidance)
# Substances misuse services (Drugs, Alcohol and Volatile substances)
# Teenage pregnancy services
# Connexions
# Student support/including mandatory awards

# Equivalent to 
# In table A
# Positive Actions
# Youth Work
# Connexions
# Student Support
# Teenage Pregnancy - in table A1 Family Support Services
# Drug Abuse advice - In Table A1 Family Support Services (why necessary to combine non-sg, non-cla)



# Get young peoples spending from table a sheet two and add as gspend_yps

yps_2010 <- readxl::read_xls("data/s251/2009-10/outturn detailed report table a 2009 10.xls", sheet = 2, skip = 3) %>%
  clean_names()

yps_2010 <- yps_2010 %>%
  select(lea, lea_name, line, s52_line_reference, total_expenditure = total_expenditure_k)

yps_2010 <- yps_2010 %>% 
  filter(line %in% c(96:99)) %>%
  select(-line) %>%
  group_by(old_la_code = lea, la_name = lea_name) %>%
  summarise(gspend_yps = sum(total_expenditure))

yps_2010 <- left_join(yps_2010, lea_ons_code_lu, by = c("old_la_code")) %>% 
  relocate(new_la_code, .after = old_la_code)


anti_join(s251_2010, yps_2010, by = "new_la_code")

# aggregate poole and bournemouth

yps_2010 <- yps_2010 %>%
  ungroup() %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000028" ~ "E06000058",
                            new_la_code == "E06000029" ~ "E06000058",
                            TRUE ~ new_la_code),
    la_name = ifelse(new_la_code == "E06000058", "Bournemouth, Christchurch and Poole", la_name)
  ) %>%
  group_by(new_la_code, la_name) %>%
  summarise(gspend_yps = sum(gspend_yps)) %>%
  ungroup()

s251_2010 <- left_join(s251_2010, yps_2010 %>% select(-la_name), by = "new_la_code")


# Merge gspend_saf and gspend_strat into gspend_saf
s251_2010 <- s251_2010 %>%
  mutate(
    gspend_saf = rowSums(across(c(gspend_saf, gspend_strat)))
  ) %>%
  select(-gspend_strat)

# Calculate gspend_nonsg_noncla
s251_2010 <- s251_2010 %>%
  mutate(
    gspend_noncla_nonsg = rowSums(across(c(gspend_fss, gspend_oth, gspend_yps, gspend_ss5)))
  ) 

# Merged 2010 spend data with 11 to 21

anti_join(s251_11_22, s251_2010, by = "new_la_code") %>% group_by(new_la_code, la_name) %>% summarise()

# Update Northumberland, Buckinghamshire and Gateshead codes in 2010 data

s251_2010 <- s251_2010 %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000048" ~ "E06000057",
                            new_la_code == "E08000020" ~ "E08000037",
                            new_la_code == "E10000002" ~ "E06000060",
                            TRUE ~ new_la_code)
  )

# add missing columns and order columns to be the same (just in case!)
s251_2010 <- s251_2010 %>%
  mutate(old_la_code = NA, .before = new_la_code) %>%
  relocate(
    time_period,
    old_la_code,
    new_la_code, 
    la_name,
    gspend_cla,
    gspend_fss, 
    gspend_oth,
    gspend_saf, 
    gspend_yps,
    gspend_ss5,
    gspend_yjs,
    gspend_noncla_nonsg
  ) %>% 
  mutate_at(vars(gspend_cla:gspend_noncla_nonsg), ~ifelse(. == 0, NA, .))


s251_10_22 <- bind_rows(s251_11_22, s251_2010) 


# Adjust for inflation
s251_10_22

s251_10_22 %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = gspend_cla), linewidth = 0.1)

s251_10_22 %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = gspend_noncla_nonsg), linewidth = 0.1)


# Top Level SPPI, Sections H to U excl. Section K
# Update to 2022 prices
sppi <- readxl::read_xlsx("data/sppi/sppi.xlsx")
sppi <- sppi %>% 
  select(1, `Top Level SPPI, Sections H to U excl. Section K`) %>%
  slice(-2:-6) %>%
  drop_na() %>%
  slice(2:14) %>%
  rename(time_period = 1, sppi =2) %>%
  # rebase to 2021 prices
  mutate(time_period = as.numeric(time_period),
         sppi = 110.2/as.numeric(sppi))

sppi

# Updated 2022
sppi <- read_csv("data/sppi/sppi_2022.csv") %>%
  select(1, `Top Level SPPI, Sections H to U excl. Section K`) %>%
  slice(-1:-6) %>%
  drop_na() %>%
  slice(1:14) %>% 
  rename(time_period = 1, sppi =2) %>%
  # rebase to 2022 prices
  mutate(time_period = as.numeric(time_period),
         sppi = 115.9/as.numeric(sppi))
  
sppi


anti_join(s251_10_22, sppi, by = "time_period")

s251_10_22 <- left_join(s251_10_22, sppi, by = "time_period")

# Adjust spending for inflation
s251_10_22 <- s251_10_22 %>%
  mutate_at(vars(gspend_cla:gspend_noncla_nonsg), ~.*sppi)

s251_10_22 %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = gspend_cla), linewidth = 0.1)

s251_10_22 %>%
  ggplot() +
  geom_line(aes(group = new_la_code, x = time_period, y = gspend_noncla_nonsg), linewidth = 0.1)


#view(s251_11_21 %>% arrange(la_name, time_period))

# Add percentage breakdowns of spend on CLA, SAF, and non-cla, non-SAF

s251_10_22 <- s251_10_22 %>%
  mutate(
    gspend_pc_cla    = round((gspend_cla / (gspend_cla+gspend_saf+gspend_noncla_nonsg)) * 100, 1),
    gspend_pc_sg     = round((gspend_saf / (gspend_cla+gspend_saf+gspend_noncla_nonsg)) * 100, 1),
    gspend_pc_ncnsg  = round((gspend_noncla_nonsg / (gspend_cla+gspend_saf+gspend_noncla_nonsg)) * 100, 1)
  )


# merge with merged data

anti_join(merged_data %>% filter(time_period > 2010), 
          s251_10_22 %>% select(-la_name, -old_la_code), 
          by = c("time_period", "new_la_code"))


anti_join(merged_data %>% filter(time_period > 2009), 
          s251_10_22 %>% select(-la_name, -old_la_code), 
          by = c("time_period", "new_la_code"))

merged_data <- left_join(merged_data, 
                         s251_10_22 %>% select(-la_name, -old_la_code), 
                         by = c("time_period", "new_la_code"))

# create per child spend variables
merged_data <- merged_data %>%
  mutate_at(vars(gspend_cla:gspend_noncla_nonsg), list(pc = ~ . / population_0_16))

# add cla spend per cla and safeguarding spend per cpp (at March 31st)
merged_data <- merged_data %>%
  mutate(
    gspend_cla_percla = gspend_cla / at31_cla,
    gspend_saf_percpp = gspend_saf / at31_cpp
  )



#### Housing NOT updated for 2022 - need better source of data since open data
#### Portal does not seem to be updated

# Housing data

lt_ut_lu <- read_csv("data/ltla-utla-lookup/ltla-utla-lookup.csv") %>% clean_names()

# total households in 1000s
n_hhs <- read_csv("data/mhclg/household-n-projections/hh-projections.csv")

n_hhs <- n_hhs %>% 
  arrange(FeatureCode, DateCode) %>%
  clean_names() %>%
  mutate(n_households = value*1000) %>%
  select(-measurement, -units, -value, -households) 

anti_join(lt_ut_lu %>% select(-fid, -ltla21nm), n_hhs, by = c("ltla21cd"="feature_code")) #%>% view(title = "missing_from_lu")
anti_join(n_hhs %>% filter(date_code == 2018), lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) # %>% view(title = "missing_from_housing")

# Update LTLA codes with new ones
n_hhs <- n_hhs %>% 
  mutate(
    feature_code = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      feature_code == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      feature_code == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      feature_code == "E06000048" ~ "E06000057",
      # Ayesbury Vale (Buckinghamshire)
      feature_code == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      feature_code == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      feature_code == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      feature_code == "E07000007" ~ "E06000060",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      feature_code == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      feature_code == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      feature_code == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      feature_code == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      feature_code == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      feature_code == "E07000053" ~ "E06000059",
      # East Hertfordshire -> East Hertfordshire
      feature_code == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      feature_code == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      feature_code == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      feature_code == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      feature_code == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      feature_code == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      feature_code == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      feature_code == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      feature_code == "E07000156" ~ "E06000061",
      # Taunton Deane -> Somerset West and Taunton
      feature_code == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      feature_code == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      feature_code == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      feature_code == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      feature_code == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      feature_code == "E07000206" ~ "E07000244",
      # Gateshead -> Gateshead (new)
      feature_code == "E08000020" ~ "E08000037",
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ feature_code
    )
  )

anti_join(n_hhs, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>% .$feature_code %>% unique()

n_hhs <- left_join(n_hhs, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>%
  group_by(utla21cd, utla21nm, date_code) %>%
  summarise(n_households = sum(n_households)) %>%
  ungroup() 




# households in temporary accommodation
tempacc <- read_csv("data/mhclg/temp-accom/temp-accom.csv") %>%
  clean_names() 

unique(tempacc$measurement)  
unique(tempacc$dclg_temporary_housing_types_concept_scheme)  

tempacc <- tempacc %>%
  filter(dclg_temporary_housing_types_concept_scheme == "All temporary housing types") %>%
  select(-dclg_temporary_housing_types_concept_scheme, -units, -measurement) %>%
  # sum over year
  mutate(date_code = as.numeric(str_remove_all(date_code, " Q[0-9]"))) %>%
  group_by(feature_code, date_code) %>%
  summarise(tempacc_hh = sum(value)) %>%
  ungroup() %>%
  arrange(feature_code, date_code) #%>%


# update ltla codes with most recent

anti_join(tempacc, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>% .$feature_code %>% unique()

# Update LTLA codes with new ones
tempacc <- tempacc %>% 
  mutate(
    feature_code = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      feature_code == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      feature_code == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      feature_code == "E06000048" ~ "E06000057",
      # Ayesbury Vale (Buckinghamshire)
      feature_code == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      feature_code == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      feature_code == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      feature_code == "E07000007" ~ "E06000060",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      feature_code == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      feature_code == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      feature_code == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      feature_code == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      feature_code == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      feature_code == "E07000053" ~ "E06000059",
      # East Hertfordshire -> East Hertfordshire
      feature_code == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      feature_code == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      feature_code == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      feature_code == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      feature_code == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      feature_code == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      feature_code == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      feature_code == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      feature_code == "E07000156" ~ "E06000061",
      # Taunton Deane -> Somerset West and Taunton
      feature_code == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      feature_code == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      feature_code == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      feature_code == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      feature_code == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      feature_code == "E07000206" ~ "E07000244",
      # Gateshead -> Gateshead (new)
      feature_code == "E08000020" ~ "E08000037",
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ feature_code
    )
  )


tempacc <- left_join(tempacc, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>%
  group_by(utla21cd, utla21nm, date_code) %>%
  summarise(tempacc_hh = sum(tempacc_hh)) %>%
  ungroup()


#view(tempacc %>% arrange(utla21cd, date_code))

# 2011 only contains 3 quarters and 2018 only includes one quarter so should be removed
tempacc <- tempacc %>%
  filter(!date_code == 2011 & !date_code == 2018)

# some will be missing from having missing years (presumably zeroes)
anti_join(n_hhs %>% filter(date_code > 2011 & date_code < 2018), 
          tempacc, by = c("utla21cd", "utla21nm", "date_code"))
m_hhd <- left_join(n_hhs, tempacc, by = c("utla21cd", "utla21nm", "date_code"))


############################################################
# Accepted homeless and intentionally homeless
homeless <- read_csv("data/mhclg/homelessness/homelessness-decisions.csv") %>%
  clean_names()

unique(homeless$measurement)
unique(homeless$dclg_homelessness_decsion_type_concept_scheme)

# 2018 and 2011 incomplete
homeless <- homeless %>%
  pivot_wider(values_from = value, names_from = dclg_homelessness_decsion_type_concept_scheme) %>%
  rename(rej_not_hl = `Rejected not homeless`,
         rej_not_priority = `Rejected not in priority need`,
         acc_hl = `Accepted Total`,
         rej_int_hl = `Rejected intentionally homeless`,
         hl_apps = All
  ) %>%
  select(-measurement, -units) %>%
  mutate(
    homeless_any = ifelse(is.na(rej_not_priority) & is.na(acc_hl) & is.na(rej_int_hl), NA,
                          rowSums(across(c(rej_not_priority, acc_hl, rej_int_hl)), na.rm = TRUE))
  ) %>%
  # sum over year
  mutate(date_code = as.numeric(str_remove_all(date_code, " Q[0-9]"))) %>%
  group_by(feature_code, date_code) %>%
  summarise_at(vars(rej_not_hl:homeless_any), ~sum(., na.rm = TRUE)) %>%
  filter(!date_code %in% c(2011, 2018)) %>%
  ungroup() 

anti_join(homeless, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>% .$feature_code %>% unique(.) %>% sort(.)

homeless <- homeless %>% 
  mutate(
    feature_code = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      feature_code == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      feature_code == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      feature_code == "E06000048" ~ "E06000057",
      # Ayesbury Vale (Buckinghamshire)
      feature_code == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      feature_code == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      feature_code == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      feature_code == "E07000007" ~ "E06000060",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      feature_code == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      feature_code == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      feature_code == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      feature_code == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      feature_code == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      feature_code == "E07000053" ~ "E06000059",
      # East Hertfordshire -> East Hertfordshire
      feature_code == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      feature_code == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      feature_code == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      feature_code == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      feature_code == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      feature_code == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      feature_code == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      feature_code == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      feature_code == "E07000156" ~ "E06000061",
      # Taunton Deane -> Somerset West and Taunton
      feature_code == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      feature_code == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      feature_code == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      feature_code == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      feature_code == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      feature_code == "E07000206" ~ "E07000244",
      # Gateshead -> Gateshead (new)
      feature_code == "E08000020" ~ "E08000037",
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ feature_code
    )
  )

homeless <- left_join(homeless, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>% 
  group_by(utla21cd, utla21nm, date_code) %>%
  summarise_at(vars(rej_not_hl:homeless_any), ~sum(.)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(utla21cd))

# homeless %>%
#   ggplot() +
#   geom_line(aes(x = date_code, y = hl_apps, group = utla21cd), size = 0.1)

# Merge homeless data with rest
anti_join(m_hhd %>% filter(date_code == 2015), homeless %>% select(-utla21nm), by = c("utla21cd", "date_code")) # Isles of Schilly missing. NA = England total

m_hhd <- left_join(m_hhd, homeless %>% select(-utla21nm), by = c("utla21cd", "date_code")) %>%
  filter(!is.na(utla21cd))

m_hhd


# Households on Waiting Lists
waitlist <- read_csv("data/mhclg/house-waiting-lists/waiting-lists.csv")

# through 2017
unique(waitlist$Measurement)
unique(waitlist$DateCode) %>% sort()
unique(waitlist$Households) %>% sort()

waitlist <- waitlist %>% select(-Measurement, -Households, -Units) %>% clean_names()
waitlist

anti_join(waitlist, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd")) %>% .$feature_code %>% unique(.) %>% sort(.)

waitlist <- waitlist %>% 
  mutate(
    feature_code = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      feature_code == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      feature_code == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      feature_code == "E06000048" ~ "E06000057",
      # Mid Bedfordshire -> Central Bedfordshire
      feature_code == "E07000001" ~ "E06000056",
      # South Bedfordshire -> Central Bedfordshire
      feature_code == "E07000003" ~ "E06000056",
      # Ayesbury Vale (Buckinghamshire)
      feature_code == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      feature_code == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      feature_code == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      feature_code == "E07000007" ~ "E06000060",
      # Chester -> Chesire West and Chester
      feature_code == "E07000013" ~ "E06000050",
      # Congleton -> Chesire East
      feature_code == "E07000014" ~ "E06000049",
      # Crewe and Nantwich -> Chesire East
      feature_code == "E07000015" ~ "E06000049",
      # Ellesmere Port and Neston -> Chesire West and Chester
      feature_code == "E07000016" ~ "E06000050",
      # Macclesfield -> Chesire East
      feature_code == "E07000017" ~ "E06000049",
      # Vale Royal -> Cheshire West and Chester
      feature_code == "E07000018" ~ "E06000050",
      # Caradon -> Cornwall 
      feature_code == "E07000019" ~ "E06000052",
      # Carrick -> Cornwall 
      feature_code == "E07000020" ~ "E06000052",
      # Kerrier -> Cornwall
      feature_code == "E07000021" ~ "E06000052",
      # North Cornwall -> Cornwall
      feature_code == "E07000022" ~ "E06000052",
      # Penwith -> Cornwall
      feature_code == "E07000023" ~ "E06000052",
      # Restormel -> Cornwall
      feature_code == "E07000024" ~ "E06000052",
      # Isles of Scilly -> Isles of Scilly new
      feature_code == "E07000025" ~ "E06000053",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      feature_code == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      feature_code == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      feature_code == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      feature_code == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      feature_code == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      feature_code == "E07000053" ~ "E06000059",
      # Chester-le-street -> County Durham
      feature_code == "E07000054" ~ "E06000047",
      # Derwentside -> County Durham
      feature_code == "E07000055" ~ "E06000047",
      # Durham -> County Durham
      feature_code == "E07000056" ~ "E06000047",
      # Easington -> County Durham
      feature_code == "E07000057" ~ "E06000047",
      # Sedgefield -> County Durham
      feature_code == "E07000058" ~ "E06000047",
      # Teesdale -> County Durham
      feature_code == "E07000059" ~ "E06000047",
      # Wear Valley -> County Durham
      feature_code == "E07000060" ~ "E06000047",
      # East Hertfordshire -> East Hertfordshire
      feature_code == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      feature_code == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      feature_code == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      feature_code == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      feature_code == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      feature_code == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      feature_code == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      feature_code == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      feature_code == "E07000156" ~ "E06000061",
      # Alnwick -> Northumberland -> Northumberland (new)
      feature_code == "E07000157" ~ "E06000057",
      # Berwick-upon-Tweed -> Northumberland -> Northumberland (new)
      feature_code == "E07000158" ~ "E06000057",
      # Blyth Valley -> Northumberland -> Northumberland (new)
      feature_code == "E07000159" ~ "E06000057",
      # Castle Morpeth -> Northumberland -> Northumberland (new)
      feature_code == "E07000160" ~ "E06000057",
      # Tynedale -> Northumberland -> Northumberland (new)
      feature_code == "E07000161" ~ "E06000057",
      # Wansbeck -> Northumberland -> Northumberland (new)
      feature_code == "E07000162" ~ "E06000057",
      # Bridgnorth -> Shropshire 
      feature_code == "E07000182" ~ "E06000051",
      # North Shropshire -> Shropshire
      feature_code == "E07000183" ~ "E06000051",
      # Oswestry -> Shropshire
      feature_code == "E07000184" ~ "E06000051",
      # Shrewsbury and Atcham -> Shropshire
      feature_code == "E07000185" ~ "E06000051",
      # South Shropshire -> Shropshire
      feature_code == "E07000186" ~ "E06000051",
      # Taunton Deane -> Somerset West and Taunton
      feature_code == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      feature_code == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      feature_code == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      feature_code == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      feature_code == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      feature_code == "E07000206" ~ "E07000244",
      # Kennet -> Wiltshire
      feature_code == "E07000230" ~ "E06000054",
      # North Wiltshire -> Wiltshire
      feature_code == "E07000231" ~ "E06000054",
      # Salisbury -> Wiltshire
      feature_code == "E07000232" ~ "E06000054",
      # West Wiltshire -> Wiltshire
      feature_code == "E07000233" ~ "E06000054",
      # Gateshead -> Gateshead (new)
      feature_code == "E08000020" ~ "E08000037",
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ feature_code
    )
  )

waitlist <- left_join(waitlist, lt_ut_lu %>% select(-fid, -ltla21nm), by = c("feature_code"="ltla21cd"))

waitlist <- waitlist %>%
  group_by(utla21cd, utla21nm, date_code) %>%
  summarise(value = sum(value)) %>%
  rename(waitlist_n = value)

min(waitlist$date_code)
max(waitlist$date_code)

anti_join(m_hhd %>% filter(date_code > 1999 & date_code < 2018), waitlist, by = c("utla21cd", "utla21nm", "date_code"))
# Missing Hounslow 2011

m_hhd <- left_join(m_hhd, waitlist, by = c("utla21cd", "utla21nm", "date_code"))

# Add data for tenure type here
















# Calculate per 10,000 household rates 
m_hhd <- m_hhd %>% 
  mutate_at(vars(tempacc_hh:waitlist_n), list(rate = ~(. / n_households) * 10000) )


m_hhd <- m_hhd %>% rename(time_period = date_code)
m_hhd <- m_hhd %>% rename(new_la_code = utla21cd)


# join m_hhd to merged_data 

# Missing Codes
anti_join(merged_data %>% filter(time_period %in% 2011:2019), m_hhd %>% select(-utla21nm), by = c("new_la_code", "time_period")) %>% .$new_la_code %>% unique() %>% sort()
anti_join(merged_data %>% filter(time_period %in% 2011:2019), m_hhd %>% select(-utla21nm), by = c("new_la_code", "time_period")) %>% .$la_name %>% unique() %>% sort()

# Dorset and Northamptonshire
m_hhd <- m_hhd %>%
  mutate(
    new_la_code = case_when(# Dorset 
      new_la_code == "E06000059" ~ "E10000009",
      # merge North Northamptonshire and West Northamptonshire into Northamptonshire and recaclulate rates
      new_la_code == "E06000061" ~ "E10000021",
      new_la_code == "E06000062" ~ "E10000021",
      TRUE ~ new_la_code
    ),
    utla21nm = case_when(
      new_la_code == "E10000021" ~ "Northamptonshire",
      TRUE ~ utla21nm
    )
  ) %>%
  group_by(new_la_code, utla21nm, time_period) %>%
  summarise_at(vars(n_households:waitlist_n), ~sum(.)) %>%
  ungroup() %>%
  mutate_at(vars(tempacc_hh:waitlist_n), list(rate = ~(. / n_households) * 10000))


merged_data <- left_join(merged_data, m_hhd %>% select(-utla21nm), by = c("new_la_code", "time_period"))






############### INDICES OF MULTIPLE DEPRIVATION





# IMD 2015 and IMD 2019 at UTLA level
imd_2015_imd <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "IMD") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    imd = imd_average_score
  )

imd_2015_income <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "Income") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    income_dep = income_average_score
  )

imd_2015_employ <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "Employment") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    employ_dep = employment_average_score
  )

imd_2015_edu <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "Education") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    edu_dep = education_skills_and_training_average_score
  )

imd_2015_health <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "Health") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    health_dep = health_deprivation_and_disability_average_score
  )

imd_2015_crime <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "Crime") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    crime_dep = crime_average_score
  )

imd_2015_idaci <- readxl::read_xlsx("data/imd/imd-2015.xlsx", sheet = "IDACI") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2013,
    la_name = upper_tier_local_authority_district_name_2013,
    idaci = income_deprivation_affecting_children_index_idaci_average_score
  )


# merge IMDs
imd2015 <- left_join(imd_2015_imd, imd_2015_income, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2015_employ, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2015_edu, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2015_health, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2015_crime, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2015_idaci, by = c("new_la_code", "la_name")) %>%
  rename_at(vars(imd:idaci), ~paste0("imd15_", .))


# IMD 
imd_2019_imd <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "IMD") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    imd = imd_average_score
  )

imd_2019_income <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "Income") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    income_dep = income_average_score
  )

imd_2019_employ <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "Employment") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    employ_dep = employment_average_score
  )

imd_2019_edu <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "Education") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    edu_dep = education_skills_and_training_average_score
  )

imd_2019_health <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "Health") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    health_dep = health_deprivation_and_disability_average_score
  )

imd_2019_crime <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "Crime") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    crime_dep = crime_average_score
  )

imd_2019_idaci <- readxl::read_xlsx("data/imd/imd-2019.xlsx", sheet = "IDACI") %>%
  clean_names() %>%
  select(
    new_la_code = upper_tier_local_authority_district_code_2019,
    la_name = upper_tier_local_authority_district_name_2019,
    idaci = idaci_average_score
  )

imd2019 <- left_join(imd_2019_imd, imd_2019_income, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2019_employ, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2019_edu, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2019_health, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2019_crime, by = c("new_la_code", "la_name")) %>%
  left_join(imd_2019_idaci, by = c("new_la_code", "la_name")) %>%
  rename_at(vars(imd:idaci), ~paste0("imd19_", .))

anti_join(imd2019, imd2015 %>% select(-la_name), by = "new_la_code")
anti_join(imd2015, imd2019 %>% select(-la_name), by = "new_la_code")

# Update code for Dorset and combine Bournemouth and Poole into Bournemouth, Christchurch and Poole (Population Weighted Average)

# Bournemouth household population 2015: E06000028: 84821
# Poole household population 2015: E06000029: 66028
read_csv("data/mhclg/household-n-projections/hh-projections.csv") %>% 
  arrange(FeatureCode, DateCode) %>%
  clean_names() %>%
  mutate(n_households = value*1000) %>%
  select(-measurement, -units, -value, -households) %>% 
  filter(feature_code %in% c("E06000028", "E06000029"), date_code == 2015)

b_pop <- 84821
p_pop <- 66028
bp_pop <- b_pop + p_pop

imd2015 <- imd2015 %>%
  add_row(new_la_code = "E06000058",
          la_name = "Bournemouth, Christchurch and Poole",
          imd15_imd = ((21.8 * b_pop) + (15.2 * p_pop)) / bp_pop,
          imd15_income_dep = ((0.14 * b_pop) + (0.112 * p_pop)) / bp_pop,
          imd15_employ_dep = ((0.117 * b_pop) + (0.089 * p_pop)) / bp_pop,
          imd15_edu_dep = ((21.5 * b_pop) + (20.2 * p_pop)) / bp_pop,
          imd15_health_dep = ((0.067 * b_pop) + (-0.216 * p_pop)) / bp_pop,
          imd15_crime_dep = ((0.322 * b_pop) + (-0.244 * p_pop)) / bp_pop,
          imd15_idaci = ((0.187 * b_pop) + (0.162 * p_pop)) / bp_pop
  ) %>%
  # update Dorset code
  mutate(new_la_code = case_when(
    new_la_code == "E10000009" ~ "E06000059",
    TRUE ~ new_la_code
  ))

anti_join(imd2019, imd2015 %>% select(-la_name), by = "new_la_code")

imd_combined <- left_join(imd2019, imd2015 %>% select(-la_name), by = "new_la_code")

# Merge IMD data with merged data

anti_join(merged_data, imd_combined %>% select(-la_name), by = "new_la_code") %>% group_by(new_la_code, la_name) %>% summarise()
anti_join(imd_combined, merged_data, by = "new_la_code") %>% group_by(new_la_code, la_name) %>% summarise()

# Update codes for Buckinghamshire and Dorset
imd_combined <- imd_combined %>%
  mutate(
    new_la_code = case_when(new_la_code == "E06000059" ~ "E10000009",
                            new_la_code == "E10000002" ~ "E06000060",
                            TRUE ~ new_la_code
    )
  )

merged_data <- left_join(merged_data, imd_combined %>% select(-la_name), by = "new_la_code")

# Check for duplicates
sum(duplicated(merged_data))


merged_data %>%
  filter(!la_name %in% "City of London") %>%
  ggplot() +
  geom_line(aes(x = time_period, y = gspend_noncla_nonsg_pc, group = new_la_code), size = 0.1)

merged_data %>%
  group_by(time_period) %>%
  mutate_at(vars(gspend_cla_percla, gspend_saf_pc, gspend_noncla_nonsg_pc), ~ifelse(. == 0 | . == Inf, NA, .)) %>%
  summarise_at(vars(gspend_cla_percla, gspend_saf_pc, gspend_noncla_nonsg_pc), list(med = ~median(., na.rm = TRUE),
                                                                                    mean = ~mean(., na.rm = TRUE)))



####### Income data


# Add median income estimate and inequality estimate as ratio between mean and median
# ltla level


# Keep N jobs for weighting at UTLA level

ehw2011 <- readxl::read_xls("data/average-income/Earnings and hours worked-2011/REVISED - Home Geography Table 8.7a   Annual pay - Gross 2011.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, old_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(old_la_code)) %>%
  mutate(
    time_period = 2011, .before = njobs
  )
ehw2011


ehw2012 <- readxl::read_xls("data/average-income/Earnings and hours worked-2012/Home Geography Table 8.7a   Annual pay - Gross 2012.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2012, .before = njobs
  )
ehw2012

anti_join(ehw2011, ehw2012, by = "la_name")

ehw2011 <- ehw2011 %>%
  mutate(
    la_name = case_when(la_name == "St Helens" ~ "St. Helens",
                        la_name == "Herefordshire, County of UA" ~ "Herefordshire UA",
                        la_name == "Isles of Scilly" ~ "Isles of Scilly UA",
                        TRUE ~ la_name
    )
  )

ehw2011 <- left_join(ehw2011, ehw2012 %>% select(la_name, new_la_code), by = "la_name") %>%
  select(-old_la_code) %>%
  relocate(new_la_code, .after = la_name)

# Add earnings and hours worked 2010
ehw2010 <- readxl::read_xls("data/average-income/Earnings and hours worked-2010/REVISED - Home Geography Table 8.7a   Annual pay - Gross 2010.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, old_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(old_la_code)) %>%
  mutate(
    time_period = 2010, .before = njobs
  )


# get new codes
anti_join(ehw2010, ehw2012, by = "la_name") #%>% view()

ehw2010 <- ehw2010 %>%
  mutate(
    la_name = case_when(la_name == "County Durham" ~ "County Durham UA",
                        # Chester-le-Street - included in County Durham aggregate
                        # Derwentside - included in County Durham Aggregate
                        # Durham - included in County Durham aggregate
                        # Easington - included in County Durham aggregate
                        # Sedgefield - included in County Durham aggregate
                        # Teesdale - included in County Durham aggregate
                        # Wear Valley - included in County Durham aggregate
                        # Alnwick - included in Northumberland UA
                        # Berwick-upon-Tweed - included in Northumberland
                        # Blyth Valley - included in Northumberland
                        # Castle Morpeth - included in Northumberland
                        # Tynedale - included in Northumberland 
                        # Wansbeck - included in Northumberland
                        ### Cheshire sub areas need to be aggregated up to Cheshire East and Cheshire West & Chester
                        # Congleton -> Cheshire East UA
                        la_name == "Congleton" ~ "Cheshire East UA",
                        # Crewe and Nantwich -> Cheshire East UA
                        la_name == "Crewe and Nantwich" ~ "Cheshire East UA",
                        # Macclesfield -> Cheshire East UA
                        la_name == "Macclesfield" ~ "Cheshire East UA",
                        # Cheshire West & Chester UA - already aggregated
                        la_name == "Cheshire West & Chester UA" ~ "Cheshire West and Chester UA",
                        # Chester -> included in Cheshire West and Chester UA
                        # Ellesmere Port & Neston -> included in Cheshire West and Chester UA
                        # Vale Royal -> included in Cheshire West and Chester UA
                        # Merseyside -> Merseyside MC
                        la_name == "Merseyside" ~ "Merseyside MC",
                        # Herefordshire, County of UA -> Herefordshire UA
                        la_name == "Herefordshire, County of UA" ~ "Herefordshire UA",
                        # Bridgnorth - included in Shopshire UA aggregate
                        # North Shropshire - included in Shropshire UA aggregate
                        # Oswestry - included in Shropshire UA aggregate
                        # Shrewsbury and Atcham - included in Shropshire UA aggregate
                        # South Shropshire - included in Shropshire UA aggregate
                        ### Combine Mid Bedfordshire and South Bedfordshire into Central Bedfordshire UA and aggregate (weighted)
                        la_name == "Mid Bedfordshire" ~ "Central Bedfordshire UA",
                        la_name == "South Bedfordshire" ~ "Central Bedfordshire UA",
                        # Bedford -> Bedford UA
                        la_name == "Bedford" ~ "Bedford UA",
                        la_name == "St. Albans" ~ "St Albans",
                        la_name == "St. Edmundsbury" ~ "St Edmundsbury",
                        # Caradon - included in Cornwall UA aggregate
                        # Carrick - included in Cornwall UA aggregate
                        # Kerrier - included in Cornwall UA aggregate
                        # North Cornwall - included in Cornwall UA aggregate
                        # Penwith - included in Cornwall UA aggregate
                        # Restormel - included in Cornwall UA aggregate
                        # Isles of Scilly -> Isles of Scilly UA
                        la_name == "Isles of Scilly" ~ "Isles of Scilly UA",
                        # Kennet - included in Wiltshire UA aggregate
                        # North Wiltshire - included in Wiltshire UA aggregate
                        # Salisbury - included in Wiltshire UA aggregate
                        # West Wiltshire - included in Wiltshire UA aggregare
                        # Other non-matches are Wales/Scotland, not currently included
                        TRUE ~ la_name
    )
  )

# Aggregate Central Bedfordshire and Cheshire East 
ehw2010 <- ehw2010 %>%
  mutate_at(vars(njobs:mean_gppa), as.numeric) %>%
  group_by(la_name) %>%
  summarise(time_period = 2010,
            median_gppa = sum((median_gppa * njobs)) / sum(njobs),
            mean_gppa = sum((mean_gppa * njobs)) / sum(njobs),
            njobs = sum(njobs)
  ) 


ehw2010 <- left_join(ehw2010, ehw2012 %>% select(la_name, new_la_code), by = "la_name") %>%
  relocate(new_la_code, .after = la_name) %>%
  filter(!is.na(new_la_code))

ehw2013 <- readxl::read_xls("data/average-income/Earnings and hours worked-2013/Home Geography Table 8.7a   Annual pay - Gross 2013.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2013, .before = njobs
  )

ehw2014 <- readxl::read_xls("data/average-income/Earnings and hours worked-2014/Home Geography Table 8.7a   Annual pay - Gross 2014.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2014, .before = njobs
  )

ehw2015 <- readxl::read_xls("data/average-income/Earnings and hours worked-2015/Home Geography Table 8.7a   Annual pay - Gross 2015.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2015, .before = njobs
  )

ehw2016 <- readxl::read_xls("data/average-income/Earnings and hours worked-2016/Home Geography Table 8.7a   Annual pay - Gross 2016.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2016, .before = njobs
  )

ehw2017 <- readxl::read_xls("data/average-income/Earnings and hours worked-2017/Home Geography Table 8.7a   Annual pay - Gross 2017.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2017, .before = njobs
  )

ehw2018 <- readxl::read_xls("data/average-income/Earnings and hours worked-2018/Home Geography Table 8.7a   Annual pay - Gross 2018.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2018, .before = njobs
  )

ehw2019 <- readxl::read_xls("data/average-income/Earnings and hours worked-2019/Home Geography Table 8.7a   Annual pay - Gross 2019.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2019, .before = njobs
  )

ehw2020 <- readxl::read_xls("data/average-income/Earnings and hours worked-2020/Home Geography Table 8.7a   Annual pay - Gross 2020.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2020, .before = njobs
  )

ehw2021 <- readxl::read_xls("data/average-income/Earnings and hours worked-2021-revised/Home Geography Table 8.7a   Annual pay - Gross 2021.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2021, .before = njobs
  )

ehw2021 <- ehw2021 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2020 <- ehw2020 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2019 <- ehw2019 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2018 <- ehw2018 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2017 <- ehw2017 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2016 <- ehw2016 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2015 <- ehw2015 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2014 <- ehw2014 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2013 <- ehw2013 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2012 <- ehw2012 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))
ehw2011 <- ehw2011 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))


# Bind rows for data
ehw <- bind_rows(ehw2021, ehw2020, ehw2019, ehw2018, ehw2017, ehw2016, ehw2015, ehw2014, ehw2013, ehw2012, ehw2011, ehw2010)


# Add earnings and hours worked 2022 prov
ehw2022 <- readxl::read_xls("data/average-income/Earnings and hours worked-2022-prov/PROV - Home Geography Table 8.7a   Annual pay - Gross 2022.xls", sheet = "All", skip = 4) %>%
  clean_names() %>%
  select(la_name = description, new_la_code = code, njobs = thousand, median_gppa = median, mean_gppa = mean) %>%
  filter(!is.na(new_la_code)) %>%
  mutate(
    time_period = 2022, .before = njobs
  )

ehw2022 <- ehw2022 %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .)))

ehw <- bind_rows(ehw2022, ehw)


left_join(ehw, ltla_utla_lu, by = c("new_la_code" = "ltla21cd"))

anti_join(ehw, ltla_utla_lu, by = c("new_la_code" = "ltla21cd")) %>% group_by(new_la_code, la_name) %>% summarise() %>% print(n = 123)

ehw <- ehw %>% 
  mutate(
    new_la_code = case_when(
      # Bournemouth -> Bournemouth, Christchurch and Poole
      new_la_code == "E06000028" ~ "E06000058",
      # Poole -> Bournemouth, Christchurch and Poole
      new_la_code == "E06000029" ~ "E06000058",
      # Northumberland -> Northumberland (new)
      new_la_code == "E06000048" ~ "E06000057",
      # Mid Bedfordshire -> Central Bedfordshire
      new_la_code == "E07000001" ~ "E06000056",
      # South Bedfordshire -> Central Bedfordshire
      new_la_code == "E07000003" ~ "E06000056",
      # Ayesbury Vale (Buckinghamshire)
      new_la_code == "E07000004" ~ "E06000060",
      # Chiltern -> Buckinghamshire
      new_la_code == "E07000005" ~ "E06000060",
      # South Bucks -> Buckinghamshire
      new_la_code == "E07000006" ~ "E06000060",
      # Wycombe -> Buckinghamshire
      new_la_code == "E07000007" ~ "E06000060",
      # Buckinghamshire -> Buckinghamshire (update code)
      new_la_code == "E10000002" ~ "E06000060",
      # Chester -> Chesire West and Chester
      new_la_code == "E07000013" ~ "E06000050",
      # Congleton -> Chesire East
      new_la_code == "E07000014" ~ "E06000049",
      # Crewe and Nantwich -> Chesire East
      new_la_code == "E07000015" ~ "E06000049",
      # Ellesmere Port and Neston -> Chesire West and Chester
      new_la_code == "E07000016" ~ "E06000050",
      # Macclesfield -> Chesire East
      new_la_code == "E07000017" ~ "E06000049",
      # Vale Royal -> Cheshire West and Chester
      new_la_code == "E07000018" ~ "E06000050",
      # Caradon -> Cornwall 
      new_la_code == "E07000019" ~ "E06000052",
      # Carrick -> Cornwall 
      new_la_code == "E07000020" ~ "E06000052",
      # Kerrier -> Cornwall
      new_la_code == "E07000021" ~ "E06000052",
      # North Cornwall -> Cornwall
      new_la_code == "E07000022" ~ "E06000052",
      # Penwith -> Cornwall
      new_la_code == "E07000023" ~ "E06000052",
      # Restormel -> Cornwall
      new_la_code == "E07000024" ~ "E06000052",
      # Isles of Scilly -> Isles of Scilly new
      new_la_code == "E07000025" ~ "E06000053",
      # Christchurch -> Bournemouth, Christcurch, and Poole
      new_la_code == "E07000048" ~ "E06000058",
      # East Dorset -> Dorset
      new_la_code == "E07000049" ~ "E06000059",
      # North Dorset -> Dorset
      new_la_code == "E07000050" ~ "E06000059",
      # Purbeck -> Dorset
      new_la_code == "E07000051" ~ "E06000059",
      # West Dorset -> Dorset
      new_la_code == "E07000052" ~ "E06000059",
      # Weymouth and Portland -> Dorset
      new_la_code == "E07000053" ~ "E06000059",
      # Chester-le-street -> County Durham
      new_la_code == "E07000054" ~ "E06000047",
      # Derwentside -> County Durham
      new_la_code == "E07000055" ~ "E06000047",
      # Durham -> County Durham
      new_la_code == "E07000056" ~ "E06000047",
      # Easington -> County Durham
      new_la_code == "E07000057" ~ "E06000047",
      # Sedgefield -> County Durham
      new_la_code == "E07000058" ~ "E06000047",
      # Teesdale -> County Durham
      new_la_code == "E07000059" ~ "E06000047",
      # Wear Valley -> County Durham
      new_la_code == "E07000060" ~ "E06000047",
      # East Hertfordshire -> East Hertfordshire
      new_la_code == "E07000097" ~ "E07000242",
      # Stevenage -> Stevenage (new)
      new_la_code == "E07000101" ~ "E07000243",
      # Corby -> North Northamptonshire
      new_la_code == "E07000150" ~ "E06000061",
      # Daventry -> West Northamptonshire
      new_la_code == "E07000151" ~ "E06000062",
      # East Northamptonshire -> North Northamptonshire
      new_la_code == "E07000152" ~ "E06000061",
      # Kettering -> North Northamptonshire
      new_la_code == "E07000153" ~ "E06000061",
      # Northampton -> West Northamptonshire
      new_la_code == "E07000154" ~ "E06000062",
      # South Northamptonshire -> West Northamptonshire
      new_la_code == "E07000155" ~ "E06000062",
      # Wellingborough -> North Northamptonshire
      new_la_code == "E07000156" ~ "E06000061",
      # Alnwick -> Northumberland -> Northumberland (new)
      new_la_code == "E07000157" ~ "E06000057",
      # Berwick-upon-Tweed -> Northumberland -> Northumberland (new)
      new_la_code == "E07000158" ~ "E06000057",
      # Blyth Valley -> Northumberland -> Northumberland (new)
      new_la_code == "E07000159" ~ "E06000057",
      # Castle Morpeth -> Northumberland -> Northumberland (new)
      new_la_code == "E07000160" ~ "E06000057",
      # Tynedale -> Northumberland -> Northumberland (new)
      new_la_code == "E07000161" ~ "E06000057",
      # Wansbeck -> Northumberland -> Northumberland (new)
      new_la_code == "E07000162" ~ "E06000057",
      # Bridgnorth -> Shropshire 
      new_la_code == "E07000182" ~ "E06000051",
      # North Shropshire -> Shropshire
      new_la_code == "E07000183" ~ "E06000051",
      # Oswestry -> Shropshire
      new_la_code == "E07000184" ~ "E06000051",
      # Shrewsbury and Atcham -> Shropshire
      new_la_code == "E07000185" ~ "E06000051",
      # South Shropshire -> Shropshire
      new_la_code == "E07000186" ~ "E06000051",
      # Taunton Deane -> Somerset West and Taunton
      new_la_code == "E07000190" ~ "E07000246",
      # West Somerset -> Somerset West and Taunton
      new_la_code == "E07000191" ~ "E07000246",
      # Forest Heath -> West Suffolk
      new_la_code == "E07000201" ~ "E07000245",
      # St Edmundsbury -> West Suffolk
      new_la_code == "E07000204" ~ "E07000245",
      # Suffolk Coastal -> East Suffolk
      new_la_code == "E07000205" ~ "E07000244",
      # Waveney -> East Suffolk
      new_la_code == "E07000206" ~ "E07000244",
      # Kennet -> Wiltshire
      new_la_code == "E07000230" ~ "E06000054",
      # North Wiltshire -> Wiltshire
      new_la_code == "E07000231" ~ "E06000054",
      # Salisbury -> Wiltshire
      new_la_code == "E07000232" ~ "E06000054",
      # West Wiltshire -> Wiltshire
      new_la_code == "E07000233" ~ "E06000054",
      # Gateshead -> Gateshead (new)
      new_la_code == "E08000020" ~ "E08000037",
      # rest are already aggregates - will aggregate and weight them again with lower levels
      # Ignore missing UTLA level ones starting with E1, E11, E13, and E92
      TRUE ~ new_la_code
    )
  )


anti_join(ehw, ltla_utla_lu, by = c("new_la_code" = "ltla21cd")) %>% group_by(new_la_code, la_name) %>% summarise()
anti_join(ltla_utla_lu, ehw, by = c("ltla21cd" = "new_la_code")) 

# Remove counties and regions (E10, E11, E12, E92), counties (K), and Scotland(S) and Wales (W)
ehw <- ehw %>%
  filter(!str_detect(new_la_code, "E10|E11|E12|E92|E13|N92|N|K|S|W"))


ehw <- left_join(ehw, ltla_utla_lu, by = c("new_la_code" = "ltla21cd"))

# Aggregate data to UTLA, weighting by njobs
ehw <- ehw %>%
  mutate_at(vars(njobs:mean_gppa), ~as.numeric(ifelse(. == "x", NA, .))) %>%
  mutate(njobs_med = ifelse(is.na(median_gppa), NA, njobs),
         njobs_mu = ifelse(is.na(mean_gppa), NA, njobs)
  ) %>%
  group_by(utla21cd, utla21nm, time_period) %>%
  # Any missing subregion (ltlas) means that end result is missing, 
  # as it may exclude particularly deprived or affluent areas in each
  # year
  summarise(
    median_gppa = sum(median_gppa*njobs_med) / sum(njobs_med),
    mean_gppa   = sum(mean_gppa*njobs_mu) / sum(njobs_mu),
    njobs = sum(njobs)
  )

# Check sums with Derbyshire example
50+29+35+19+46+31+30+46
(24599*50+22934*29+22882*35+22412*19+26386*46+23574*31+25659*30+28693*46)/286
# matches

# remove zeroes (censored data) - change to NA
ehw <- ehw %>%
  ungroup() %>%
  mutate_at(vars(median_gppa:njobs), ~ifelse(. == 0, NA, .))

anti_join(merged_data, ehw, by = c("new_la_code"="utla21cd", "time_period")) %>% filter(time_period > 2010) %>% group_by(new_la_code, la_name) %>% summarise()

# Change Dorset code and aggregate North and West Northamptonshire
ehw <- ehw %>%
  mutate(
    utla21cd = case_when(utla21cd == "E06000059" ~ "E10000009",
                         utla21cd == "E06000061" ~ "E10000021",
                         utla21cd == "E06000062" ~ "E10000021",
                         TRUE ~ utla21cd
    ),
    utla21nm = case_when(utla21cd == "E10000021" ~ "Northamptonshire",
                         TRUE ~ utla21nm)
  ) %>% group_by(utla21cd, utla21nm, time_period) %>%
  summarise(
    median_gppa = sum(median_gppa*njobs) / sum(njobs),
    mean_gppa   = sum(mean_gppa*njobs) / sum(njobs),
    njobs = sum(njobs)
  )

merged_data <- left_join(merged_data %>% ungroup(), ehw %>% ungroup() %>% select(-utla21nm), 
                         by = c("new_la_code"="utla21cd", "time_period")) 


# Adjust for CPI inflation - updated for 2022
cpih <- read_csv("data/cpih/CPIH-2022.csv", skip = 7) %>% 
  rename(year = 1, cpih = 2) %>%
  slice(1:35)

# rebase to 2010 to get 2021 prices
cpih_2022 <- cpih$cpih[cpih$year == 2022]

cpih <- cpih %>%
  mutate(cpih = cpih_2022 / cpih) %>%
  mutate(year = as.numeric(year))

merged_data <- left_join(merged_data, cpih, by = c("time_period" = "year"))

merged_data <- merged_data %>%
  mutate(
    median_gppa_adj = median_gppa * cpih,
    mean_gppa_adj = mean_gppa * cpih
  )

merged_data <- merged_data %>%
  mutate_at(vars(median_gppa, median_gppa_adj, mean_gppa, mean_gppa_adj),
            ~ifelse(. == 0, NA, .))

merged_data %>%
  ggplot() +
  geom_line(aes(x = time_period, y = median_gppa_adj, group = new_la_code), size = 0.2)

merged_data %>%
  ggplot() +
  geom_line(aes(x = time_period, y = mean_gppa_adj, group = new_la_code), size = 0.2)

merged_data %>%
  ggplot() +
  geom_line(aes(x = time_period, y = mean_gppa_adj/median_gppa_adj, group = new_la_code), size = 0.2)

merged_data %>%
  group_by(time_period) %>%
  summarise(median(mean_gppa_adj/median_gppa_adj, na.rm = TRUE))

merged_data %>%
  group_by(time_period) %>%
  summarise(median(mean_gppa_adj, na.rm = TRUE))

merged_data %>%
  group_by(time_period) %>%
  summarise(median(median_gppa_adj, na.rm = TRUE))

# calculate mean/median ratio
merged_data <- merged_data %>%
  mutate(mm_ginc_ratio = mean_gppa_adj/median_gppa_adj) 


# merged_data %>%
#   filter(!la_name %in% "City of London") %>%
#   ggplot() +
#   geom_line(
#     aes(x = time_period, y = gspend_noncla_nonsg_pc, group = new_la_code),
#     size = 0.1
#   )
# 
# merged_data %>%
#   filter(!la_name %in% "City of London") %>%
#   ggplot() +
#   geom_smooth(
#     aes(x = time_period, y = gspend_noncla_nonsg_pc, group = new_la_code),
#     size = 0.1, se = FALSE, span = 0.4
#   )


# political control variable addition

council_pol <- readxl::read_xlsx("data/Council by Political Party.xlsx", skip =1)
colnames(council_pol)

council_pol <- council_pol %>%
  select(
    la_name_2010 = 1, 
    control_2010 = `Maj...8`,
    control_2011 = 20,
    control_2012 = 32,
    control_2013 = 44,
    control_2014 = 56,
    control_2015 = 68,
    control_2016 = 80,
    control_2017 = 92,
    control_2018 = 104,
    control_2019 = 116,
    control_2020 = 128,
    control_2021 = 140
  )

council_pol <- council_pol %>%
  mutate_all(as.character) %>%
  pivot_longer(-1) %>%
  mutate(name = parse_number(name)) %>%
  rename(la_name = la_name_2010, time_period = name, majority = value)

unique(council_pol$majority)

council_pol <- council_pol %>%
  group_by(la_name) %>%
  summarise(
    tot_years = n(),
    n_lab = sum(ifelse(majority == "Lab" | majority == "LAB", 1, 0), na.rm = TRUE),
    n_con = sum(ifelse(majority == "Con" | majority == "CON", 1, 0), na.rm = TRUE),
    n_ld = sum(ifelse(majority == "LD", 1, 0), na.rm = TRUE),
    n_ind = sum(ifelse(majority == "Ind" | majority == "IND", 1, 0), na.rm = TRUE),
    n_noc = sum(ifelse(majority == "NOC", 1, 0), na.rm = TRUE)
  ) %>%
  mutate(
    majority = case_when(n_lab >= 8 ~ "Labour",
                         n_con >= 8 ~ "Conservative",
                         n_ld >= 8 ~ "Liberal Democrat",
                         n_ind >= 8 ~ "Independent",
                         n_noc >= 8 ~ "No Overall Control",
                         TRUE ~ "No Dominant Majority Over Time"
    )
  ) 


# match la_names and add to data

# Add utla codes


anti_join(council_pol, ltla_utla_lu, by = c("la_name" = "utla21nm")) %>% group_by(la_name) %>% summarise()

council_pol <- council_pol %>%
  mutate(
    la_name = case_when(la_name == "Barking & Dagenham" ~ "Barking and Dagenham",
                        la_name == "Bath & North East Somerset" ~ "Bath and North East Somerset",
                        la_name == "Brighton & Hove" ~ "Brighton and Hove",
                        la_name == "Bristol" ~ "Bristol, City of",
                        la_name == "Durham" ~ "County Durham",
                        la_name == "East Riding Of Yorkshire" ~ "East Riding of Yorkshire",
                        la_name == "Hammersmith & Fulham" ~ "Hammersmith and Fulham",
                        la_name == "Herefordshire" ~ "Herefordshire, County of",
                        la_name == "Kensington & Chelsea" ~ "Kensington and Chelsea",
                        la_name == "Kingston Upon Hull" ~ "Kingston upon Hull, City of",
                        la_name == "Kingston Upon Thames" ~ "Kingston upon Thames",
                        la_name == "Newcastle Upon Tyne" ~ "Newcastle upon Tyne",
                        la_name == "Redcar & Cleveland" ~ "Redcar and Cleveland",
                        la_name == "Richmond Upon Thames" ~ "Richmond upon Thames",
                        la_name == "St Helens" ~ "St. Helens",
                        la_name == "Stockton-On-Tees" ~ "Stockton-on-Tees",
                        la_name == "Telford & Wrekin" ~ "Telford and Wrekin",
                        la_name == "Windsor & Maidenhead" ~ "Windsor and Maidenhead",
                        TRUE ~ la_name
    )
  )

council_pol <- left_join(council_pol, ltla_utla_lu %>% select(-fid, -ltla21cd, -ltla21nm) %>% group_by(utla21cd, utla21nm) %>% summarise() %>% ungroup(), 
                         by = c("la_name" = "utla21nm")) %>%
  relocate(utla21cd, .before = la_name)

anti_join(merged_data, council_pol %>% select(-la_name, -tot_years), by = c("new_la_code" = "utla21cd")) %>% group_by(new_la_code, la_name) %>% summarise()

# Update Dorset code
# Missing: Isles of Scilly, City of London, Bournemouth, Christchurch and Poole, Northamptonshire

council_pol <- council_pol %>%
  mutate(
    utla21cd = case_when(utla21cd == "E06000059" ~ "E10000009",
                         TRUE ~ utla21cd)
  )


merged_data <- left_join(merged_data, council_pol %>% select(-la_name, -tot_years), by = c("new_la_code" = "utla21cd"), suffix = c("", "pol_"))


# Some tidying of merged_data: ensure all la_codes and names are the same

anti_join(merged_data, 
          ltla_utla_lu %>% select(-fid, -ltla21cd, -ltla21nm) %>% group_by(utla21cd, utla21nm) %>% summarise() %>% ungroup(),
          by = c("new_la_code" = "utla21cd"))

merged_data <- merged_data %>%
  mutate(
    new_la_code = case_when(new_la_code == "E10000009" ~ "E06000059",
                            TRUE ~ new_la_code)
  )

merged_data <- left_join(merged_data %>% select(-la_name), 
                         ltla_utla_lu %>% select(-fid, -ltla21cd, -ltla21nm) %>% group_by(utla21cd, utla21nm) %>% summarise() %>% ungroup(),
                         by = c("new_la_code" = "utla21cd")) %>%
  relocate(la_name = utla21nm, .after = old_la_code)

# check number of LAs/Codes
merged_data %>% group_by(new_la_code, la_name) %>% summarise()

# save data to tidied data folder

if (dir.exists("tidied_data") == FALSE) { dir.create("tidied_data") }


# reorder columns
merged_data <- merged_data %>%
  relocate(
    # Identifiers
    new_la_code:la_name,
    # Population data
    population_0_16:population_5_16_upper,
    # Referrals
    referrals_n:pc_children_rereferrred, referrals_rate_10000:referrals_not_reref_rate_10000,
    # SEND/EHCP Statements
    send_statem_n:sendst_or_ehcp_n, send_statem_rate_10000:send_or_ehcp_rate_10000,
    # CIN
    anypoint_episodes:at31_episodes, any_cin_ep_rate_10000:at31_cin_ep_rate_10000,
    # CPP
    cpp_start:at31_cpp_rate10000,
    # CLA
    at31_cla:cla_cease_rate10000,
    # Poverty
    rel_pov_chil:clif_11_16_cpov_rate,
    # Median household income from earnings
    median_gppa:mm_ginc_ratio,
    # Spending
    gspend_cla:gspend_saf_percpp,
    # IMD
    imd19_imd:imd15_idaci,
    # Housing
    n_households:waitlist_n_rate,
    # Political party control
    n_lab:majority
  )

write_csv(merged_data, file = "tidied_data/merged_data.csv")

# Mergers/boundary changes:
# Bournemouth, Christchurch and Poole
# Dorset
# Northamptonshire
# Buckinghamshire
# Gateshead

# Codebook for variables

var_names <- colnames(merged_data)
print(var_names, width = 20)
var_labels <- c(
  # Identifiers
  "Upper tier local authority code",
  "Year of data measurement (if financial year, end of financial year)",
  "Old 3-digit local authority code",
  "Local authority name", 
  
  # Population data
  "Mean Population estimate for children (aged 0-16) (ONS Population Estimates from Admin Data)",
  "Lower Population estimate for children (aged 0-16) (ONS Population Estimates from Admin Data)",
  "Upper Population estimate for children (aged 0-16) (ONS Population Estimates from Admin Data)",
  "Mean Population estimate for children (aged 5-16) (ONS Population Estimates from Admin Data)",
  "Lower Population estimate for children (aged 5-16) (ONS Population Estimates from Admin Data)",
  "Upper Population estimate for children (aged 5-16) (ONS Population Estimates from Admin Data)",
  
  # Referrals
  "Number of referrals to children's services, including repeat referrals (CIN Census)",
  "Number of re-referrals to children's services (CIN Census)",
  "Number of referrals ending in No Further Action (CIN Census)",
  "Number of referrals deemed Not in Need (CIN Census)",
  "Number of children referred to children's services (CIN Census)",
  "Number of children re-referred to children's services (CIN Census)",
  "Percentage of referrals that were re-referrals (CIN Census)",
  "Percentage of referrals that were NFA (CIN Census)",
  "Percentage of referrals that were NIN (CIN Census)",
  "Percentage of individual children referred who were re-referred within 12 months (CIN Census)",
  "Rate of referrals throughout year (Rate per 10,000 0-16 population) (CIN Census)",
  "Rate of children referred throughout year (Rate per 10,000 0-16 population) (CIN Census)",
  "Rate of referrals throughout year that were not re-referrals (Rate per 10,000 0-16 population) (CIN Census)",
  
  # SEND/EHCP Statements
  "Number of SEN statements (EHCP Data, DfE)",
  "Number of EHCP plans (succeeding SEN statements) (EHCP Data, DfE)",
  "Combined number of SEN statements and EHCP plans (EHCP Data, DfE)",
  "Rate of SEN statements (Rate per 10,000 5-16 population) (EHCP Data, DfE)",
  "Rate of EHCP plans (Rate per 10,000 5-16 population) (EHCP Data, DfE)",
  "Rate of combined SEN statements & EHCP plans (Rate per 10,000 5-16 population) (EHCP Data, DfE)",
  
  # CIN
  "Number of children in need episodes at any point in the year (CIN Census)",
  "Number of children who started an episode in need in the year (CIN Census)",
  "Number of children in need episodes on 31st March (CIN Census)",
  "Rate of children in need episodes at any point in the year (Rate per 10,000 0-16 population) (CIN Census)",
  "Rate of children who started an episode in need in the year (Rate per 10,000 0-16 population) (CIN Census)",
  "Rate of children in need episodes on 31st March (Rate per 10,000 0-16 population) (CIN Census)",
  
  # CPP
  "Number of children starting a Child Protection Plan (CIN Census)",
  "Number of children on a Child Protection Plan on 31st March (CIN Census)",
  "Rate of children starting a Child Protection Plan (Rate per 10,000 0-16 population) (CIN Census)",
  "Rate of children on a Child Protection Plan on 31st March (Rate per 10,000 0-16 population) (CIN Census)",
  
  # CLA
  "Number of children looked after at 31st March (SSDA903)", 
  "Number of children starting to be looked after throughout the year (SSDA903)",
  "Number of children ceasing to be looked after throughout the year (SSDA903)",
  "Rate of children looked after at 31st March (Rate per 10,000 0-16 population) (SSDA903)", 
  "Rate of children starting to be looked after throughout the year (Rate per 10,000 0-16 population) (SSDA903)",
  "Rate of children ceasing to be looked after throughout the year (Rate per 10,000 0-16 population) (SSDA903)",
  
  # Poverty
  "Number of children in relative low income households (2015-2021, 2022*provisional) (DWP)",
  "Percentage of children in relative low income (2015-2021, 2022*provisional, rate per 100) (DWP)",
  "Number of children in low income families (2011-2016) (DWP)",
  "Rate of children in low income families (2011-2016) (DWP)",
  
  
  # Median household income from earnings
  "Median household gross pay per annum estimate (ONS)",
  "Mean household gross pay per annum estimate (ONS)",
  "Number of jobs (in 1000s) (ONS)",
  "Consumer Price Inflation including Housing (CPIH) (ONS)",
  "Median household gross pay per annum estimate adjusted by CPIH (2022 prices) (ONS)",
  "Mean household gross pay per annum estimate adjusted by CPIH (2022 prices) (ONS)",
  "Median to mean gross pay per annum ratio (indactor of skewness/inequality) (2022 prices) (ONS)",
  
  # Spending
  "Gross expenditure on children looked after (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on family support services (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on other services (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on safeguarding (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on services for young people (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on Sure Start and under 5s (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on youth justice services (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on non-CLA, non-safeguarding services (family support, services for young people, sure start and under 5s, and other) (SPPI Adjusted to 2021 Prices) (S251 + Revenue Outturns)",
  "Services Provider Price Inflation Index (SPPI) - rebased to 2022, as a multiplicative factor",
  "Percentage of total gross expenditure on children looked after",
  "Percentage of total gross expenditure on safeguarding",
  "Percentage of total gross expenditure on non-CLA, non-safeguarding",
  "Gross expenditure on children looked after per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on family support services per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on other services per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on safeguarding per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on services for young people per child aged 0-16 (SPPI Adjusted to 2022 Prices) (All children age 0-16) (S251 + Revenue Outturns)",
  "Gross expenditure on Sure Start and under 5s per child aged 0-16 (SPPI Adjusted to 2022 Prices) (All children age 0-16) (S251 + Revenue Outturns)",
  "Gross expenditure on youth justice services per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on non-CLA, non-safeguarding services per child aged 0-16 (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on children looked after per child looked after (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  "Gross expenditure on safeguarding per child on a child protection plan (SPPI Adjusted to 2022 Prices) (S251 + Revenue Outturns)",
  
  # IMD
  "Indices of Multiple Deprivation Score (IMD 2019)",
  "Income Deprivation Score (IMD 2019)",
  "Employment Deprivation Score (IMD 2019)",
  "Education Training and Skills Deprivation Score (IMD 2019)",
  "Health Deprivation Score (IMD 2019)",
  "Crime Deprivation Score (IMD 2019)",
  "Income Deprivation Affecting Children Index Score (IMD 2019)",
  "Indices of Multiple Deprivation Score (IMD 2015)",
  "Income Deprivation Score (IMD 2015)",
  "Employment Deprivation Score (IMD 2015)",
  "Education Training and Skills Deprivation Score (IMD 2015)",
  "Health Deprivation Score (IMD 2015)",
  "Crime Deprivation Score (IMD 2015)",
  "Income Deprivation Affecting Children Index Score (IMD 2015)",
  
  # Housing
  "Number of households",
  "Number of households in temporary accommodation (MHCLG Open Data)",
  "Number of homelessness applications rejected not homeless (MHCLG Open Data)",
  "Number of homelessness applications (MHCLG Open Data)",
  "Number of homelessness applications rejected not priority (MHCLG Open Data)",
  "Number of accepted homelessness applications (MHCLG Open Data)",
  "Number of homelessness applications rejected intentionally homeless (MHCLG Open Data)",
  "Number of homelessness applications any outcome (MHCLG Open Data)",
  "Number of households on the waitlist for social housing (MHCLG Open Data)",
  "Rate of households in temporary accommodation per 10,000 households (MHCLG Open Data)",
  "Rate of homelessness applications rejected not homeless per 10,000 households (MHCLG Open Data)",
  "Rate of homelessness applications per 10,000 households (MHCLG Open Data)",
  "Rate of homelessness applications rejected not priority per 10,000 households (MHCLG Open Data)",
  "Rate of accepted homelessness applications per 10,000 households (MHCLG Open Data)",
  "Rate of homelessness applications rejected intentionally homeless per 10,000 households (MHCLG Open Data)",
  "Rate of homelessness applications any outcome per 10,000 households (MHCLG Open Data)",
  "Rate of households on the waitlist for social housing per 10,000 households (MHCLG Open Data)",
  
  # Political party control
  "Number of years with Labour party local political control",
  "Number of years with Conservative party local political control",
  "Number of years with Liberal Democrat local party political control",
  "Number of years with independent local party political control",
  "Number of years with no overall local party political control", 
  "Political party (or lack of) with more than 8 years political control between 2010-2022"
)

merged_data_codebook <- tibble(var_names, var_labels) 

write_csv(merged_data_codebook, "tidied_data/merged_data_codebook.csv")

