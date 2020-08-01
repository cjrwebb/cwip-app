# LA Level CSC data 
library(tidyverse)
library(readxl)
library(janitor)
library(sf)

# start of data tidying ---------------------------------------------------

# 2010 - 2017 spending data 

# start of data tidying ---------------------------------------------------

legacy_data <- read_rds("Data/cwipdata2_long_temp.RDS")

legacy_data <- legacy_data %>% clean_names("upper_camel")

names(legacy_data) <- abbreviate(names(legacy_data), minlength = 12)

legacy_data

# import and tidy 2018 data

s251data2018 <- read_xlsx("Data/S251ye2018.xlsx")

# get rid of total categories and CERA to avoid double counting

exclusion_categories <- unique(s251data2018$Description)[27:36]

s251data2018 <- s251data2018 %>% filter(!Description %in% exclusion_categories)

s251data2018 <- s251data2018 %>% clean_names() %>%
  select(la, la_name, report_group, total_expenditure) %>%
  group_by(la_name, report_group) %>%
  summarise(
    la = first(la),
    total_expenditure = sum(total_expenditure)
  )


s251data2018 <- s251data2018 %>% mutate(
  report_group = abbreviate(report_group, minlength = 10)
) %>%
  mutate(
    general_rg = case_when(
      report_group == "CHILDRENLA" ~ "CLA",
      report_group == "SAFEGCAYPS" ~ "SG",
      TRUE ~ "NCLANSG"
    )
  )

# calculate totals

s251data2018 <- s251data2018 %>% group_by(
  la_name, general_rg
) %>%
  summarise(
    la = first(la),
    gen_exp = sum(total_expenditure)/100000
  )

#  Updated - looks about right

# 2018-19 spending data

s251data2019 <- read_xlsx("Data/S251ye2019.xlsx")

# Exclude totals
exclusion_categories <- unique(s251data2019$Description)[28:37]

s251data2019 <- s251data2019 %>% filter(!Description %in% exclusion_categories)

s251data2019 <- s251data2019 %>% clean_names() %>%
  select(la, la_name, report_group, total_expenditure) %>%
  group_by(la_name, report_group) %>%
  summarise(
    la = first(la),
    total_expenditure = sum(total_expenditure)
  )

s251data2019 <- s251data2019 %>% mutate(
  report_group = abbreviate(report_group, minlength = 10)
) %>%
  mutate(
    general_rg = case_when(
      report_group == "CHILDRENLA" ~ "CLA",
      report_group == "SAFEGCAYPS" ~ "SG",
      TRUE ~ "NCLANSG"
    )
  )

# calculate totals

s251data2019 <- s251data2019 %>% group_by(
  la_name, general_rg
) %>%
  summarise(
    la = first(la),
    gen_exp = sum(total_expenditure)/100000
  )

# Add 0-17 population estimates for 2017-18 and 2018-19 and CIN/CPP/CLA from CIN Census/CLA

# lots of opportunities with this much richer data

# mid-2017 estimates for 2017-18
cin_census <- read_csv("Data/cin_census_20122019.csv")

cin_census <- cin_census %>% filter(!is.na(la_name))

cin_census <- cin_census %>% filter(year %in% c("201819", "201718"))

unique(cin_census$metric)
unique(cin_census$source)
unique(cin_census$subset_type)
unique(cin_census$subset_category)

cin_census <- cin_census %>% filter(metric %in%
                                      c("Referrals in the year", 
                                        "Children in need at 31 March",
                                        "CPPs at 31 March")) %>%
  filter(value_type == "number") %>%
  filter(subset_order == 1 | subset_order == 3 & subset_type == "Re-referrals") %>%
  mutate(
    metric = ifelse(subset_type == "Re-referrals", "Re-referrals", metric)
  ) %>%
  group_by(la_name, year, metric) %>%
  summarise(
    level = first(level),
    old_la_code = first(old_la_code),
    new_la_code = first(new_la_code),
    value = first(value)
  )



# cin_census <- cin_census %>% filter(metric %in%
#                         c("Referrals in the year", 
#                           "Children in need at 31 March",
#                           "CPPs at 31 March")) %>%
#   filter(value_type == "number") %>%
#   filter(subset_order == 1) %>% 
#   group_by(la_name, year, metric) %>%
#   summarise(
#     level = first(level),
#     old_la_code = first(old_la_code),
#     new_la_code = first(new_la_code),
#     value = first(value)
#   )

# pivot wider across metric and year

cin_census <- cin_census %>% 
  pivot_wider(names_from = c("metric", "year"), values_from = "value") %>%
  clean_names()

names(cin_census)

names(cin_census)[5:12] <- c("CIN_31_1718", "CPP_31_1718", "REREF_1718", "REF_1718",
                             "CIN_31_1819", "CPP_31_1819", "REREF_1819", "REF_1819")

cin_census


# Add CLA and local authority population

CLA_data <- read_csv("Data/CLA2019.csv")

CLA_data <- CLA_data %>% filter(
  geog_l == "LA"
) %>% select(
  LA_code = geog_c,
  LA_name = geog_n,
  CLA_Mar2018,
  CLA_Mar2019,
  Pop2017 = CLA_Pop2017,
  Pop2018 = CLA_Pop2018
) %>%
  mutate(
    CLA_2018_rate = (CLA_Mar2018 / Pop2017) *10000,
    CLA_2019_rate = (CLA_Mar2019 / Pop2018) *10000,
    LA_code = as.numeric(LA_code)
  )


# Join cin and CLA data

csc_data <- left_join(cin_census, CLA_data %>% select(-LA_name), by = c("old_la_code" = "LA_code")) %>%
  mutate(
    REF_rate_1718 = (REF_1718 / Pop2017) *10000,
    REF_rate_1819 = (REF_1819 / Pop2017) *10000,
    REREF_rate_1718 = (REREF_1718 / REF_1718) *100,
    REREF_rate_1819 = (REREF_1819 / REF_1819) *100,
    CIN_rate_1718 = (CIN_31_1718 / Pop2017) *10000,
    CIN_rate_1819 = (CIN_31_1819 / Pop2018) *10000,
    CPP_rate_1718 = (CPP_31_1718 / Pop2017) *10000,
    CPP_rate_1819 = (CPP_31_1819 / Pop2018) *10000
  )

csc_data

# Join S251 datas
s251data2018 <- s251data2018 %>% 
  pivot_wider(names_from = general_rg, values_from = gen_exp, names_prefix = "EXP_") %>%
  mutate(EXP_TOT = sum(EXP_CLA, EXP_NCLANSG, EXP_SG))

s251data2019 <- s251data2019 %>% 
  pivot_wider(names_from = general_rg, values_from = gen_exp, names_prefix = "EXP_") %>%
  mutate(EXP_TOT = sum(EXP_CLA, EXP_NCLANSG, EXP_SG))



s251_1719 <- left_join(s251data2018, s251data2019, 
                       by = "la", suffix = c("_1718", "_1819")) %>%
  select(-la_name_1819) %>%
  rename(la_name = la_name_1718) 


# Join S251 to CSC data

csc_s251 <- left_join(csc_data, s251_1719, by = c("old_la_code" = "la")) %>%
  rename(la_name = la_name.x) %>%
  select(-la_name.y)

# calculate spend per child and change to long format

longer_names <- gsub(x = names(csc_s251)[5:26], "1[7-9]1[7-9]", "")
longer_names <- gsub(x = longer_names, pattern = "201[7-9]", "")
longer_names <- gsub(x = longer_names, pattern = "_", "")
longer_names <- unique(longer_names)


csc_s251_long <- csc_s251 %>% ungroup() %>%
  pivot_longer(cols = CIN_31_1718:EXP_TOT_1819, 
               names_to = "metric",
               values_to = "value") %>%
  mutate(year = ifelse(str_detect(metric, "1718") | str_detect(metric, "2017")
                       | str_detect(metric, "CLA_Mar2018") | str_detect(metric, "CLA_2018"), 
                       2018, 2019)) %>%
  mutate(metric = str_remove_all(metric, "1[7-9]1[7-9]") %>%
           str_remove_all("201[7-9]") %>%
           str_remove_all("_")
  ) %>%
  pivot_wider(names_from = metric, values_from = value)

csc_s251_long <- csc_s251_long %>% mutate(
  tot_exp_pc = (EXPTOT*100000) / Pop,
  sg_exp_pc = (EXPSG*100000) / Pop,
  nonsgcla_exp_pc = (EXPNCLANSG*100000) / Pop,
  cla_exp_pcla = (EXPCLA*100000) / CLAMar
)

# Check spending

ggplot(csc_s251_long) +
  geom_line(aes(x = year, y = nonsgcla_exp_pc, group = la_name)) +
  ylim(c(0,500))

ggplot(csc_s251_long) +
  geom_line(aes(x = year, y = cla_exp_pcla, group = la_name)) 

# looks okay

names(csc_s251_long)

# Tidy up and find a way to join legacy data - can join with geog code

legacy_data$GeogCode

names(legacy_data)

legacy_data <- legacy_data %>%
  select(new_la_code = GeogCode,
         year = Time,
         EXPTOT = TE100_000S20,
         EXPCLA = LACE100_000S,
         EXPSG = SE100_000S20,
         EXPNCLANSG = NLNSE100_000,
         CLArate = LkACRP10_000,
         REFrate = RfrrRP10_000,
         REREFrate = PrcnORRRW12M,
         CINrate = ChINRP10_000,
         CPPrate = ChPPRP10_000,
         Pop = ChilpoPt1,
         CLAMar = LacpoPt1
  ) %>%
  mutate(
    year = 2010 + year,
    Pop = Pop*1000
  )


legacy_data

la_name_lookup <- csc_s251 %>% group_by(new_la_code) %>%
  summarise(la_name = first(la_name))

legacy_data <- legacy_data %>%
  mutate(
    new_la_code = case_when(new_la_code == "E08000037" ~ "E08000020",
                            new_la_code == "E06000057" ~ "E06000048",
                            TRUE ~ new_la_code)
  )


legacy_data <- left_join(legacy_data, la_name_lookup, by = "new_la_code")

legacy_data


# Merge legacy data and new data

combined_csc_data <- bind_rows(legacy_data, csc_s251_long)


# **Adjust for inflation and recalculate all rates**

deflator <- read_xlsx("Data/GDP_deflator.xlsx")

deflator <- deflator %>% mutate(
  delfator_adjusted = ifelse(year < 2018,
                             deflator$deflator_2019[7],
                             deflator_2019)
)



combined_csc_data <- combined_csc_data %>%
  mutate_at(
    vars(EXPTOT:EXPNCLANSG),
    ~ifelse(year < 2018,
            . * deflator$delfator_adjusted[7],
            ifelse(year == 2018, . * deflator$delfator_adjusted[8],
                   .))
  )


# recalculate spend per child

combined_csc_data <- combined_csc_data %>% mutate(
  tot_exp_pc = (EXPTOT*100000) / Pop,
  sg_exp_pc = (EXPSG*100000) / Pop,
  nonsgcla_exp_pc = (EXPNCLANSG*100000) / Pop,
  cla_exp_pcla = (EXPCLA*100000) / CLAMar
)

imd_2015 <- read_xlsx("Data/LA_IMD.xlsx", sheet = 2) %>%
  clean_names() 

imd_2015 <- imd_2015 %>% select(
  la_code = upper_tier_local_authority_district_code_2013,
  imd2015 = imd_average_score
)

combined_csc_data <- left_join(combined_csc_data, imd_2015, by = c("new_la_code" = "la_code"))

# View(combined_csc_data)

combined_csc_data <- combined_csc_data %>%
  mutate(
    imd2015 = case_when(
      la_name == "Gateshead" ~ 25.932,
      la_name == "Northumberland" ~ 20.525,
      TRUE ~ imd2015
    )
  )

combined_csc_data <- combined_csc_data %>%
  select(-level:-REF) %>%
  select(new_la_code, la_name, everything())

combined_csc_data <- combined_csc_data %>%
  mutate_at(vars(EXPCLA, EXPSG, EXPNCLANSG), list(as_pc = ~(. / EXPTOT) * 100)) %>%
  select(new_la_code:EXPNCLANSG, tot_exp_pc:cla_exp_pcla, EXPCLA_as_pc:EXPNCLANSG_as_pc, CLArate:CPPrate, Pop, imd2015)

names(combined_csc_data)


# 2012 onward CIN data only?

cin_census <- read_csv("Data/cin_census_20122019.csv")

cin_census <- cin_census %>% filter(!is.na(la_name))

unique(cin_census$metric)
unique(cin_census$source)
unique(cin_census$subset_type)
unique(cin_census$subset_category)
unique(cin_census$subset_order)
unique(cin_census$value_type)
unique(cin_census$la_name)
unique(cin_census$year)

cin_census_variables <- cin_census %>%
  filter(value_type %in% c("percentage", "rate", "percentile")) %>%
  mutate(
    description = paste0(metric, " - ", subset_type, " - ", subset_category, " (", value_type, ")")
  ) %>%
  arrange(value_type) %>%
  .$description %>%
  unique(.)


cin_census_simplified <- cin_census %>%
  filter(value_type %in% c("percentage", "rate", "percentile")) %>%
  mutate(
    description = paste0(metric, " - ", subset_type, " - ", subset_category, " (", value_type, ")"),
    year = as.numeric(str_sub(as.character(year), end = -3)) + 1
  ) %>%
  arrange(value_type) %>%
  select(year, new_la_code, la_name, description, value)

unique(cin_census_simplified$year)

cin_census_variables[1]

cin_census_simplified %>%
  filter(description == cin_census_variables[3]) %>%
  ggplot() +
  geom_line(aes(group = la_name, x = year, y = value)) +
  ylab(cin_census_variables[3]) +
  xlab("Year Ending")

cin_census_simplified
names(cin_census_simplified)

# simplify CLA returns

# 2015 - 2019
cla_data <- read_csv("Data/CLA2019.csv")
cla_data
names(cla_data)
cla_data$CLA_Rate2015
cla_data$CLA_Mar2015
unique(cla_data$geog_l)

cla_data %>% 
  mutate_at(vars(CLA_Mar2015:CLA_RPC_Other_pc), ~as.numeric(.)) %>%
  pivot_longer(cols = CLA_Mar2015:CLA_RPC_Other_pc, names_to = "description") %>%
  select(new_la_code = New_geog_code, la_name = geog_n, description, value)

# get descriptions to join on - not very good - only really rates over years

# Reshape combined into long
combined_varnames <- tibble(varnames = names(combined_csc_data),
       vardesc = c(
         "LA Code",
         "LA Name",
         "Year",
         "Expenditure on Childrens and Young Peoples Services (Total, £100,000s in 2019 prices)",
         "Expenditure on Children Looked After (Total, £100,000s in 2019 prices)",
         "Expenditure on Safeguarding (Total, £100,000s in 2019 prices)",
         "Expenditure on Early Help/Family Support (non-CLA, non-SG) (Total, £100,000s in 2019 prices)",
         "Total Expenditure per Child Aged 0-17 (£ per child in 2019 prices)",
         "Safeguarding Expenditure per Child Aged 0-17 (£ per child in 2019 prices)",
         "Early Help/Family Support (non-SG, non-CLA) Expenditure per Child Aged 0-17 (£ per child in 2019 prices)",
         "Children Looked After Expenditure per Child Looked After Aged 0-17 (£ per CLA in 2019 prices)",
         "CLA Expenditure as a Percentage of All Expenditure",
         "Safeguarding Expenditure as a Percentage of All Expenditure",
         "Early Help/Family Support (non-SG, non-CLA) Expenditure as a Percentage of All Expenditure",
         "Children Looked After Rate per 10,000",
         "Referral Rate per 10,000 Aged 0-17",
         "Re-referral Rate per 100 referrals",
         "Children in Need rate per 10,000 Aged 0-17",
         "Child Protection Plan rate per 10,000 Aged 0-17",
         "Total Population Aged 0-17",
         "Indices of Multiple Deprivation Score 2015"
       ))


csc_data_long_simplified <- combined_csc_data %>%
  pivot_longer(cols = EXPTOT:imd2015, names_to = "description") %>%
  left_join(., combined_varnames, by = c("description" = "varnames")) %>%
  mutate(description = vardesc) %>%
  select(-vardesc) 


# bind into single table


names(cin_census_simplified)
names(csc_data_long_simplified)

csc_all <- bind_rows(csc_data_long_simplified, cin_census_simplified)

unique(csc_all$description)

# Add LA shapefile data
la_sf <- read_sf("data/la_sf") %>%
  st_simplify(dTolerance = 300) %>%
  st_transform(crs = 4326) %>%
  st_cast("MULTIPOLYGON")

class(la_sf)

la_sf

# Better to keep separate and just join choices to shapefile when plotting
#csc_all_sf <- left_join(la_sf, csc_all, by = c("ctyua19cd" = "new_la_code"))


la_sf_centroids <- st_centroid(la_sf)

la_sf_centroids


ggplot() + 
  geom_sf(data = la_sf, fill = 'white', size = 0.1) +
  geom_sf(data = la_sf_centroids %>% group_by(ctyua19cd) %>%
            slice(1) %>% ungroup(), color = 'blue') 

region_lookup <- read_rds("Data/cwipdata2_long_temp.RDS") %>%
  select(geog_code, LA, Region) %>%
  mutate(
    new_la_code = case_when(geog_code == "E08000037" ~ "E08000020",
                            geog_code == "E06000057" ~ "E06000048",
                            TRUE ~ geog_code)
  ) %>%
  select(new_la_code, LA, Region)

#View(region_lookup)

la_sf %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  filter(is.na(Region)) %>%
  .$ctyua19nm


la_sf <- la_sf %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  mutate(
    Region = case_when(
      ctyua19nm == "Northumberland" ~ "North East",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "South West", 
      ctyua19nm == "Dorset" ~ "South West",
      ctyua19nm == "Gateshead" ~ "North East",
      TRUE ~ Region
    ),
    ctyua19cd = case_when(
      ctyua19nm == "Northumberland" ~ "E06000048",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "E06000028",
      ctyua19nm == "Dorset" ~ "E10000009",
      ctyua19nm == "Gateshead" ~ "E08000020",
      TRUE ~ ctyua19cd
    )
  ) 

la_sf_centroids <- la_sf_centroids %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  mutate(
    Region = case_when(
      ctyua19nm == "Northumberland" ~ "North East",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "South West", 
      ctyua19nm == "Dorset" ~ "South West",
      ctyua19nm == "Gateshead" ~ "North East",
      TRUE ~ Region
    ),
    ctyua19cd = case_when(
      ctyua19nm == "Northumberland" ~ "E06000048",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "E06000028",
      ctyua19nm == "Dorset" ~ "E10000009",
      ctyua19nm == "Gateshead" ~ "E08000020",
      TRUE ~ ctyua19cd
    )
  ) 


la_sf_centroids <- la_sf_centroids %>% group_by(ctyua19cd) %>%
  slice(1) %>% ungroup()

# Population size adjusted cartogram



la_sf_cartdata <- read_sf("data/la_sf") %>%
  # st_simplify(dTolerance = 300) %>%
  st_transform(crs = 4326) %>%
  st_cast("MULTIPOLYGON") %>%
  left_join(., csc_all %>% filter(description == "Total Population Aged 0-17") %>% group_by(new_la_code) %>%
              summarise(total_chil_pop = mean(value, na.rm = TRUE)),
            by = c("ctyua19cd" = "new_la_code")) %>%
  st_transform(crs = 3857) %>%
  filter(!is.na(total_chil_pop))

la_sf_cartdata <- cartogram::cartogram_dorling(la_sf_cartdata, weight = "total_chil_pop")

la_sf_centroids_cart <- st_centroid(la_sf_cartdata, of_largest_polygon = TRUE) %>% st_transform(crs = 4326)

la_sf_cartdata <- la_sf_cartdata %>% st_simplify(dTolerance = 300) %>% st_transform(crs = 4326)

la_sf_cartdata %>%
  ggplot() +
  geom_sf()




la_sf_centroids_cart


ggplot() + 
  geom_sf(data = la_sf_cartdata, fill = 'white', size = 0.1) +
  geom_sf(data = la_sf_centroids_cart %>% group_by(ctyua19cd) %>%
            slice(1) %>% ungroup(), color = 'blue') 

region_lookup <- read_rds("Data/cwipdata2_long_temp.RDS") %>%
  select(geog_code, LA, Region) %>%
  mutate(
    new_la_code = case_when(geog_code == "E08000037" ~ "E08000020",
                            geog_code == "E06000057" ~ "E06000048",
                            TRUE ~ geog_code)
  ) %>%
  select(new_la_code, LA, Region)

#View(region_lookup)

la_sf_cartdata %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  filter(is.na(Region)) %>%
  .$ctyua19nm


la_sf_cartdata <- la_sf_cartdata %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  mutate(
    Region = case_when(
      ctyua19nm == "Northumberland" ~ "North East",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "South West", 
      ctyua19nm == "Dorset" ~ "South West",
      ctyua19nm == "Gateshead" ~ "North East",
      TRUE ~ Region
    ),
    ctyua19cd = case_when(
      ctyua19nm == "Northumberland" ~ "E06000048",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "E06000028",
      ctyua19nm == "Dorset" ~ "E10000009",
      ctyua19nm == "Gateshead" ~ "E08000020",
      TRUE ~ ctyua19cd
    )
  ) 

la_sf_centroids_cart <- la_sf_centroids_cart %>% left_join(region_lookup, by = c("ctyua19cd" = "new_la_code")) %>%
  mutate(
    Region = case_when(
      ctyua19nm == "Northumberland" ~ "North East",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "South West", 
      ctyua19nm == "Dorset" ~ "South West",
      ctyua19nm == "Gateshead" ~ "North East",
      TRUE ~ Region
    ),
    ctyua19cd = case_when(
      ctyua19nm == "Northumberland" ~ "E06000048",
      ctyua19nm == "Bournemouth, Christchurch and Poole" ~ "E06000028",
      ctyua19nm == "Dorset" ~ "E10000009",
      ctyua19nm == "Gateshead" ~ "E08000020",
      TRUE ~ ctyua19cd
    )
  ) 


la_sf_centroids_cart <- la_sf_centroids_cart %>% group_by(ctyua19cd) %>%
  slice(1) %>% ungroup()

ggplot() + 
  geom_sf(data = la_sf_cartdata, fill = 'white', size = 0.1) +
  geom_sf(data = la_sf_centroids_cart %>% group_by(ctyua19cd) %>%
            slice(1) %>% ungroup(), color = 'blue') 


# Get ethnic inequalities data (CLA)

read_csv("Data/CLA2019.csv") %>%
  names(.)

cla_2019_ethnicity <- read_csv("Data/CLA2019.csv") %>%
  select(New_geog_code, geog_n, CLA_White:CLA_Oth)

cla_2019_ethnicity <- cla_2019_ethnicity %>%
  slice(-1:-12) %>%
  mutate_at(vars(CLA_White:CLA_Oth), ~as.numeric(ifelse(. == "c", NA, .)))

ethnicity_populations <- read_rds("Data/cwipdata2_long_temp.RDS") %>%
  select(LA, time, WhAll_tot1:OthOth_tot1) %>%
  filter(time == 1)


# ONS population growth:
pop_growth_2011.2019 <- 66796800 / 63285100

ethnicity_populations <- ethnicity_populations %>%
  mutate_at(vars(WhAll_tot1:OthOth_tot1), ~round(. * pop_growth_2011.2019, 1)) %>%
  select(LA, WhAll_tot1, MxAll_tot1, AsAll_tot1, BlAll_tot1, EthOthAll_tot1)

ethnicity_populations

sort(cla_2019_ethnicity$geog_n)

cla_2019_ethnicity <- cla_2019_ethnicity %>%
  mutate(geog_n = case_when(geog_n == "Bedford Borough" ~ "Bedford",
                            TRUE ~ geog_n))

cla_eth_data <- left_join(ethnicity_populations, cla_2019_ethnicity, by = c("LA" = "geog_n")) 

cla_eth_data <- cla_eth_data %>%
  select(LA, New_geog_code, everything()) %>%
  mutate(
    white_cla_rate = (CLA_White / WhAll_tot1) * 10000,
    mixed_cla_rate = (CLA_Mixed / MxAll_tot1) * 10000,
    asian_cla_rate = (CLA_Asian / AsAll_tot1) * 10000,
    black_cla_rate = (CLA_Black / BlAll_tot1) * 10000,
    other_cla_rate = (CLA_EOTH / EthOthAll_tot1) * 10000
  ) %>%
  select(LA, New_geog_code, white_cla_rate:other_cla_rate) %>%
  mutate_at(vars(mixed_cla_rate:other_cla_rate), list(relative = ~ . / white_cla_rate)) %>%
  pivot_longer(white_cla_rate:other_cla_rate_relative, names_to = "description") %>%
  mutate(
    description = case_when(
      description == "white_cla_rate" ~ "Estimated children looked after rate per 10,000 aged 0-17 for White British Children (2019)",
      description == "mixed_cla_rate" ~ "Estimated children looked after rate per 10,000 aged 0-17 for Mixed Heritage Children (2019)",
      description == "asian_cla_rate" ~ "Estimated children looked after rate per 10,000 aged 0-17 for Asian Children (2019)",
      description == "black_cla_rate" ~ "Estimated children looked after rate per 10,000 aged 0-17 for Black Children (2019)",
      description == "other_cla_rate" ~ "Estimated children looked after rate per 10,000 aged 0-17 for 'Other' Ethnicity Children (2019)",
      description == "mixed_cla_rate_relative" ~ "Estimated rate ratio of Mixed Heritage children looked after rate per 10,000 aged 0-17 for every 1 White British Child looked after per 10,000 (2019)",
      description == "asian_cla_rate_relative" ~ "Estimated rate ratio of Asian children looked after rate per 10,000 aged 0-17 for every 1 White British Child looked after per 10,000 (2019)",
      description == "black_cla_rate_relative" ~ "Estimated rate ratio of Black children looked after rate per 10,000 aged 0-17 for every 1 White British Child looked after per 10,000 (2019)",
      description == "other_cla_rate_relative" ~ "Estimated rate ratio of 'Other ethnic group' children looked after rate per 10,000 aged 0-17 for every 1 White British Child looked after per 10,000 (2019)"
    )
  )


cla_eth_data

#write_rds(cla_eth_data, "data/csc_ethnicity_data.RDS")


# Save files as rds

#write_rds(csc_all, "data/csc_data.RDS")
#write_rds(la_sf, "data/la_sf.RDS")
#write_rds(la_sf_centroids, "data/la_sf_centroids.RDS")
# write_rds(la_sf_cartdata, "data/la_sf_cart.RDS")
# write_rds(la_sf_centroids_cart, "data/la_sf_centroids_cart.RDS")


# For DFE 
# log_prob_example <- function (a, b, x) {exp(a + (b*x)) / (exp(a + (b*x)) + 1) }
# 
# log_prob_example(a = -2.1972, b = 0.049, x = 0)
# 
# for (i in 0:50) {
# 
#   print(i)
#   odds <- log_prob_example(a = -2.1972, b = 0.049, x = i)
#   print(odds)
#   
# }
# 




