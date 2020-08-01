# Spending update analysis
library(tidyverse)
library(janitor)
library(readxl)
library(MplusAutomation)
library(plotly)


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

# Data visualisation

combined_csc_data %>% filter(la_name != "City of London") %>% ggplot() +
  geom_line(aes(x = year, y = nonsgcla_exp_pc, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  scale_y_continuous(breaks = seq(0, 1500, 250))

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = sg_exp_pc, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = cla_exp_pcla, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CINrate - (CLArate + CPPrate), group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CPPrate, group = la_name),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CLArate, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CPPrate + CLArate, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = REFrate, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = REREFrate, group = new_la_code),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 


combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  group_by(year) %>%
  summarise(
    CINrate = mean(CINrate, na.rm = TRUE),
    supp_spend_pc = mean(nonsgcla_exp_pc, na.rm = TRUE)
  )

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  group_by(year) %>%
  summarise(
    CINrate = mean(CINrate, na.rm = TRUE),
    supp_spend_pc = mean(nonsgcla_exp_pc, na.rm = TRUE)
  )



# Add IMD2015

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

combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  ggplot() +
  geom_line(aes(x = year, y = CLArate, group = la_name, col = imd2015),
            alpha = 0.4) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) 


combined_csc_data %>% 
  filter(!la_name %in% c("City of London", "Isles Of Scilly")) %>% 
  mutate(
    dprv_tert = ntile(imd2015, 3)
  ) %>%
  group_by(year, dprv_tert) %>%
  summarise(
    CINrate = mean(CINrate, na.rm = TRUE),
    supp_spend_pc = mean(nonsgcla_exp_pc, na.rm = TRUE)
  ) %>%
  pivot_wider(id = year, names_from = dprv_tert, values_from = CINrate:supp_spend_pc)

# Add neglect data

#saveRDS(combined_csc_data, "combined_csc_data.RDS")


# CIN ALTM-SR ---------------------------------------------------------------


analysis_data <- combined_csc_data %>%
  filter(!la_name %in% c("City of London", "Isles Of Scilly"))

# London lookup

london_lookup <- read_csv("Data/cin_census_20122019.csv")
london_lookup <- london_lookup %>% filter(!is.na(la_name))

london_lookup <- london_lookup %>% group_by(la_name) %>%
  summarise(
    region_name = first(region_name)
  )

analysis_data <- left_join(analysis_data, london_lookup, by = "la_name")

analysis_data <- analysis_data %>% mutate(
  InrLdn = ifelse(region_name == "Inner London", 1, 0)
)

# Pivot wider with year and select only variables for Mplus
names(analysis_data)

analysis_data_mplus <- analysis_data %>% ungroup() %>%
  mutate(
    CINrate = CINrate - (CLArate) #+ CPPrate)
  ) %>%
  select(
  la_name, year, CINrate, nonsgcla_exp_pc, imd2015, InrLdn
)

analysis_data_mplus <- analysis_data_mplus %>% 
  pivot_wider(names_from = "year", 
              values_from = c("CINrate", "nonsgcla_exp_pc"))


names(analysis_data_mplus) <- 
  str_replace_all(tolower(abbreviate(str_remove_all(names(analysis_data_mplus), "_"), 8)),
                  "cinrate20", "cin") %>%
  str_remove_all("20")

# analysis_data_mplus <- analysis_data_mplus %>% select(-imd15)

# Exclude haringey tp1 as an outlier for CIN in t1

analysis_data_mplus <- analysis_data_mplus %>% filter(laname != "Haringey") %>%
  mutate(
    imd15 = scale(imd15)[,1]
  ) 

#analysis_data_mplus <- analysis_data_mplus %>% mutate(
#  cinr11 = ifelse(laname == "Haringey", NA, cinr11)
#) 


# FINAL MODEL -------------------------------------------------------------

mplus_model <- mplusObject(rdata = analysis_data_mplus, autov = TRUE,
                           TITLE = "RIRS-CLPM NonSG NonLAC Spend and Int Rates",
                           MODEL = "
  
  ! Create two random intercept factors
  RI_cinr BY cinr11@1 cinr12@1 cinr13@1
              cinr14@1 cinr15@1 cinr16@1
              cinr17@1 cinr18@1 cinr19@1;
              
  RI_nnsg BY nnsg11@1 nnsg12@1 nnsg13@1
              nnsg14@1 nnsg15@1 nnsg16@1
              nnsg17@1 nnsg18@1 nnsg19@1;
              
  ! Create two random slope factors
  
  RS_cinr BY cinr11@0 cinr12@1 cinr13@2
              cinr14@3 cinr15@4 cinr16@5
              cinr17@6 cinr18@7 cinr19@8;
              
  RS_nnsg BY nnsg11@0 nnsg12@1 nnsg13@2
              nnsg14@3 nnsg15@4 nnsg16@5
              nnsg17@6 nnsg18@7 nnsg19@8;
              
  ! Create random quadratic factor
  
  ! RQ_nnsg BY nnsg11@0 nnsg12@1 nnsg13@4
  !            nnsg14@9 nnsg15@16 nnsg16@25
  !            nnsg17@36 nnsg18@49 nnsg19@64;
              
  ! RQ_cinr BY cinr11@0 cinr12@1 cinr13@4
  !            cinr14@9 cinr15@16 cinr16@25
  !            cinr17@36 cinr18@49 cinr19@64;
  
  
  ! Create within-local authority centred variables (latent)
  
  lcin11 BY cinr11@1; 
  lcin12 BY cinr12@1; 
  lcin13 BY cinr13@1;
  lcin14 BY cinr14@1; 
  lcin15 BY cinr15@1; 
  lcin16 BY cinr16@1;
  lcin17 BY cinr17@1; 
  lcin18 BY cinr18@1; 
  lcin19 BY cinr19@1;
  
  lspen11 BY nnsg11@1; 
  lspen12 BY nnsg12@1; 
  lspen13 BY nnsg13@1;
  lspen14 BY nnsg14@1; 
  lspen15 BY nnsg15@1; 
  lspen16 BY nnsg16@1;
  lspen17 BY nnsg17@1; 
  lspen18 BY nnsg18@1; 
  lspen19 BY nnsg19@1;
  
  ! Constrain measurement error variances to 0
  cinr11-nnsg19@0;
  lcin11-lcin19;
  lspen11-lspen19;
  RI_cinr;
  RI_nnsg;
  RS_cinr;
  RS_nnsg;
  
  ! Optional: Constrain observed means per variable over time 
  [nnsg11-nnsg19@0]; 
  [cinr11-cinr19@0];
  [lcin11-lcin19@0];
  [lspen11-lspen19@0];
  [RI_cinr RI_nnsg];
  [RS_cinr RS_nnsg];
  ![RQ_nnsg];
  ![RQ_cinr];
  
  ! Estimate the lagged effects between
  ! the within-LA centered variables
  lcin12 ON lcin11 lspen11 (a g);
  lcin13 ON lcin12 lspen12 (a g);
  lcin14 ON lcin13 lspen13 (a g);
  lcin15 ON lcin14 lspen14 (a g);
  lcin16 ON lcin15 lspen15 (a g);
  lcin17 ON lcin16 lspen16 (a g);
  lcin18 ON lcin17 lspen17 (a g);
  lcin19 ON lcin18 lspen18 (a g);
  
  lspen12 ON lspen11 lcin11 (b d);
  lspen13 ON lspen12 lcin12 (b d);
  lspen14 ON lspen13 lcin13 (b d);
  lspen15 ON lspen14 lcin14 (b d);
  lspen16 ON lspen15 lcin15 (b d);
  lspen17 ON lspen16 lcin16 (b d);
  lspen18 ON lspen17 lcin17 (b d);
  lspen19 ON lspen18 lcin18 (b d);
  
  ! Estimate the covariance between the within‐person 
  ! centered variables at the first wave
  lcin11 WITH lspen11;
  
  ! Estimate the residual covariances at the same
  ! time points 
  lspen12 WITH lcin12 (l);
  lspen13 WITH lcin13 (l);
  lspen14 WITH lcin14 (l);
  lspen15 WITH lcin15 (l);
  lspen16 WITH lcin16 (l);
  lspen17 WITH lcin17 (l);
  lspen18 WITH lcin18 (l);
  lspen19 WITH lcin19 (l);
  
  ! Fix the correlation between the individual factors and the other
  ! exogenous variables to zero (by default these would be estimated)
  ! Only for latent factors
  
  RI_cinr WITH lcin11@0 lspen11@0 lcin19@0 lspen19@0;
  RS_cinr WITH lcin11@0 lspen11@0 lcin19@0 lspen19@0;
  !RQ_cinr WITH lcin11@0 lspen11@0;
  RI_cinr ON imd15 inrldn;
  RS_cinr ON imd15@0 inrldn;
  !RQ_cinr ON imd15;
  imd15 WITH inrldn;
  imd15 WITH lcin11@0 lspen11@0;
  inrldn WITH lcin11@0 lspen11@0;
  
  RI_nnsg WITH lcin11@0 lspen11@0 lcin19@0 lspen19@0;
  RS_nnsg WITH lcin11@0 lspen11@0 lcin19@0 lspen19@0;
  !RQ_nnsg WITH lcin11@0 lspen11@0;
  RI_nnsg ON imd15 inrldn;
  RS_nnsg ON imd15 inrldn;
  !RQ_nnsg ON imd15;
  
  ! Fix the variances and covariance of the random intercepts to zero RI_x@0;
  !RI_cinr@0;
  !RI_nnsg WITH RI_cinr@0;
  
  !RS_cinr@0;
  !RS_nnsg WITH RS_cinr@0;
  
  !RQ_nnsg@0;
  !RQ_cinr@0;

  ",
 OUTPUT = "TECH1 STDYX CINT(bcbootstrap)",
 ANALYSIS = "estimator = ml;
             iterations = 10000;
             bootstrap = 5000"
                             )

mplusModeler(mplus_model, dataout = "spend_cin_model.dat")


runModels("spend_cin_model.inp")


# Read in model results in Mplus

cin_spend_model <- readModels(target = "spend_cin_model.out", what = "parameters")

cin_spend_model$parameters$unstandardized
cin_spend_model$parameters$stdyx.standardized
cin_spend_model$parameters$ci.unstandardized %>% select(paramHeader, param, low2.5, up2.5)
cin_spend_model$parameters$ci.stdyx.standardized %>% select(paramHeader, param, low2.5, up2.5) %>%
  filter(str_detect(paramHeader, "RI") | str_detect(paramHeader, "RS"))

cin_spend_model_fit <- readModels(target = "spend_cin_model.out", what = "summaries")

cin_spend_model_fit$summaries$CFI
cin_spend_model_fit$summaries$SRMR
cin_spend_model_fit$summaries$TLI


cin_spend_model$parameters$stdyx.standardized %>% 
  filter(str_detect(paramHeader, "LCIN") & str_detect(param, "LSPEN"))

cin_spend_model$parameters$ci.stdyx.standardized %>% 
  select(paramHeader, param, low2.5, up2.5) %>% 
  filter(str_detect(paramHeader, "LCIN") & str_detect(param, "LSPEN"))




