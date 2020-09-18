library(tidyverse)
library(glmmTMB)
library(ggtext)
library(patchwork)

# Load CSC and deprivation data
csc_data <- read_rds("data/csc_data_v3.RDS") %>%
  mutate(value = ifelse(is.infinite(value), NA, value)) %>%
  filter(!la_name %in% c("City of London", "City Of London", "Isles of Scilly", "Isles Of Scilly"))

csc_vars <- unique(csc_data$description)

csc_vars[5] # Total spend per child
csc_vars[7] # EH/FS Spend per child
csc_vars[18] # Deprivation
csc_vars[17] # Total child population
  
# Get SEN data

sen1 <- readxl::read_xlsx("data/sen_data.xlsx", sheet = 1)
sen2 <- readxl::read_xlsx("data/sen_data.xlsx", sheet = 2)

sen_data <- sen1 %>% left_join(sen2, by = c("la_code", "la_name"))

sen_data <- sen_data %>%
  select(sort(tidyselect::peek_vars())) %>%
  select(la_code, la_name, everything()) %>%
  pivot_longer(pupsennostatem2011:totalpup2019) %>%
  mutate(year = str_extract(name, "[0-9][0-9][0-9][0-9]"),
         name = str_remove_all(name, "[0-9][0-9][0-9][0-9]")) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(total_send = pupsennostatem + pupsenstatement,
         year = as.double(year))

# Tidy up CSC for modelling

ce_data <- csc_data %>%
  filter(description %in% c(csc_vars[5], csc_vars[7], csc_vars[18], csc_vars[17])) %>%
  pivot_wider(names_from = description, values_from = value) %>%
  rename(tot_exp_pc = 4, ehfs_exp_pc = 5, tot_chil_pop = 6, imd_sc = 7)

ce_data <- ce_data %>% 
  left_join(sen_data %>% select(la_code, year, total_send),  
                      by = c("new_la_code" = "la_code", "year")) %>%
  mutate(send_pc_pop = total_send / tot_chil_pop * 100)

ce_data

# Models are tot_exp_pc | ehfs_exp_pc ~ imd_sc + send_pc_pop + tot_chil_pop

# pre-model checks

GGally::ggpairs(ce_data, columns = c("tot_exp_pc", "ehfs_exp_pc", "imd_sc", "send_pc_pop", "tot_chil_pop"))

# Some possible leverage points in Spending per child - look out for outliers

ce_data <- ce_data %>%
  mutate(year = as.character(year))

tot_spen_model <- glmmTMB(tot_exp_pc ~ scale(imd_sc) + send_pc_pop + I(tot_chil_pop/100) + (1 | year),
                 data = ce_data)

summary(tot_spen_model)

MuMIn::r.squaredGLMM(tot_spen_model)


eh_spen_model <- glmmTMB(ehfs_exp_pc ~ scale(imd_sc) + send_pc_pop + I(tot_chil_pop/100) + (1 | year),
                          data = ce_data)

summary(eh_spen_model)

MuMIn::r.squaredGLMM(eh_spen_model)

# Extract effect of underlying need factors on funding - e.g. if funding was 
# exactly proportional to underlying need they would all coverge

summary(tot_spen_model)

ce_data <- ce_data %>%
  mutate(
    tot_exp_pc_adjusted = tot_exp_pc - (142.67 * scale(imd_sc)[,1] + 28.89 * send_pc_pop + -0.088317 * (tot_chil_pop/100)),
    ehfs_exp_pc_adjusted = ehfs_exp_pc - (49.736 * scale(imd_sc)[,1] + 9.051 * send_pc_pop + -0.029313 * (tot_chil_pop/100))
  ) 


# Graph results
# regular line graph

# 5 = most deprived 30, 1 = least deprived 30, 3 = middle deprived 30
ce_data_quintiles <- ce_data %>%
  mutate(imd_quintile = ntile(x = imd_sc, n = 5)) %>% 
  group_by(imd_quintile, year) %>%
  summarise(tot_exp_pc = mean(tot_exp_pc, na.rm = TRUE),
            ehfs_exp_pc = mean(ehfs_exp_pc, na.rm = TRUE),
            tot_exp_pc_adjusted = mean(tot_exp_pc_adjusted, na.rm = TRUE),
            ehfs_exp_pc_adjusted = mean(ehfs_exp_pc_adjusted, na.rm = TRUE)) %>%
  filter(imd_quintile %in% c(1, 3, 5))

ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc, 
                group = la_name, col = imd_sc), alpha = 0.2) +
  ggnewscale::new_scale_color() +
  geom_line(data = ce_data_quintiles,
            aes(x = year, y = tot_exp_pc, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2)

ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name, col = imd_sc), alpha = 0.2) +
  ggnewscale::new_scale_color() +
  geom_line(data = ce_data_quintiles,
            aes(x = year, y = tot_exp_pc_adjusted, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) 


ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = tot_exp_pc, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) 


ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = tot_exp_pc_adjusted, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) 


ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = ehfs_exp_pc, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) 


ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = ehfs_exp_pc_adjusted, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) 



ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Blackpool", "Wokingham")),
            aes(x = year, y = tot_exp_pc, 
                group = la_name), alpha = 1, size = 2)
  
ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Blackpool", "Wokingham")),
            aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name), alpha = 1, size = 2)

ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = ehfs_exp_pc, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Blackpool", "Wokingham")),
            aes(x = year, y = ehfs_exp_pc, 
                group = la_name), alpha = 1, size = 2)

ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = ehfs_exp_pc_adjusted, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Blackpool", "Wokingham")),
            aes(x = year, y = ehfs_exp_pc_adjusted, 
                group = la_name), alpha = 1, size = 2)



# Multiple model r_squares with year to show how strongly it corresponded

ce_data

# Rep this for all years, show how EHFS funding has become completely unrelated to
# underlying need

tot_exp_rsqs <- tibble(
  year = 2011:2019,
  rsq = NA
)


for (i in 1:9) {
tot_exp_rsqs$rsq[i] <- lm(data = ce_data %>% filter(year == 2010+i),
   formula = tot_exp_pc ~ scale(imd_sc) + send_pc_pop + I(tot_chil_pop/100)) %>%
  summary(.) %>%
  .$r.squared
}
  
tot_exp_rsqs
  

ehfs_exp_rsqs <- tibble(
  year = 2011:2019,
  rsq = NA
)

for (i in 1:9) {
ehfs_exp_rsqs$rsq[i] <- lm(data = ce_data %>% filter(year == 2010+i),
   formula = ehfs_exp_pc ~ scale(imd_sc) + send_pc_pop + I(tot_chil_pop/100)) %>%
  summary(.) %>%
  .$r.squared
}

ehfs_exp_rsqs


### Final graphs: Stick to total expenditure

# Funding correspondence with total child population, deprivation, and SEND % of child pop

plots1_corr <- ggplot() +
  geom_line(data = tot_exp_rsqs, aes(x = year, y = rsq*100), size = 2, col = "#631A70") +
  geom_line(data = ehfs_exp_rsqs, aes(x = year, y = rsq*100), size = 2, col = "#00AC4D") +
  geom_text(data = tot_exp_rsqs %>% filter(year %in% c(2011, 2019)), aes(x = year, y = rsq*100 + 4, label = paste0(round(rsq, 2) * 100, "%")), col = "#631A70", fontface = "bold", size = 6) +
  geom_text(data = ehfs_exp_rsqs %>% filter(year %in% c(2011, 2019)), aes(x = year, y = rsq*100 - 5, label = paste0(round(rsq, 2) * 100, "%")), col = "#00AC4D", fontface = "bold", size = 6) +
  geom_hline(yintercept = 100) +
  annotate(geom = "text", y = 95, x = 2015, label = "If spending were totally based on a combination of these factors the\ncorrespondence would be 100%", fontface = "bold") +
  ylim(0, 100) +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  ylab("Child population, SEND %, & Deprivation\nCorrespondence with Spending per Child") +
  xlab("Year Ending") +
  ggtitle("<span style='color:#631A70'>Total</span> and <span style='color:#00AC4D'>Preventative</span> Spending on Children's Services has low correspondence<br>with Child population, SEND, and Deprivation. For early help and family<br>support the correspondence has fallen more than half.<br>") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Rounded MT Bold", size = 14),
        plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 16)) &
  plot_annotation(caption = "Analysis and data visualisation by Calum Webb, the University of Sheffield, on behalf of Children England.\nData from S251 returns, ONS population projections, DCLG & DfE. All figures adjusted for inflation. Correspondence measured with R-squared value.\nCode & Data: github.com/cjrwebb/cwip-app")


# LA examples: Blackpool and Wokingham

trend_la_1 <- ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Birmingham")),
            aes(x = year, y = tot_exp_pc, 
                group = la_name), alpha = 1, size = 2, col = "#631A70") +
  geom_line(data = ce_data %>% filter(la_name %in% c("Buckinghamshire")),
            aes(x = year, y = tot_exp_pc, 
                group = la_name), alpha = 1, size = 2, col = "#00AC4D") +
  scale_color_gradient(low = "#00AC4D", high = "#631A70") +
  ylab("Spending per child, unadjusted") +
  xlab("Year Ending") +
  ggtitle("Before Adjusting for Need") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Rounded MT Bold", size = 14),
        plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 14),
        legend.position = "none") 

trend_la_2 <- ce_data %>%
  ggplot() +
  geom_line(aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name, col = imd_sc), alpha = 0.2) + 
  geom_line(data = ce_data %>% filter(la_name %in% c("Birmingham")),
            aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name), alpha = 1, size = 2, col = "#631A70") +
  geom_line(data = ce_data %>% filter(la_name %in% c("Buckinghamshire")),
            aes(x = year, y = tot_exp_pc_adjusted, 
                group = la_name), alpha = 1, size = 2, col = "#00AC4D") +
  scale_color_gradient(low = "#00AC4D", high = "#631A70") +
  ylab("Spending per child, adjusted for\nPopulation, SEND %, and Deprivation") +
  xlab("Year Ending") +
  ggtitle("After Adjusting for Need") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Rounded MT Bold", size = 14),
        plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 14),
        legend.position = "none") 


plots2_la <- trend_la_1 + trend_la_2 &
  plot_annotation(title = "This means that while it looks like <span style='color:#631A70'>high deprivation authorities like Birmingham</span> spend more per child, they actually end up with less<br>relative to the scale of underlying needs than, say, a <span style='color:#00AC4D'>low deprivation authority like Buckinghamshire</span>.<br>",
                  caption = "Analysis and data visualisation by Calum Webb, the University of Sheffield, on behalf of Children England.\nData from S251 returns, ONS population projections, DCLG & DfE. All figures adjusted for inflation. Code & Data: github.com/cjrwebb/cwip-app",
                  theme =   theme(
                                  plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 16))
  )



# 3 Quintile Averages to show average trend

quinplot1 <- ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = tot_exp_pc, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) +
  scale_color_manual(values = c("#00AC4D", "grey50", "#631A70")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  ggtitle("Before Adjusting for Needs") +
  xlab("Year Ending") +
  ylab("Spending per child, unadjusted") +
  theme(text = element_text(family = "Arial Rounded MT Bold", size = 14),
        plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 14),
        legend.position = "none") 



quinplot2 <- ce_data %>%
  ggplot() +
  geom_line(data = ce_data_quintiles,
            aes(x = as.numeric(year), y = tot_exp_pc_adjusted, 
                group = as.character(imd_quintile), col = as.character(imd_quintile)), 
            alpha = 1, size = 2) +
  scale_color_manual(values = c("#00AC4D", "grey50", "#631A70")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  ggtitle("After Adjusting for Needs") +
  xlab("Year Ending") +
  ylab("Spending per child, adjusted for child population\nSEND %, and deprivation") +
  theme(text = element_text(family = "Arial Rounded MT Bold", size = 14),
        plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 14),
        legend.position = "none") 

plots3_quin <- quinplot1 + quinplot2  &
  plot_annotation(title = "This pattern is consistent across local authorities. The <span style='color:#631A70'>most deprived 30 local authorities</span> have seen an enormous drop in their<br>spending power after adjusting for need, whereas the <span style='color:#7F7F7F'>middle deprived 30</span> have seen large fluctuations and the <span style='color:#00AC4D'>least deprived 30<br>local authorities</span> have seen an increase relative to underlying needs.<br>",
                  caption = "Analysis and data visualisation by Calum Webb, the University of Sheffield, on behalf of Children England.\nData from S251 returns, ONS population projections, DCLG & DfE. All figures adjusted for inflation. Code & Data: github.com/cjrwebb/cwip-app",
                  theme =   theme(
                                  plot.title = element_markdown(family = "Arial Rounded MT Bold", size = 16))
  )





# output for 1400 x 760

ggsave(plot = plots1_corr, filename = "img/plots1_corr.png", device = "png", width = 10, height = 8)
ggsave(plot = plots2_la, filename = "img/plots2_la.png", device = "png", width = 16, height = 8)
ggsave(plot = plots3_quin, filename = "img/plots3_quin.png", device = "png", width = 16, height = 8)

