## FDA SHRIMP IMPORT REFUSAL

## Data Source
# Food and Drug Administration
# Received from WWF Aquaculture Lead Madeleine Craig on Jan 16, 2019
# Timeseries: 2014 to 2017
# Format: XLSX, CSV


## Summary
# Collaborate with WWF to visualize US shrimp import refusals timeseries. Where they are coming from and reason for refusal.
library(tidyverse)
library(plotly)
library(viridis)
library(lubridate)
library(rworldmap)
library(leaflet.minicharts)

# create file path shortcuts to the data
raw_path <- "data/raw/FDA_Shrimp_Import"
int_path <- "data/int/FDA_Shrimp_Import"



## Read in FDA and Country Coordinates Data
fda <- read.csv(file.path(raw_path, "Shrimp_Import_Refusals.csv"), stringsAsFactors = FALSE)

cntry_coord <- read.csv("data/ref/country_lat_lon.csv", stringsAsFactors = FALSE) %>% 
  mutate(COUNTRY_NAME = case_when(
    str_detect(COUNTRY_NAME, "United States") ~ "United States of America",
    TRUE ~ COUNTRY_NAME
  ))
  


## Tidy

# fix date column to a format R can understand
fda$REFUSAL_DATE <- as.Date(fda$REFUSAL_DATE, "%m/%d/%Y")

# create a separate year column
shrimp_imp <- fda %>%
  mutate(YEAR = lubridate::year(REFUSAL_DATE)) 

# fix country names Cote d'Ivoire to Ivory Coast
# see original Excel sheet to compare
shrimp_tidy <- shrimp_imp %>%
  mutate(COUNTRY_NAME = case_when(
    str_detect(COUNTRY_NAME, "Cote d'Ivoire") ~ "Ivory Coast",
    str_detect(COUNTRY_NAME, "UAE") ~ "United Arab Emirates",
    str_detect(COUNTRY_NAME, "\\#N\\/A") ~ "Myanmar",
    str_detect(COUNTRY_NAME, "Viet Nam") ~ "Vietnam",
    str_detect(COUNTRY_NAME, "Venezuela \\(Bolivarian Republic of\\)") ~ "Venezuela",
    str_detect(COUNTRY_NAME, "Korea, Republic of") ~ "South Korea",
    str_detect(COUNTRY_NAME, "Côte d'Ivoire") ~ "Ivory Coast",
    str_detect(COUNTRY_NAME, "Brunei Darussalam") ~ "Brunei",
    str_detect(COUNTRY_NAME, "Taiwan\\, Province of China") ~ "Taiwan",
    TRUE ~ COUNTRY_NAME
  ))



## SHRIMP IMPORT REFUSAL DOT DISTRIBUTION MAP ##

## Wrangle: 
# Add identifier for each unique refusal instance
# Expand refusal charges column so that each row is an individual refusal reason
shrimp_gather <- shrimp_tidy %>%
  select(YEAR, COUNTRY_NAME, starts_with("REFUSAL_CHARGES")) %>% 
  mutate(ID = 1:nrow(.)) %>% # num of refusal instances
  gather("DELETE", "REFUSAL_CHARGES", contains("REFUSAL_CHARGES")) %>%
  select(-DELETE) %>%
  filter(REFUSAL_CHARGES != "")  # remove rows with no refusal charge value



## Summarize: 
# Count refusal instances per country and year
# Count refusal REASONS per country and year
# This format will work for pie graph
ref_instances <- shrimp_gather %>% 
  group_by(YEAR, COUNTRY_NAME) %>% 
  summarise(REFUSAL_NUM = length(unique(ID))) %>%
  ungroup()

# fix reason names so they are appropriate col names that R can read
ref_reasons <- shrimp_gather %>%
  group_by(YEAR, COUNTRY_NAME, REFUSAL_CHARGES) %>%
  tally() %>%
  ungroup() %>%
  group_by(YEAR, COUNTRY_NAME) %>%
  mutate(REFUSAL_CHARGES = str_replace(REFUSAL_CHARGES," ", "_")) %>%
  mutate(REFUSAL_CHARGES = case_when(
           str_detect(REFUSAL_CHARGES, "^YELLOW_#5$") ~ "YELLOW_5",
           str_detect(REFUSAL_CHARGES, "^3LACKS_FIRM$") ~ "LACKS_FIRM3",
           TRUE ~ REFUSAL_CHARGES)
         ) %>%
  spread(REFUSAL_CHARGES, n, fill = 0)

# join refusal instances table with refusal reasons tally table
shrimp_summ <- ref_reasons %>% 
  left_join(ref_instances, by = c("YEAR", "COUNTRY_NAME"))

# check that sum of number of refusals equals original number of rows in fda data table
sum(shrimp_summ$REFUSAL_NUM);nrow(fda)

# check country name matches
setdiff(shrimp_summ$COUNTRY_NAME, cntry_coord$COUNTRY_NAME)



## Plotting
# Combine lat lon info with shrimp data
shrimp_spatial <- shrimp_summ %>%
  left_join(cntry_coord, by = "COUNTRY_NAME")

# Taking top refusals to make dataset easier to test
shrimp_refuse_dot <- shrimp_spatial %>%
  select(YEAR, COUNTRY_NAME, LAT, LON, SALMONELLA, VETDRUGES, NITROFURAN, FILTHY, REFUSAL_NUM) 



## Save data table
write.csv(shrimp_refuse_dot, "data/output/shrimp_refuse_map.csv")





## REFUSAL CHARGES STACKED BAR GRAPH ##

## Summarize
ref_summ <- shrimp_gather %>%
  group_by(YEAR, COUNTRY_NAME, REFUSAL_CHARGES) %>%
  tally() %>%
  ungroup() %>%
  rename(REFUSAL_COUNT = n)

#unique(ref_summ$REFUSAL_CHARGES) # 45 reasons

# Fix naming
fix_charges <- ref_summ %>%
  mutate(DESCRIPTION = case_when(
    str_detect(REFUSAL_CHARGES, "SALMONELLA") ~ "SALMONELLA",
    str_detect(REFUSAL_CHARGES, "NITROFURAN") ~ "NITROFURAN",
    str_detect(REFUSAL_CHARGES, "FILTHY") ~ "FILTHY",
    str_detect(REFUSAL_CHARGES, "VETDRUGES") ~ "VET. DRUGS"))

# clump all other charges into one category
shrimp_stacked <- fix_charges %>%
  mutate(DESCRIPTION = if_else(is.na(DESCRIPTION), "OTHER", DESCRIPTION))

# Reorder, starting from what you want listed last to first in the Legend  
shrimp_stacked$DESCRIPTION <- factor(shrimp_stacked$DESCRIPTION, levels = c("OTHER", "FILTHY", "VET. DRUGS", "NITROFURAN", "SALMONELLA"))

# top offending countries in the last 5 years - used for ordering drop down menu countries
total_refusals <- shrimp_stacked %>% 
  filter(YEAR %in% c(2014, 2015, 2016, 2017, 2018)) %>% 
  group_by(COUNTRY_NAME) %>% 
  summarise(total = sum(REFUSAL_COUNT)) %>% 
  ungroup() %>% 
  arrange(desc(total))
order_cntry <- total_refusals$COUNTRY_NAME



## Save data
write.csv(shrimp_stacked, "data/output/shrimp_refuse_bar.csv")
write.csv(total_refusals, "data/int/shrimp_refuse_totals_5yr.csv")



## Summary Stats
# Determine top offending countries based on cumuluative number of refusals over the last ten years and % contribution
stats <- shrimp_refuse_dot %>% 
  select(YEAR, COUNTRY = COUNTRY_NAME, REFUSAL_NUM) %>% 
  filter(YEAR %in% c(2009:2018)) %>% 
  group_by(COUNTRY) %>% 
  dplyr::summarize(REFUSAL_TOT = sum(REFUSAL_NUM)) %>% 
  ungroup() %>% 
  arrange(desc(REFUSAL_TOT))

all = sum(stats$REFUSAL_TOT)

stats <- stats %>% 
  mutate(REFUSAL_PCT = REFUSAL_TOT/all)



stats_2 <- shrimp_stacked %>% 
  select(YEAR, REFUSAL_COUNT, DESCRIPTION) %>% 
  filter(YEAR %in% c(2009:2018)) %>% 
  group_by(DESCRIPTION) %>% 
  dplyr::summarize(TOT = sum(REFUSAL_COUNT)) %>% 
  ungroup() %>% 
  arrange(desc(TOT))

all = sum(stats_2$TOT)

stats_2 <- stats_2 %>% 
  mutate(PCT = TOT/all)
  

