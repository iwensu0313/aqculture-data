## FARMED SEAFOOD IMPORT DATA

## Data Source
# NOAA Fisheries Statistics - Cumulative Trade Data by U.S. Customs District
# Link: https://www.st.nmfs.noaa.gov/commercial-fisheries/foreign-trade/applications/trade-by-specific-us-customs-district
# Downloaded each year one at a time, make sure to select December for the 'Month' drop-down to get data from Jan-Dec
# Timeseries: 2014-2018
# Format: ASCII
# Downloaded: 10/30/2018


## Summary 
# Aggregate fisheries imports into California by value and visualize change over time. Summarize products with highest value.
# Check if USD needs to be corrected for inflation
library(validate)
library(tidyverse)
library(plotly)
library(viridis)


## Read in California Import Data from NOAA
# data_years <- 2014:2017
# data <- list()
# 
# for(i in data_years){ # i = 2014
  
dat <- read.csv("data/raw/NOAA_Fishstats_Trade/trade_alldstrct_byproduct.results", header = FALSE, skip = 1, sep = "|") %>% 
  select(-V1, -V9)

# dat <- read.csv(sprintf("data/raw/NOAA_Fishstats_Trade/trade_%s.data_in", i), header = FALSE, skip = 1, sep = "|") %>% 
#     mutate(V1 = i)
#   # save each year's data into a list
#   list_name <- as.character(i)
#   data[[list_name]] <- dat
#}


## Clean up data tables
# Need to hand select Edible products, change with original source online
# Column names should be: Year, Edible, Product Name, Country, Kilos, Dollars
colnames(dat) <- c("Year", "Edible", "Product", "District", "Country", "Kilos", "Dollars")

# colnames = c("Year", "Product", "Country", "Kilos", "Dollars")
# 
# tidy <- function(x) {
#   ## clean up dataframe
#   tmp <- x[!(names(x) %in% c("V4", "V5", "V8"))]
#   tmp <- setNames(tmp, colnames)
# }

# tidy_data <- lapply(data, tidy)

# Combine (Note: for larger dataset, maybe we don't want to bind it into one)
#all_data <- map_df(tidy_data, rbind)

#summary(all_data)

write.csv(dat, "data/int/noaa_imports.csv", row.names = FALSE)


## Filter data table
# Select edible, farmed/aquaculture products
farm <- dat %>% 
  filter(Edible == "E") %>% 
  filter(str_detect(Product, "FARM"))


## Combine some products and fix some country names
# frozen_oysters = str_c(c("OYSTERS FROZEN/DRIED/SALTED/BRINE FARMED", "OYSTERS FROZEN FARMED", "OYSTERS FROZEN FARMED"), collapse = "|")
# atl_salmon = str_c(c("SALMON ATLANTIC FILLET FRESH FARMED", "SALMON ATLANTIC MEAT FRESH FARMED", "SALMON ATLANTIC FRESH FARMED"), collapse = "|")

# combine_prod <- all_data %>% 
#   mutate(Product = case_when(
#     str_detect(Product, frozen_oysters) ~ "FARMED FROZEN OR PROCESSED OYSTERS",
#     str_detect(Product, atl_salmon) ~ "FARMED FRESH ATLANTIC SALMON",
#     TRUE ~ Product
#   )) %>% 
#   mutate(Country = case_when(
#     str_detect(Country, "FAROE IS.") ~ "FAROE ISLAND",
#     str_detect(Country, "CHINA") ~ "CHINA",
#     TRUE ~ Country
#   ))
# 
# ## Summarize information
# # Average Dollars/Kilos for FARMED PRODUCTS
# summary_plot <- combine_prod %>% 
#   #filter(str_detect(Product, "FARM")) %>% 
#   group_by(Year, Product, Country) %>% 
#   summarise(Dollars = sum(Dollars),
#             Kilos = sum(Kilos)) %>% 
#   mutate(AvgRate = Dollars/Kilos) %>% # per country avg
#   ungroup() %>% 
#   group_by(Year, Product) %>% # aggregate total across countries
#   mutate(TotalValue = sum(Dollars), 
#          TotalKilos = sum(Kilos),
#          TotalAvgRate = mean(AvgRate, na.rm=TRUE)) %>% 
#   ungroup() %>% 
#   filter(Dollars != 0) %>% 
#   mutate(Year = as.character(Year))  # so the x-axis values don't show half years
# 
# ## Tidy for Plotting
# # Combine total imports into CA over time into data table
# totals <- summary_plot %>% 
#   group_by(Year) %>% 
#   summarise(Dollars = sum(Dollars)) %>% 
#   mutate(Product = "ALL PRODUCTS",
#          Country = "ALL COUNTRIES") %>% 
#   as.data.frame()
# 
# ca_import_plot <- summary_plot %>%
#   select(Year, Product, Country, Dollars) %>% 
#   rbind(totals) %>% 
#   arrange(desc(Year)) %>%
#   mutate(Country = map_chr(Country, capStr))
# 
# write.csv(ca_import_plot, "data/output/ca_import_plot.csv")
# 
# 
# 
# 
# ## Data Download
# noaaimportTable <- read.csv("data/int/noaa_ca_imports.csv") %>% 
#   arrange(desc(Year))
