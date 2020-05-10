## US Food Fish Production ##

## Data Source
# USDA Quick Stats Census

# Source: https://quickstats.nass.usda.gov/
# Downloaded: Nov. 15, 2018
# Timeseries: 2013
# Format: CSV
# Metadata: https://quickstats.nass.usda.gov/src/glossary.pdf
# Notes: Queried for Census, Aquaculture, Food Fish, Sales (clicking the Get Data button should extract all data items, all years, and both state and national data. Data tables include sales (in $, head, and lb) and no. of operations per state. I don't believe inflation is accounted for. 

# The life cycle of fish products goes from EGGS to FINGERLINGS & FRY to STOCKERS to FOODSIZE or BROODSTOCK. STOCKERS are young fish kept until mature or food size. BROODSTOCK are mature fish that are used in aquaculture for breeding purposes. 

# Outputs:
# data/int/usda_fish/data_NA_count.csv
# data/int/usda_fish/US_sales_2013.csv
# data/int/usda_fish/US_sales_all_tidy.csv
# data/int/usda_fish/US_sales_gapfill_2013.csv
# # data/int/usda_fish/US_sales_per_operation
# data/output/fish_us_map.csv


## Setup
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(USAboundaries)
library(validate)
library(devtools)
library(Hmisc)

# raw data file directory
rawdata <- "data/raw/USDA_Quickstats"
intdata <- "data/int"

update_date = "20200509"

## Import Data
# 1998, 2005, and 2013 Census Data for Fish Food
filename = paste("food_fish_sales_",update_date,".csv",sep="")
data <- read.csv(file.path(rawdata, filename), stringsAsFactors = FALSE)


## Wrangle: Total
# Tidy and reorganize categories of fish sales. Set US TOTAL category to have code of 0.

# remove columns that only contain NA values
data <- data[ ,colSums(!is.na(data)) > 0]

# remove unnecessary columns
fishprod <- data %>%
  select(-Program, -Period, -Geo.Level, -watershed_code, -Domain.Category) %>%
  rename(State_Code = State.ANSI) %>%
  mutate(State_Code = ifelse(is.na(State_Code), 0, State_Code)) %>%
  mutate(State = ifelse(State == "US TOTAL", "US", State))

DT::datatable(fishprod)


## Select Totals
# Summary
# 1. There are two categories in `Domain`: TOTAL and SALES. Select TOTAL.
# 2. Split `Data.Item` column into `Product` and `Unit`. Tidy strings.
# 3. Split `Product` into species and product type.
# 4. Separate WHOLESALE types
# 5. Fix species names

# Definitions
# In the `Data.Item` column, `Sales` is defined as sales in US dollars.
# Operations: Depending upon the data series, may refer to farms, ranches, growers, or producers (original USDA metadata).

### Step 1-2
# Select TOTALS and Split `Data.Item` into `Product` and `Unit`

# split Data.Item into Product and Unit
fishunit <- fishprod %>%
  filter(Domain == "TOTAL") %>%
  select(-Domain) %>%
  separate(Data.Item, c("Product", "Unit"), " - ") %>%
  mutate(Product = str_replace(Product, "FOOD FISH, ", "")) %>%
  mutate(Unit = case_when(
    str_detect(Unit, "OPERATIONS WITH SALES$") ~ "OPERATIONS",
    str_detect(Unit, "SALES, MEASURED IN \\$$") ~ "DOLLARS",
    str_detect(Unit, "SALES, MEASURED IN \\$ \\/ OPERATION$") ~ "DOLLARS_PER_OPERATION",
    str_detect(Unit, "SALES, MEASURED IN EGGS$") ~ "EGGS",
    str_detect(Unit, "SALES, MEASURED IN HEAD$") ~ "HEAD",
    str_detect(Unit, "SALES, MEASURED IN LB$") ~ "LB",
    str_detect(Unit, "SALES, MEASURED IN LB \\/ HEAD$") ~ "LB_PER_HEAD",
    str_detect(Unit, "SALES, MEASURED IN PCT BT SIZE GROUP") ~ "PCT BT SIZE GROUP",
    str_detect(Unit, "SALES, MEASURED IN PCT BY OUTLET") ~ "PCT BY OUTLET"
  ))

# Save unique category of values (e.g. dollars, head, lb)
data_cat <- unique(fishunit$Unit)

DT::datatable(fishunit)

### Step 3
#*Split `Product` into species and product type*
#Not all will split perfectly, will need to fix a few things.
#REGEX `,\\s*(?=[^,]+$)` Explained: look for a comma (`,`) that is followed by zero or more (`*`) white space (`\\s`), which is immediately followed by one or more non-comma values (`?=[^,]+`) at the end of the string (`$`). See (Regex cheatsheet)[https://www.rexegg.com/regex-quickstart.html].

# first split: separate by last comma
firstsplit <- fishunit %>%
  separate(Product, c("Species", "Product_Type"), ",\\s*(?=[^,]+$)", fill="right")

### Step 4

# Second split: wholesale types & retail
# Wholesale Product Types
# Check values (e.g. EXPORTS) in column Product_Type that are associated with WHOLESALE in Species and create new wholesale category column. For these, assign a Species value of NA and Product_Type as WHOLESALE.
whole <- firstsplit %>% filter(Species == "WHOLESALE")
wholesale_types <- str_c(unique(whole$Product_Type), collapse="|")

secsplit <- firstsplit %>%
  mutate(Wholesale_Type = ifelse(str_detect(Product_Type, wholesale_types), Product_Type, NA),
         Product_Type = ifelse(Species == "WHOLESALE" | Species == "RETAIL", Species, Product_Type),
         Species = ifelse(Species == "WHOLESALE" | Species == "RETAIL", NA, Species))

### Step 5
# Correct Species names w/o Product Type
# The only values in `Product_Type` should be:
# * FINGERLINGS & FRY
# * FOODSIZE
# * STOCKERS
# * BROODSTOCK
# * EGGS
# * WHOLESALE
# * RETAIL

# First rearrange Species column separated by commas, placing second part before the first (except for "CARP, (EXCL GRASS)". Second, move parts of species names (e.g. `RED`, `ARCTIC`) in the `Product_Type` column back into `Species`.
# REGEX `'(.*)\\,\\s+(\\w+)','\\2 \\1'` explained: Substitute any strings in `Species` that have a comma separating two word phrases and place the second phrase in front of the first.

secsplit$Species <- sub('(.*)\\,\\s+(.*)','\\2 \\1', secsplit$Species)
secsplit$Species <- sub('\\(.*\\)', 'OTHER', secsplit$Species)
#secsplit$Species <- sub(', ', ' ', secsplit$Species) # remove comma from "CARP, (EXCL GRASS)"

# check names, should be 29 types incl NA
unique(secsplit$Species)

# find parts of species names that got placed in product type - do "(EXCL GRASS" separately
unique(secsplit$Product_Type)
secsplit$Product_Type <- sub('\\(.*\\)', 'OTHER', secsplit$Product_Type)
sp_names <- str_c(c("HYBRID STRIPED", "^GRASS$", "RED", "YELLOW", "ATLANTIC", "PACIFIC", "ARCTIC", "^OTHER$"), collapse="|")

# Replace any NAs with "NONE" to prevent it from being removed in filters later
sp_fix <- secsplit %>%
  mutate(
    Product_Type = ifelse(is.na(Product_Type), "ALL PRODUCTS", Product_Type),
    Species = ifelse(str_detect(Product_Type, sp_names),
                     paste(Product_Type, Species, sep=" "), Species)
    #Species = ifelse(str_detect(Product_Type, "(EXCL GRASS)"),
     #                paste(Species, Product_Type, sep=" "), Species),
    #Species = ifelse(is.na(Species), "NONE", Species)
    )

# remove species names from Product_Type
#sp_remove <- str_c(c(sp_names, "(EXCL GRASS)"), collapse="|")
totalfish <- sp_fix %>%
  mutate(Product_Type = ifelse(str_detect(Product_Type, sp_names), "ALL PRODUCTS", Product_Type))


## Check
unique(totalfish$Product_Type) 
unique(totalfish$Species) 


## Save Tidied Intermediate Data
# TOTAL Finfish Data
# check with USDA 2013 Census Report, pg 10 (http://www.aquafeed.com/documents/1412204142_1.pdf)
tidy_fish <- totalfish %>% 
  mutate(Value = as.numeric(str_replace_all(Value, ",", "")))
filename <- paste("data/int/usda_fish/US_sales_all_tidy_", update_date, ".csv", sep="")
write.csv(tidy_fish, filename, row.names = FALSE)


## Save data for Gapfilling
# Avg dollars per operation in the US per census year
# Average 2013 sales in dollars per operation in the US is 564,928 USD. in 2005 it was 364,038 USD and in 1998 it was 319,056 USD.
dolperop <- tidy_fish %>%
  filter(Unit == "DOLLARS_PER_OPERATION") %>%
  select(Year, State, Species, Unit, Value) 

filename <- paste("usda_fish/US_sales_per_operation_", update_date, ".csv", sep="")
write.csv(dolperop, file.path(intdata, filename))

dolperop1998 <- dolperop[3,5]
dolperop2005 <- dolperop[2,5]
dolperop2013 <- dolperop[1,5]
dolperop2018 <- dolperop[4,5]



## Visualize Data


## US FOOD FISH PRODUCTION PER STATE ##
# Summary: by sales in dollar, and no. of operations


### Filter: 2018 Raw Data
# Within totals, just select raw,  not calculated, data. Select for most recent year (2013).
# Remove wholesale and retail information - the units for this is PCT BY OUTLET.

# Filter removes NA
rawfish <- tidy_fish %>%
  filter(Product_Type != "WHOLESALE" & Product_Type != "RETAIL") %>%
  filter(Unit %in% c("OPERATIONS", "HEAD", "LB", "DOLLARS", "EGGS")) %>%
  filter(State != "US") %>%
  select(-Wholesale_Type, -State_Code) %>%
  filter(Year == 2018) %>%
  filter(Species == "FOOD FISH", Product_Type == "ALL PRODUCTS") # get totals for food fish

### Gapfill
# Use average sales in dollars per operation in 2018 to estimate missing state values

# Check: 
# should only be two entries per state, one for OPERATIONS, one for DOLLARS
table(rawfish$State)
# Only NAs should be DOLLAR value
sum(is.na(rawfish$Value))
er <- rawfish %>% filter(Unit == "DOLLARS")
sum(is.na(er$Value)) # number of NAs, 19

# fill in estimated values and gapfill method
gf_fish <- rawfish %>%
  mutate(gf_method = ifelse(is.na(Value), "AVG USD PER OP", "NONE")) %>% 
  group_by(State) %>% 
  mutate(Value = ifelse(is.na(Value), # selects No. of Operations
                        prod(Value, na.rm=TRUE)*dolperop2018,
                        Value)) %>% 
  ungroup()

### NA Count 
# per data type
# Lots of NAs for Sales measured in dollars.
NA_count <- rawfish %>%
  group_by(Unit) %>%
  summarise(num_NA = sum(is.na(Value)),
            pct_NA = round(sum(is.na(Value))/length(Value),2)) %>%
  ungroup()

filename = paste("usda_fish/data_NA_count_", update_date, ".csv", sep="")
write.csv(NA_count, file.path(intdata, filename))

# Total Sales in 2018 per State
fish_sales <- gf_fish %>%
  select(Year, State, Unit, Value) 

# create columns needed for mapping in map module
data_for_map <- fish_sales %>%
  rename(state = State,
         map_data = Value,
         type = Unit) %>%
  mutate(units = case_when(
    str_detect(type, "DOLLARS") ~ "USD",
    # str_detect(type, "HEAD") ~ "fish",
    # str_detect(type, "LB") ~ "lbs",
    str_detect(type,"OPERATIONS") ~ "operations",
    #str_detect(type, "EGGS") ~ "eggs",
    TRUE ~ type
  )) %>%
  mutate(taxon = "Fish")

### TIDY FOR PLOTTING MAP
# just state and lat/lon
state_tidy <- us_states(resolution = "low") %>%
  dplyr::select(state_name) %>%
  dplyr::rename(state = state_name) %>%
  dplyr::mutate(state = toupper(state))

# maybe there's a more efficient way to add values back into the states with missing values.. but for now this is fine..
fish_us <- state_tidy %>%
  full_join(data_for_map, by = "state") %>%
  filter(!state %in% c("PUERTO RICO", "DISTRICT OF COLUMBIA")) %>%
  tidyr::complete(state, type) %>%  # fill in categories where there is no data
  filter(!is.na(type)) %>% # remove extra NA rows created
  # select(-geometry) %>%  # temporarily remove geometry 
  mutate(units = case_when( # add back in where there are NAs..
    str_detect(type, "EGGS") ~ "eggs",
    str_detect(type, "DOLLARS") ~ "USD",
    str_detect(type, "HEAD") ~ "fish",
    str_detect(type, "OPERATIONS") ~ "operations",
    str_detect(type, "LB") ~ "lbs"
  )) %>% 
  mutate(taxon = "Fish") # add back in where there are NAs..

fish_us_map <- state_tidy %>% 
  left_join(fish_us, by = "state") # add lat/lon back in




## US FOOD FISH SALES PER OPERATION OVER TIME ##
# Summary: sales/operation over time
# Need to gapfill Sales in Dollars

### Tidy
# remove US values, wholesale & retail
# select only operations and dollars
# select data for All Food Fish Products
fish_ts <- tidy_fish %>%
  filter(Product_Type != "WHOLESALE" & Product_Type != "RETAIL") %>%
  filter(Unit %in% c("OPERATIONS", "DOLLARS")) %>%
  filter(State != "US") %>%
  select(-Wholesale_Type, -State_Code, -Commodity) %>%
  filter(Species == "FOOD FISH", Product_Type == "ALL PRODUCTS") %>% 
  tidyr::spread(Unit, Value)

### Gapfill
# Use average sales in dollars per operation per census year to estimate missing state values

# Check: 
# should only be at most 3 entries per state, 1 for each census year
table(fish_ts$State)
# Count number of NAs that will be gapfilled
sum(is.na(fish_ts$DOLLARS)) # 43 NAs
sum(is.na(fish_ts$OPERATIONS)) # should b 0 NAs

# Fill in estimated values and gapfill method
# Summary: Where there is an NA for every State-Year, multiply No. of Operations for that State-Year by Avg Dollars per Operation in that Census Year
# Make sure to manually check some values
gf_fish_ts <- fish_ts %>%
  mutate(gf_method = ifelse(is.na(DOLLARS), "AVG USD PER OP", "NONE")) %>% 
  group_by(State, Year) %>% 
  mutate(DOLLARS = ifelse(is.na(DOLLARS) & Year == 2018,
                          OPERATIONS*dolperop2018,
                          DOLLARS),
         DOLLARS = ifelse(is.na(DOLLARS) & Year == 2013,
                        OPERATIONS*dolperop2013,
                        DOLLARS),
         DOLLARS = ifelse(is.na(DOLLARS) & Year == 2005,
                        OPERATIONS*dolperop2005,
                        DOLLARS),
         DOLLARS = ifelse(is.na(DOLLARS) & Year == 1998,
                OPERATIONS*dolperop1998,
                DOLLARS)) %>%
  ungroup()


# Check gapfilling e.g. Delaware had all NA values should be true

# for(i in 1:3){ # i=1
# 
# census <- c(1998, 2005, 2013)  
# 
# # dollars is the gapfilled value
# dollars <- filter(gf_fish_ts, State == "DELAWARE" & Year == census[i])["DOLLARS"]
# num_op <- filter(gf_fish_ts, State == "DELAWARE" & Year == census[i])["OPERATIONS"]
# 
# listcensusavg = list("1998" = dolperop1998,
#               "2005" = dolperop2005,
#               "2013" = dolperop2013)
# 
# census_avg = listcensusavg[i][[1]]
# 
# # check that gapfiilled value equals to census avg multipled by numer of operaitons
# print((dollars == num_op*census_avg)[[1]])
# 
# }

### Summarize: dolls/op
fish_summ <- gf_fish_ts %>% 
  mutate(Dol_per_Op = DOLLARS/OPERATIONS) %>% 
  select(-Species) %>% 
  mutate(Year = as.character(Year))

# Get quantiles of dollars per operation to use in legend when plot
zero <- stats::quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[1]]
twenty <- quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[2]]
forty <- quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[3]]
sixty <- quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[4]]
eighty <- quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[5]]
hundo <- quantile(fish_summ$Dol_per_Op, probs=seq(0,1,0.2), na.rm=TRUE)[[6]]

fish_dolop_plot <- fish_summ %>% 
  mutate(Quantile = case_when(
    Dol_per_Op >= zero & Dol_per_Op <= twenty ~ "11K-53K",
    Dol_per_Op > twenty & Dol_per_Op <= forty ~ "54K-125K",
    Dol_per_Op > forty & Dol_per_Op <= sixty ~ "126K-320K",
    Dol_per_Op > sixty & Dol_per_Op <= eighty ~ "321K-556K",
    Dol_per_Op > eighty & Dol_per_Op <= hundo ~ "557K-5,970K"
    )) %>% 
  mutate(Quantile = as.factor(Quantile))

fish_dolop_plot$Quantile <- factor(fish_dolop_plot$Quantile, levels = c("11K-53K", "54K-125K", "126K-320K", "321K-556K", "557K-5,970K"))




## FOOD FISH PRODUCTION BASELINE STATS ##
# Read in tidied US Food Fish data
filename <- paste("data/int/usda_fish/US_sales_all_tidy_", update_date, ".csv", sep="")
stats <- read.csv(filename)

## which species has the largest $ share
sales <- stats %>%
  filter(Unit == "DOLLARS",
         Year == 2018,
         Product_Type == "ALL PRODUCTS", # select totals for all products
         State == "US") %>% 
  filter(!Species %in% c("GRASS CARP", "OTHER CARP")) %>%  # remove sub categories
  select(Species, Unit, Value) %>% 
  arrange(desc(Value)) %>% 
  mutate(Percent = Value/.[.$Species == "FOOD FISH",][["Value"]]) # divide by total value

# which state has the largest $ share and what was the sales
state <- stats %>%
  filter(Unit == "DOLLARS",
         Year == 2018,
         Species == "FOOD FISH",
         Product_Type == "ALL PRODUCTS") %>% 
  select(State, Unit, Value) %>% 
  arrange(desc(Value))

# where are most of the food fish operations?
op <- stats %>%
  filter(Unit == "OPERATIONS",
         Year == 2018,
         Species == "FOOD FISH",
         Product_Type == "ALL PRODUCTS") %>%
  select(State, Unit, Value) %>%  
  arrange(desc(Value)) %>% 
  mutate(Percent = Value/.[.$State == "US",][["Value"]]) # divide by US value
