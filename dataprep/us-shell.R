## US Mollusk Production ##

## Data Source
# USDA Quick Stats Census

# Source: https://quickstats.nass.usda.gov/
# Downloaded: Dec. 3, 2018
# Timeseries: 2013
# Format: CSV
# Metadata: https://quickstats.nass.usda.gov/src/glossary.pdf
# Notes: Queried for Census, Aquaculture, Mollusk, Sales (clicking the Get Data button should extract all data items, all years, and both state and national data. Data tables include sales (in $, head, and lb) and no. of operations per state. I don't believe inflation is accounted for. 

# Outputs:
# data/int/usda_mollusk/data_NA_count.csv
# data/int/usda_mollusk/US_sales_2013.csv
# data/int/usda_mollusk/US_sales_all_tidy.csv
# data/int/usda_mollusk/US_sales_gapfill_2013.csv
# # data/int/usda_mollusk/US_sales_per_operation
# data/output/shell_us_map.csv


## Setup
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(USAboundaries)
library(validate)
library(devtools)
library(Hmisc)
library(janitor)

# raw data file directory
rawdata <- "data/raw/USDA_Quickstats"
intdata <- "data/int"

update_date = "20200509"

## Import Data
# 1998, 2005, 2013, 2018 Census Data for Molusks
filename = paste("mollusk_sales_",update_date,".csv",sep="")
data <- read.csv(file.path(rawdata, filename), stringsAsFactors = FALSE)

## Wrangle: Total
# Tidy and reorganize

# remove columns that only contain NA values
data <- data[ ,colSums(!is.na(data)) > 0]

# remove unnecessary columns, modify category names/value
molprod <- data %>%
  select(-Program, -Period, -Geo.Level, -watershed_code, -Domain.Category) %>%
  rename(State_Code = State.ANSI) %>%
  mutate(State_Code = ifelse(is.na(State_Code), 0, State_Code)) %>%
  mutate(State = ifelse(State == "US TOTAL", "US", State))

DT::datatable(molprod)


## Select Totals
# Summary
# 1. Unsure what the two categories in `Domain` are: TOTAL and SALES. At times, SALES category seems to be a smaller subset of TOTAL (ironically) and only provides aggregated value for the US, but unclear..
# 2. Split `Data.Item` column into `Product` and `Unit`. Tidy strings.
# 3. Split `Product` into species and product type.
# 4. Separate WHOLESALE types
# 5. Fix species names

# Definitions
# In the `Data.Item` column, `Sales` is defined as sales in US dollars.
# Operations: Depending upon the data series, may refer to farms, ranches, growers, or producers (original USDA metadata).

### Step 1-2
#Split `Data.Item` into `Product` and `Unit`*

# split Data.Item into Product and Unit
# simplify Unit category names
molunit <- molprod %>%
  filter(Domain == "TOTAL") %>% 
  select(-Domain) %>% 
  separate(Data.Item, c("Product", "Unit"), " - ") %>%
  mutate(Product = str_replace(Product, "MOLLUSKS, ", "")) %>%
  mutate(Unit = case_when(
    str_detect(Unit, "OPERATIONS WITH SALES$") ~ "OPERATIONS",
    str_detect(Unit, "SALES, MEASURED IN \\$$") ~ "DOLLARS",
    str_detect(Unit, "SALES, MEASURED IN \\$ \\/ OPERATION$") ~ "DOLLARS_PER_OPERATION",
    str_detect(Unit, "SALES, MEASURED IN EGGS$") ~ "EGGS",
    str_detect(Unit, "SALES, MEASURED IN HEAD$") ~ "HEAD",
    str_detect(Unit, "SALES, MEASURED IN LB$") ~ "LB",
    str_detect(Unit, "SALES, MEASURED IN HEAD \\/ LB$") ~ "HEAD_PER_LB",
    str_detect(Unit, "SALES, MEASURED IN PCT BT SIZE GROUP") ~ "PCT BT SIZE GROUP",
    str_detect(Unit, "SALES, MEASURED IN PCT BY OUTLET") ~ "PCT BY OUTLET"
  ))

# See unique category of values (e.g. dollars, head, lb)
unique(molunit$Unit)

DT::datatable(molunit)

### Step 3
#*Split `Product` into species and product type*
#Not all will split perfectly, will need to fix a few things.
#REGEX `,\\s*(?=[^,]+$)` Explained: look for a comma (`,`) that is followed by zero or more (`*`) white space (`\\s`), which is immediately followed by one or more non-comma values (`?=[^,]+`) at the end of the string (`$`). See (Regex cheatsheet)[https://www.rexegg.com/regex-quickstart.html].

# first split: separate by last comma
firstsplit <- molunit %>%
  separate(Product, c("Species", "Product_Type"), ",\\s*(?=[^,]+$)", fill="right")

### Step 4
# Second split: remove wholesale types & retail from Species column
# Check values (e.g. EXPORTS) in column Product_Type that are associated with WHOLESALE (in col Species) and create new wholesale category column. For these, assign a Species value of NA and Product_Type as WHOLESALE.
whole <- firstsplit %>% filter(Species == "WHOLESALE")
wholesale_types <- str_c(unique(whole$Product_Type), collapse="|")

secsplit <- firstsplit %>%
  mutate(Wholesale_Type = ifelse(str_detect(Product_Type, wholesale_types), Product_Type, NA)) %>%
  mutate(Product_Type = ifelse(Species == "WHOLESALE" | Species == "RETAIL", Species, Product_Type)) %>%
  mutate(Species = ifelse(Species == "WHOLESALE" | Species == "RETAIL", NA, Species)) # remove wholesale/retail from Species

### Step 5
# Correct Species names w/o Product Type
# The only values in `Product_Type` should be:
# * LARVAE & SEED
# * FOODSIZE
# * BROODSTOCK
# * WHOLESALE
# * RETAIL

# check which ones are not product types (e.g. species names)
unique(secsplit$Product_Type)

# First rearrange Species column separated by commas, placing second part before the first and convert all Species with a specified exclusion into "OTHER". Ex: "CLAMS, (EXCL HARD & MANILA)" = "OTHER CLAMS".
#Second, move parts of species names (e.g. `MANILA`, `PACIFIC`) in the `Product_Type` column back into `Species`.
# REGEX `'(.*)\\,\\s+(\\w+)','\\2 \\1'` explained: Substitute any strings in `Species` that have a comma separating two word phrases and place the second phrase in front of the first.
secsplit$Species <- sub('(.*)\\,\\s+(.*)','\\2 \\1', secsplit$Species)
secsplit$Species <- sub('\\(.*\\)', 'OTHER', secsplit$Species)

# check names, should be 14 types incl NA
unique(secsplit$Species)
# Check that any NAs in Species is associated with WHOLESALE data
check <- secsplit %>% 
  mutate(check = ifelse(is.na(Species) | Product_Type == "WHOLESALE", 1,0))
table(check$check)[2] == sum(is.na(check$Species)) # should be TRUE

# find parts of species names that got placed in product type
# replace (EXCL) with "OTHER" and save species names
unique(secsplit$Product_Type)
secsplit$Product_Type <- sub('\\(.*\\)', 'OTHER', secsplit$Product_Type)
sp_names <- str_c(c("GEODUCK", "HARD", "MANILA", "EASTERN", "PACIFIC", "^OTHER$"), collapse="|")

# Replace missing product type with ALL PRODUCTS, fix species names in product type col
sp_fix <- secsplit %>%
  mutate(
    Product_Type = ifelse(is.na(Product_Type), "ALL PRODUCTS", Product_Type),
    Species = ifelse(str_detect(Product_Type, sp_names),
                     paste(Product_Type, Species, sep=" "), Species)#,
    #Species = ifelse(is.na(Species), "NONE", Species)
  )

# remove species names from Product_Type, replace with "ALL PRODUCTS"
totalmol <- sp_fix %>%
  mutate(Product_Type = ifelse(str_detect(Product_Type, sp_names), "ALL PRODUCTS", Product_Type))


## Check
unique(totalmol$Product_Type) # should have 7 values
unique(totalmol$Species) # should have 14 values incl NA
# make sure remaining NAs in Species column are all WHOLESALE or RETAIL data
# IN THE FUTURE ADD IN IF STATEMENT TO STOP IF FALSE
k <- totalmol %>% filter(is.na(Species))
unique(k$Product_Type)


## Save Tidied Intermediate Data
# TOTAL Shellfish Data
# check with USDA 2013 Census Report, pg 10 (http://www.aquafeed.com/documents/1412204142_1.pdf)
tidy_mol <- totalmol %>% 
  mutate(Value = as.numeric(str_replace_all(Value, ",", "")))
filename <- paste("data/int/usda_mollusk/US_sales_all_tidy_", update_date, ".csv", sep="")
write.csv(tidy_mol, filename, row.names = FALSE)

## Save Data for Gapfilling
# Average 2013 sales in dollars per operation in the US is 434,613 USD. in 2005 it was 207,330 USD and in 1998 it was 166,594 USD.
dolperop <- tidy_mol %>%
  filter(Unit == "DOLLARS_PER_OPERATION") %>%
  select(Year, State, Species, Unit, Value)

filename <- paste("usda_mollusk/US_sales_per_operation_", update_date, ".csv", sep="")
write.csv(dolperop, file.path(intdata, filename), row.names = FALSE)

dolperop1998 <- dolperop[3,5]
dolperop2005 <- dolperop[2,5]
dolperop2013 <- dolperop[1,5]
dolperop2018 <- dolperop[4,5]




## Visualize Data


## US MOLLUSK PRODUCTION PER STATE ##
# Summary: by sales in dollar, and no. of operations


### Filter: 2013 Raw Data
# Within totals, just select raw,  not calculated, data. Select for most recent year (2013). Replace (D) with NA.
# Remove wholesale and retail information - the units for this is PCT BY OUTLET.
rawmoll <- tidy_mol %>%
  filter(Product_Type != "WHOLESALE" & Product_Type != "RETAIL") %>%
  filter(Unit %in% c("OPERATIONS", "HEAD", "LB", "DOLLARS")) %>%
  filter(State != "US") %>%
  select(-Wholesale_Type, -State_Code) %>%
  filter(Year == 2018) %>%
  filter(Species == "MOLLUSKS", Product_Type == "ALL PRODUCTS") # this is a temp solution for getting the total number of mollusk data per state... fix later

DT::datatable(rawmoll)


## Gapfill
# Use average sales in dollars per operation in 2013 to estimate missing state values

# Check: should only be two entries per state, one for OPERATIONS, one for DOLLARS
table(rawmoll$State)

# fill in estimated values and gapfill method
gf_moll <- rawmoll %>%
  mutate(gf_method = ifelse(is.na(Value), "AVG USD PER OP", "NONE")) %>% 
  group_by(State) %>% 
  mutate(Value = ifelse(is.na(Value),
                  prod(Value, na.rm=TRUE)*dolperop2018,
                  Value))


## Check 
# total sales after gapfilling, actual number is ~328,567,000
gf_moll %>% 
  filter(Species == "MOLLUSKS", Product_Type == "ALL PRODUCTS") %>% 
  group_by(Unit) %>% 
  summarise(tots = sum(Value, na.rm = TRUE))


## NA Tracking
# Count number of NAs per data type
# Lots of NAs for Sales measured in dollars.
NA_count <- rawmoll %>%
  group_by(Unit) %>%
  summarise(num_NA = sum(is.na(Value)),
            pct_NA = round(sum(is.na(Value))/length(Value),2)) %>%
  ungroup()

filename = paste("usda_mollusk/data_NA_count_", update_date, ".csv", sep="")
write.csv(NA_count, file.path(intdata, filename), row.names = FALSE)

## Total Sales in 2018 per State
moll_sales <- gf_moll %>%
  select(Year, State, Unit, Value)

# create columns needed for mapping in map module
data_for_map <- moll_sales %>%
  rename(state = State,
         map_data = Value,
         type = Unit) %>%
  mutate(units = case_when(
    str_detect(type, "DOLLARS") ~ "USD",
    str_detect(type,"OPERATIONS") ~ "operations",
    TRUE ~ type
  )) %>%
  mutate(taxon = "Mollusk")

### TIDY FOR PLOTTING MAP
# just state and lat/lon
state_tidy <- us_states(resolution = "low") %>%
  select(state_name, geometry) %>%
  rename(state = state_name) %>%
  mutate(state = toupper(state))

# add in all states so states with no data will show up gray
moll_us <- state_tidy %>%
  full_join(data_for_map, by = "state") %>%
  filter(!state %in% c("PUERTO RICO", "DISTRICT OF COLUMBIA")) %>%
  complete(state, type) %>%  # fill in categories where there is no data
  filter(!is.na(type)) %>% # remove extra NA rows created
  select(-geometry) %>%  # temporarily remove geometry 
  mutate(units = case_when( # add back in where there are NAs..
    str_detect(type, "DOLLARS") ~ "USD",
    str_detect(type, "OPERATIONS") ~ "operations"
  )) %>% 
  mutate(taxon = "Mollusk") # add back in where there are NAs..

# SHELL_US_MAP
shell_us_map <- state_tidy %>% 
  left_join(moll_us, by = "state") # add lat/lon back in

#st_write(shell_us_map, "data/output/shell_us_map.csv") # saves sf object as data frame




## US MOLLUSK SALES PER OPERATION OVER TIME ##
# Summary: sales/operation over time
# Need to gapfill Sales in Dollars

### Tidy
# remove US values, wholesale & retail
# select only operations and dollars
# select data for All Food Fish Products
moll_ts <- tidy_mol %>%
  filter(Product_Type != "WHOLESALE" & Product_Type != "RETAIL") %>%
  filter(Unit %in% c("OPERATIONS", "DOLLARS")) %>%
  filter(State != "US") %>%
  select(-Wholesale_Type, -State_Code, -Commodity) %>%
  filter(Species == "MOLLUSKS", Product_Type == "ALL PRODUCTS") %>% 
  spread(Unit, Value)

### Gapfill
# Use average sales in dollars per operation per census year to estimate missing state values

# Check: 
# should only be at most 3 entries per state, 1 for each census year
table(moll_ts$State)
# Count number of NAs that will be gapfilled
sum(is.na(moll_ts$DOLLARS)) # 25 NAs
sum(is.na(moll_ts$OPERATIONS)) # should b 0 NAs

# Fill in estimated values and gapfill method
# Summary: Where there is an NA for every State-Year, multiply No. of Operations for that State-Year by Avg Dollars per Operation in that Census Year
# Make sure to manually check some values
gf_moll_ts <- moll_ts %>%
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


# Check gapfilling e.g. Alaska had all NA values, for loop values should print TRUE for all three census years

# for(i in 1:3){ # i=1
# 
# census <- c(1998, 2005, 2013)
# 
# # dollars is the gapfilled value
# dollars <- filter(gf_moll_ts, State == "ALASKA" & Year == census[i])["DOLLARS"]
# num_op <- filter(gf_moll_ts, State == "ALASKA" & Year == census[i])["OPERATIONS"]
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
moll_dolop_plot <- gf_moll_ts %>% 
  mutate(Dol_per_Op = DOLLARS/OPERATIONS) %>% 
  select(-Species) %>% 
  mutate(Year = as.character(Year))




## SHELLFISH PRODUCTION BASELINE STATS ##
# Read in tidied US Mollusk data
filename = paste("data/int/usda_mollusk/US_sales_all_tidy_", update_date, ".csv", sep="")
stats <- read.csv(filename)

# which species has the largest $ share?
sales <- stats %>%
  filter(Unit == "DOLLARS",
         Year == 2018,
         Species %in% c("ABALONE", "CLAMS", "MUSSELS", "OTHER SPECIES", "OYSTERS"), # select the umbrella categories
         Product_Type == "ALL PRODUCTS", # select totals for all products
         State == "US") %>% 
  select(Species, Unit, Value) %>% 
  arrange(desc(Value)) %>% 
  janitor::adorn_totals("row") %>% # add a row with totals of the Values if no NAs
  mutate(Percent = Value/.[.$Species == "Total",][["Value"]]) # divide by total value

# which state has the largest $ share?
state <- stats %>%
  filter(Unit == "DOLLARS",
         Year == 2018,
         Species == "MOLLUSKS",
         Product_Type == "ALL PRODUCTS") %>% 
  select(State, Unit, Value) %>% 
  arrange(desc(Value))

state$Value[2]/state$Value[1] # no. 1 is US, no. 2 is top producing state

# Which states have the most number of operations
ops <- stats %>% 
  filter(Unit == "OPERATIONS",
         Year == 2018,
         Species == "MOLLUSKS",
         Product_Type == "ALL PRODUCTS") %>%
  select(State, Unit, Value) %>%  
  arrange(desc(Value)) %>% 
  mutate(Percent = Value/.[.$State == "US",][["Value"]]) # divide by US value




## Data Download
# combine with USDA food fish dt
filename = paste('data/int/usda_mollusk/US_sales_all_tidy_', update_date, '.csv', sep='')
USDA_shell <- read.csv(filename) %>% 
  filter(!Product_Type %in% c("RETAIL", "WHOLESALE")) %>% 
  select(-State_Code, -Commodity, -Wholesale_Type)
filename = paste('data/int/usda_fish/US_sales_all_tidy_', update_date, '.csv', sep='')
USDA_fish <- read.csv(filename) %>% 
  filter(!Product_Type %in% c("RETAIL", "WHOLESALE")) %>% 
  # select(-State_Code, -Commodity, -Wholesale_Type)
  select(-State_Code, -Commodity, -Wholesale_Type, -Ag.District, -Ag.District.Code, -County, -County.ANSI)
usdaTable <- USDA_shell %>% 
  rbind(USDA_fish)

filename = paste("data/int/usdaTable_", update_date, ".csv", sep="")
write.csv(usdaTable, filename, row.names = FALSE)
