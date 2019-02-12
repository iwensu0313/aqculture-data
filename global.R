## global variables
## sources goal data for map and chart modules
## defines global variables, source code, etc


## SET UP ENVIRONMENT ##

## libraries
library(sf)
library(tidyverse)
library(shinydashboard)
library(RColorBrewer)
library(plotly)
library(sunburstR)
library(viridis)
library(stringr)
library(USAboundaries)
library(DescTools)
library(janitor)
library(sendmailR)
library(shinyAce)
library(DT)
library(cowplot)


## Color Palettes
# Eliminate super light yellow colors
# Good for both colorQuantile() and colorNumeric()
ygb <- colorRampPalette(brewer.pal(5,'YlGnBu'))(200) # Yellow, Green, Blue
ygb_cols <- ygb[19:200] 

OrRd <- colorRampPalette(brewer.pal(5, 'OrRd'))(200) # Orange, Red
OrRd_cols <- OrRd[19:200]


## SOURCE EXTERNAL SCRIPTS ##

## source modules
source("modules/chart_card.R")
source("modules/choro_map_card.R")
source("modules/minichart_map_card.R")
source("modules/dot_map_card.R")
source("modules/summary_stats_card.R")

## source functions
source("functions/helper_functions.R")
source("functions/ui_functions.R")

## source front page css/html customization
source("front_page.R")

## set: no scientific notation and round to 2 decimals
options(scipen = 999,
        digits = 5)




## CA AQUACULTURE DATA SOURCES ##
source("dataprep/ca-prod.R")
source("dataprep/ca-imports.R")

## US AQUACULTURE DATA SOURCES ##
source("dataprep/us-fish.R")
source("dataprep/us-shell.R") # combines us-fish and us-shell

## SHRIMP IMPORT REFUSALS ##
source("dataprep/us-import-refusals.R")
