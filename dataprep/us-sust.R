
library(tidyverse)
library(RJSONIO)
library(nominatim) # devtools::install_github("hrbrmstr/nominatim")

cert <- read.csv("data/raw/Bluenote/bap_cert.csv")

us_certs <- cert %>% 
  filter(Country == "United States")

# generate key from https://developer.mapquest.com/user/me/apps
w <- osm_search("Buhl Idaho", 
                key = "YjGc67SfpnlF8DkAJ5rPwkAsLC4PS7qF")

practices <- read.csv("data/raw/USDA_Quickstats/aq_practices.csv")
