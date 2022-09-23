#import functon
source("R/functions.R")

#packages
library(DBI)
library(janitor)
library(rvest)
library(RSQLite)
library(tidyverse)

#import list of urls----------

urls <- read.csv("data/urls_to_upload.csv", header = F) |>
  as.vector() |>
  unlist()

#run-------------------------------

#do as loop as functions don't work properly in lapply/purrr::map for some reason

for (u in urls) {
  
  full_data_upload(u)
  
}

#fix for climate classes not uploaded------------------

# fix_ids <- c(28)
# 
# for (c in fix_ids) {
# 
#   add_climate_classes(connect_to_db(), c)
# 
# }

