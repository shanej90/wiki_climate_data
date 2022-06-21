#import functions----------------------------
source("R/functions.R")

#packages------------------------------
library(DBI)
library(glue)
library(rvest)
library(RSQLite)
library(tidyverse)

#get wikipedia data------------------------------------------------

#connect to sqlite db
wikiDb <- dbConnect(SQLite(), "wiki-urls.db")

#read pertientn tables
tbl_names <- c("countries", "cities", "urls")

wikiDbDfs <- lapply(
  tbl_names,
  dbReadTable,
  conn = wikiDb
)
names(wikiDbDfs) <- tbl_names

#create usable dataset
wikiDataset <- left_join(
  wikiDbDfs[["cities"]],
  wikiDbDfs[["countries"]],
  by = "country_code"
) |>
  left_join(
    wikiDbDfs[["urls"]],
    by = c("id" = "city_code")
  )

#grab wikipedia climate data
wikiClimate <- lapply(
  wikiDataset$city_name,
  safely(function(x) scrape_wiki(x))
) |>
  map_dfr("result")

#write data--------------
write.csv(wikiClimate, "data/scraped_data.csv", row.names = F)
