#script for running quick analyses on database

#packages---------------- 
library(DBI)
library(tidyverse)

#source functions--------------
source("R/functions.R")

#connect-----------------
conn <- connect_to_db()

#load data--------------------

#cities
cities <- dbReadTable(conn, "cities")

#classifications per city
cpc <- dbReadTable(conn, "classes_per_city")

#classification types
classes <- dbReadTable(conn, "climate_class")

#class types
class_types <- dbReadTable(conn, "classifications")

#countries
countries <- dbReadTable(conn, "countries")

#climate classification-------------

classData <- cities |>
  select(id, city_name, country_code) |>
  #join on pertinent country data
  left_join(
    countries |>
      transmute(country_code, country = name),
    by = "country_code"
  ) |>
  #classifications where available
  inner_join(cpc, by = c("id" = "city_id")) |>
  #classification description
  left_join(classes, by = "class_id") |>
  #class systen
  left_join(class_types, by = c("classification_id" = "class_id"))

clipr::write_clip(classData)

