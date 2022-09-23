#prep data for Shiny app----------------------

#read in functions that may be useful----------
source("R/functions.R")

#packages that may be useful-------------------
library(DBI)
library(RSQLite)
library(tidyverse)

#import data-------------------

#connect
conn <- connect_to_db()

#read tables
tables <- lapply(
  dbListTables(conn),
  dbReadTable,
  conn = conn
)

names(tables) <- dbListTables(conn)

#tidy up climate data ready for use in app--------

tables[["climate_data"]] <- tables[["climate_data"]] |>
  #step 1: standardise precipitation/rainfall
  mutate(
    metric = ifelse(
      metric %in% c("Average rainfall", "Average precipitation"),
      "Average precipitation",
      metric
    )
  ) |>
  group_by(city_code, month, metric, units) |>
  filter(value == max(value, na.rm = T)) |>
  ungroup() |>
  select(-record_no) |>
  distinct() |>
  #step 2: standardise sunshine data
  mutate(
    value = case_when(
      str_detect(metric, "daily sun") & 
        month %in% c("Jan", "Mar", "May", "Jul", "Aug", "Oct", "Dec") ~ 31 * value,
      str_detect(metric, "daily sun") &
        month %in% c("Apr", "Jun", "Sep", "Nov") ~ 30 * value,
      str_detect(metric, "daily sun") & month == "Feb" ~ 28 * value,
      TRUE ~ value
    ),
    metric = case_when(
      str_detect(metric, "daily sun") ~ "Mean monthly sunshine",
      TRUE ~ metric
    )
  ) |>
  group_by(city_code, month, metric, units) |>
  filter(value == max(value, na.rm = TRUE)) |>
  ungroup() |>
  distinct()

#calculate annual summarise for use in app-------------------

ann_summaries <- tables[["climate_data"]] |>
  #include number of days for calculations where required
  mutate(
    days = case_when(
      month %in% c("Jan", "Mar", "May", "Jul", "Aug", "Oct", "Dec") ~ 31,
      month %in% c("Apr", "Jun", "Sep", "Nov") ~ 30,
      month == "Feb" ~ 28
    ),
    weighted_values = days * value
  ) |>
  #summarise
  group_by(city_code, metric, units, time_period) |>
  summarise(
    max = max(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    total = sum(value, na.rm = TRUE),
    mean = sum(weighted_values, na.rm = TRUE) / sum(days, na.rm = TRUE)
  ) |>
  ungroup() |>
  #tall format
  pivot_longer(
    cols = c(max:mean),
    names_to = "stat",
    values_to = "summary_value"
  ) |>
  #only relevant/sensible combos
  filter(
    (metric == "Record high" & stat == "max") |
      (metric == "Record low" & stat == "min") |
      (units %in% c("hours", "mm", "inches", "days") & stat == "total") |
      (str_detect(units, "C|F") & !str_detect(metric, "Record") & stat == "mean")
  )

#get climate class per city---------------------

climate_classes <- tables[["cities"]] |>
  select(id) |>
  #classifications where available
  inner_join(tables[["classes_per_city"]], by = c("id" = "city_id")) |>
  #classification description
  left_join(tables[["climate_class"]], by = "class_id") |>
  #class systen
  left_join(tables[["classifications"]], by = c("classification_id" = "class_id"))

#city list--------------------

city_list <- tables[["cities"]] |>
  #join country data
  left_join(tables[["countries"]], by = c("country_code" = "country_code")) |>
  #create labels
  mutate(ui_label = paste0(city_name, " (", name, ")"))

#metric options--------------

metrics <- tables[["climate_data"]] |>
  mutate(ui_label = paste0(metric, " (", units, ")"))

