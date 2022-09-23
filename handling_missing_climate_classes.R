#script to deal with missing climate classes

#packages---------------- 
library(DBI)
library(tidyverse)

#source functions--------------
source("R/functions.R")

#connect to the database and identify cities with missing climate classifications----------

conn <- connect_to_db()

cities <- dbReadTable(conn, "cities")
cc_per_city <- dbReadTable(conn, "classes_per_city")
country <- dbReadTable(conn, "countries")
class_lookup <- dbReadTable(conn, "climate_class")

no_cc <- cities |>
  left_join(cc_per_city, by = c("id" = "city_id")) |>
  filter(is.na(class_id))

#get the climate data for these cities and format to run through classification function---------

climate <- dbReadTable(conn, "climate_data") |>
  filter(city_code %in% no_cc$id)

#reformat the data ready to run through the function
climate_rfmt <- climate |>
  #join on country code
  left_join(
    cities |> select(id, city_name, country_code), by = c("city_code" = "id")
  ) |>
  #tidy up overlapping fields
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
  #only relevant classifications, in C/mm
  filter(
    metric %in% c("Daily mean", "Average precipitation") &
      str_detect(units,"C|mm")
  ) |>
  select(-units) |>
  select(-record_no) |>
  distinct() |>
  pivot_wider(names_from = metric, values_from = value) |>
  filter(!is.na(`Daily mean` & !is.na(`Average precipitation`))) |>
  #add oin hemisphere
  left_join(
    country |>
      select(country_code, hemisphere),
    by = "country_code"
  )

#run the data through the classification system-------------------

#empty list to hold results
results <- list()

#loop
for (c in unique(climate_rfmt$city_code)) {
  
  #filter df
  df <- climate_rfmt |> filter(city_code == c)
  
  #city name
  city <- unique(df$city_name)
  
  #ktc class
  ktc_class <- get_ktc_class(
    df,
    month,
    `Daily mean`,
    `Average precipitation`,
    hemisphere
  )
  
  #kgc class
  kgc_class <- get_kgc_class(
    df,
    month,
    `Daily mean`,
    `Average precipitation`,
    hemisphere
  )
  
  #convert ktc/kgc to number
  ktc_num <- class_lookup |> 
    filter(classification_id == 1 & class_code == ktc_class) |>
    pull(class_id) |>
    as.numeric()
  
  kgc_num <- class_lookup |>
    filter(classification_id == 2 & class_code == kgc_class) |>
    pull(class_id) |>
    as.numeric()
  
  # 'upload' df
  upload_df <- data.frame(
    "class_id" = c(ktc_num, kgc_num),
    "city_id" = rep(c, 2),
    "city_name" = rep(city, 2)
  )
  
  #update list
  results[[match(c, unique(climate_rfmt$city_code))]] <- upload_df
  
  
}

results_df <- results |> bind_rows()

#upload
dbAppendTable(conn, "classes_per_city", results_df |> select(-city_name))

