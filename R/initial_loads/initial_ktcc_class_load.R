#add ktcc classificartions


#read function from functions file-------------------
source("R/functions.R")

#packages---------------------------
library(openxlsx)
library(tidyverse)

#import---------------------------

#data
climate_data <- read.csv("data/scraped_data.csv", stringsAsFactors = F)

#tkcc descriptions
tkcc <- read.csv("data/tkcc_classifications.csv")

#reformat data-------------------------------------

wrangled <- climate_data |>
  #sort out where both rainfall and precipitation included
  mutate(
    metric = ifelse(
      metric %in% c("Average rainfall", "Average precipitation"),
      "Average precipitation",
      metric
      )
    ) |>
  group_by(city, Month, metric, units) |>
  filter(value == max(value, na.rm = T)) |>
  ungroup() |>
  #only relevant classifications, in C/mm
  filter(metric %in% c("Daily mean", "Average precipitation") & units %in% c("°C", "mm")) |>
  select(-units) |>
  distinct() |>
  pivot_wider(names_from  = metric, values_from = value) |>
  filter(!is.na(`Daily mean` & !is.na(`Average precipitation`)))

#get ktcc classifications-----------------------------------------------------

#locations
locations <- unique(wrangled$city)

#datasets
datasets <- lapply(
  locations,
  function(x) wrangled |> filter(city == x)
)

#get classifcations
ktc_classes <- list()

for (i in 1:length(datasets)) {
  
  ktc_classes[i] <- get_ktc_class(datasets[[i]], Month, `Daily mean`, `Average precipitation`, hemisphere)
  
}
names(ktc_classes) <- locations
