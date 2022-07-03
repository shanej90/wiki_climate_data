#packages------------------------
library(factoextra)
library(tidyverse)

#load data-------------------

#seasonal adjustmenets for southern hemisphere
seasonal_adj <- read.csv("data/season_adjustments.csv", stringsAsFactors = F)

#actual data
data <- read.csv("data/initial_loads/scraped_data.csv") |>
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
  #make seasonal adjustments
  left_join(
    seasonal_adj,
    by = c("hemisphere", "Month" = "month")
  ) |>
  #dop columns not used
  select(-c(Month, time_period, country, hemisphere)) |>
  #wide format
  pivot_wider(values_from = value, names_from = c(adjusted_month,metric)) |>
  na.omit()

#clustering----------------------------------------------

#data for kmeans analysis
for_analysis <- as.data.frame(data |> select(-city)) |> scale()
rownames(for_analysis) <- data$city

#k means
km <- kmeans(for_analysis, centers = 5, nstart = 25)

#visualisation
plot <- fviz_cluster(km, data = for_analysis)

  
