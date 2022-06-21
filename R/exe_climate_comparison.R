#script to give comparison score to exeter climate based on mean temps and precipitation

#packages----------------------
library(tidyverse)

#import---------------------

#climate data
ClimateData <- read.csv("data/scraped_data.csv", stringsAsFactors = F)

#seasonal adjustments
seasonal_adj <- read.csv("data/season_adjustments.csv", stringsAsFactors = F)

#get exeter data----------------

ExeData <- ClimateData |>
  filter(
    city == "Exeter" &
      metric %in% c("Daily mean", "Average precipitation") &
      str_detect(units, "C|mm")
  )

#get climate similarity scores-------------

SimilarityScores <- ClimateData |>
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
  #relevant metrics
  filter(
    metric %in% c("Daily mean", "Average precipitation") &
      str_detect(units, "C|mm") &
      city != "Exeter"
  ) |>
  #make seasonal adjustments
  left_join(
    seasonal_adj,
    by = c("hemisphere", "Month" = "month")
  ) |>
  #join on exeter datya
  left_join(
    ExeData |> transmute(Month, metric, exe_value = value),
    by = c("adjusted_month" = "Month", "metric")
    ) |>
  #calculate difference
  mutate(diff = abs(exe_value - value)) |>
  #scale for each metric 
  group_by(metric) |>
  mutate(scaled = 100 * (1 - (diff - min(diff, na.rm = T)) / (max(diff, na.rm = T) - min(diff, na.rm = T)))) |>
  ungroup() |>
  group_by(city) |>
  summarise(score = mean(scaled)) |>
  ungroup()
