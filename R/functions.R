#scrape wikipedia data-

scrape_wiki <- function(location) {
  
  #set url
  url <- wikiDataset$url[match(location, wikiDataset$city_name)]
  
  #read page
  html <- read_html(url)
  
  #get tables
  all_tbls <- html_nodes(html, "table")
  climate_tbl <- all_tbls[grep("climate data", all_tbls, ignore.case = T)]
  climate_df <- html_table(climate_tbl[[1]]) #always take the first one
  
  #extract the timeframe
  timeband <- colnames(climate_df)[1] |>
    str_extract("[1-2][0-9][0-9][0-9]–[1-2][0-9][0-9][0-9]")
  
  #new names for columns, ensure consistency
  new_names <- paste0("col_", c(1:length(colnames(climate_df))))
  
  #wrangle the climate df
  wrangled <- climate_df |>
    stats::setNames(new_names) |>
    #relevant rows
    filter(
      str_detect(col_1, "Month|maximum|minimum|mean|precipitation|rain|humidity|sunshine|high|low") &
        !str_detect(col_1, "Source")
    ) |>
    #tall
    pivot_longer(
      cols = -col_1,
      names_to = "temp",
      values_to = "values"
    ) |>
    #temproarily back to wide
    pivot_wider(names_from = col_1, values_from = "values") |>
    #remove extraneous column
    select(-temp) |>
    #taller format
    pivot_longer(cols = -Month, names_to = "metric", values_to = "value") |>
    #remove annual total/average
    filter(Month != "Year") |>
    #get units
    mutate(
      units_1 = case_when(
        str_detect(metric, "\\°") ~ word(metric, 3),
        str_detect(metric, "rain|precip") & str_detect(metric, "mm|inch") & !str_detect(metric, "days") ~ word(metric, 3),
        str_detect(metric, "days") ~ "days",
        str_detect(metric, regex("humidity|percent", ignore_case = T)) ~ "%",
        str_detect(metric, "hour") ~ "hours"
      ),
      units_2 = case_when(
        str_detect(metric, "\\°") ~ word(metric, 4),
        str_detect(metric, "rainfall|precip") & str_detect(metric, "mm|inch") ~ word(metric, 4)
      ),
      value_2 = str_extract(value, "(?<=\\()(.*)(?=\\))"),
      value_1 = str_remove(value, "(?<=\\()(.*)(?=\\))") |> str_remove_all("\\(|\\)")
    ) |>
    mutate(across(contains("unit"), function(x) str_remove_all(x, "\\(|\\)"))) |>
    select(-value) |>
    #rename metric column for clarity
    mutate(
      metric = str_remove_all(
        metric, "\\(|\\)|\\°F|\\°C|mm|inches|hours||days|Percent|percent"
      ) 
    ) |>
    #sort out values and units
    pivot_longer(
      cols = c(units_1:value_1),
      names_to = "temp",
      values_to = "temp_values"
    ) |>
    separate(
      temp,
      into = c("temp1", "temp2"),
      sep = "_"
    ) |>
    pivot_wider(names_from = temp1, values_from = temp_values) |>
    select(-temp2) |>
    #tidy up data
    mutate(
      city = location,
      time_period = timeband, 
      value = ifelse(is.na(as.numeric(value)), -as.numeric(substr(value, 2, length(value))), as.numeric(value))
    ) |>
    #join on pertientn country data
    left_join(
      wikiDataset |>
        transmute(city_name, country = name, hemisphere),
      by = c("city" = "city_name")
    ) |>
    #standardise metrics/variables
    mutate(
      metric = str_trim(metric, side = "both"),
      metric = case_when(
        str_detect(metric, "rainy days|precipitation days") | (str_detect(metric, "rainy|precipitation") & str_detect(metric, "days")) ~
          "Average precipitation/rainfall days",
        str_detect(metric, "humidity") ~ "Humidity",
        TRUE ~ metric
      )
    ) |>
    filter(!is.na(units))
  
  
}

#define ktc class----------------------

get_ktc_class <- function(df, month_col, temp_col, prcp_col, hemisphere_col) {
  
  #
  
  #arrange by month
  df <- df |>
    mutate(month_num = match({{month_col}}, month.abb)) |>
    arrange(month_num)
  
  #general#######################################
  
  #hemisphere
  hemisphere <- df |> select({{hemisphere_col}}) |> pull() |> unique()
  
  #high sun months
  if(hemisphere == "North") {
    
    high_sun_months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep")
    
  } else {
    
    high_sun_months <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
    
  }
  
  #total rainfall
  tr <- df |> pull({{prcp_col}}) |> sum(na.rm = T)
  
  #mean annual temp
  mt <- df |> pull({{temp_col}}) |> mean(na.rm = T)
  
  #checks for tropical classifications#####################################
  
  #tcoldest month temp
  cmt <- df |>
    filter({{temp_col}} == min({{temp_col}}, na.rm = T)) |>
    pull({{temp_col}})
  
  #summer dry months
  sdm <- df |>
    filter({{prcp_col}} < 60 & {{month_col}} %in% high_sun_months) |>
    nrow()
  
  #winter dry months
  wdm <- df |>
    filter({{prcp_col}} < 60 & !{{month_col}} %in% high_sun_months) |>
    nrow()
  
  #total number of dry months
  dmn <- df |>
    filter({{prcp_col}} < 60) |>
    nrow()
  
  #checks for dry / semi-arid classifications################################
  
  #split
  prcp_by_season <- df |>
    mutate(dry = ifelse({{month_col}} %in% high_sun_months, "Yes", "No")) |>
    group_by(dry) |>
    summarise(prcp = sum({{prcp_col}}, na.rm = T)) |>
    ungroup() |>
    mutate(pct = 100 * prcp / sum(prcp))
  
  #% rain in summer
  pps <- prcp_by_season |>
    filter(dry == "Yes") |>
    pull(pct)
  
  #% rain in winter
  ppw <- prcp_by_season |>
    filter(dry == "No") |>
    pull(pct)
  
  #number of months 10C or above
  m10 <- df |>
    filter({{temp_col}} >= 10) |>
    nrow()
  
  #precipitation threshold
  dpt <- (2.3 * mt) - (0.64 * ppw) + 410
  
  
  #checks for temperate classification#########################################
  
  #warmest month temp
  wmt <- df |>
    filter({{temp_col}} == max({{temp_col}}, na.rm = T)) |>
    pull({{temp_col}})
  
  #coldest month check already done as part of tropical checks
  
  #driest summer month
  dsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #driest winter month
  dwp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #wettest summer month
  wsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #wettest winter month
  wwwp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #temperature of warmest four months
  wfm <- df |>
    arrange(desc({{temp_col}})) |>
    slice(1:4) |>
    pull({{temp_col}})
  
  #driest month prcp
  dmp <- df |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #give classification###########################################
  
  ktcc <- case_when(
    #tropical
    cmt >= 18 & dmn < 3 ~ "Ar",
    cmt >= 18 & sdm >= 3 ~ "As",
    cmt >= 18 & wdm >= 3 ~ "Aw",
    cmt >= 18 & dmn >= 3 & dmn < 3 & wdm < 3 ~ "Am",
    #dry
    tr < dpt / 2 & 
      cmt > 0 & cmt < 18 & m10 >= 8 ~ "BWh",
    tr < dpt / 2 & 
      cmt <= 0 & cmt < 18 & m10 < 8 ~ "BWk",
    tr >= (dpt / 2) & tr < dpt &
      cmt > 0 & cmt < 18 & m10 >= 8 ~ "BSh",
    tr >= (dpt / 2) & tr < dpt &
      cmt <= 0 & cmt < 18 & m10 < 8 ~ "BSk",
    #subtropical
    m10 >= 8 & cmt < 18 & tr < 890 & dsp < 30 & pps <= (ppw / 3) ~ "Cs",
    m10 >= 8 & cmt < 18 & pps >= (10 * ppw)  ~ "Cw",
    m10 >= 8 & cmt < 18 & dsp >= 30 & pps > (ppw /3) & pps < (10 * ppw) ~ "Cf",
    #temperate
    m10 < 8 & m10 >= 4 & cmt > 0 ~ "Do",
    m10 < 8 & m10 >= 4 & cmt <= 0 ~ "Dc",
    #boreal climayes
    m10 %in% c(1:3) ~ "E",
    #polar
    wmt < 10 & wmt > 0 ~ "Ft",
    wmt <= 0 ~ "Fi"
  ) |> 
    unique()
  
}

#add koppen-geigfer classification##############################################

get_kgc_class <- function(df, month_col, temp_col, prcp_col, hemisphere_col) {
  
  #arrange by month
  df <- df |>
    mutate(month_num = match({{month_col}}, month.abb)) |>
    arrange(month_num)
  
  #general#######################################
  
  #hemisphere
  hemisphere <- df |> select({{hemisphere_col}}) |> pull() |> unique()
  
  #high sun months
  if(hemisphere == "North") {
    
    high_sun_months <- c("Apr", "May", "Jun", "Jul", "Aug", "Sep")
    
  } else {
    
    high_sun_months <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
    
  }
  
  #total rainfall
  tr <- df |> pull({{prcp_col}}) |> sum(na.rm = T)
  
  #mean annual temp
  mt <- df |> pull({{temp_col}}) |> mean(na.rm = T)
  
  #topical checks###########################################
  
  #coldest month temp
  cmt <- df |> pull({{temp_col}}) |> min(na.rm = T)
  
  #driest month precipitation
  dmp <- df |> pull({{prcp_col}}) |> min(na.rm = T)
  
  #topical mw threshold
  tmwt <- 100 - (tr / 25)
  
  #dry checks################################################
  
  #% prcp in high/low sub months - initial dataframe
  psvw <- df |>
    mutate(high_sun = ifelse({{month_col}} %in% high_sun_months, "Yes", "No")) |>
    group_by(high_sun) |>
    summarise(prcp = sum({{prcp_col}}, na.rm = T)) |>
    ungroup() |>
    mutate(pct = 100 * prcp / sum(prcp))
  
  #% prcp high sun months
  pphs <- psvw |>
    filter(high_sun == "Yes") |>
    pull(pct)
  
  #dry climate prcp threshold
  dcpt <- case_when(
    pphs >= 70 ~ 280,
    pphs < 70 & pphs >= 30 ~ 140,
    TRUE ~ 0
  ) |> 
    unique()
  
  #warmest month temp
  wmt <- df |> pull({{temp_col}}) |> max(na.rm = T)
  
  #temperature of coldest month already calculated in tropical checks
  
  #temperate#################################################
  
  #coldest month climates already checked
  
  #% prcp in high sun months checked
  
  #wramest month temp checked
  
  #number of months >= 10C
  m10 <- df |>
    filter({{temp_col}} >= 10) |>
    nrow()
  
  #wettest low-sun month prcp
  wlsp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #driest low-sub month prcp
  dlsp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #wettest high sun month prcp
  whsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #driest high sun month precipitation
  dhsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}})
  
  #wetetst low sun to driest high sun ratio
  wlsp_dhsp <- wlsp / dhsp
  
  #wettest high sun to driest low sunb ratio
  whsp_dlsp <- whsp / dlsp
  
  #continental##############################################
  
  #all checks already covered
  
  #polar/alp[ine########################################
  
  #all checks already covered
  
  #assign classification########################################
  
  kgc_class <- case_when(
    #tropical
    cmt >= 18 & dmp >= 60 ~ "Af",
    cmt >= 18 & dmp < 60 & dmp >= tmwt ~ "Am",
    cmt >= 18 & dmp < 60 & dmp < tmwt ~ "Aw",
    #dry
    wmt >= 10 & tr < (dcpt / 2) & cmt > 0 ~ "BWh",
    wmt >= 10 & tr < (dcpt / 2) & cmt <= 0 ~ "BWk",
    wmt >= 10 & tr >= (dcpt / 2) & tr < dcpt & cmt > 0 ~ "BSh",
    wmt >= 10 & tr >= (dcpt / 2) & tr < dcpt & cmt <= 0 ~ "BSk",
    #temperate
    cmt > 0 & wmt >= 22 & m10 >= 4 & whsp_dlsp >= 10 ~ "Cwa",
    cmt > 0 & wmt < 22 & m10 >= 4 & whsp_dlsp >= 10 ~ "Cwb",
    cmt > 0 & wmt < 22 & m10 > 0 & m10 < 4 & whsp_dlsp >= 10 ~ "Cwc",
    cmt > 0 & wmt >= 22 & m10 >= 4 & wlsp_dhsp >= 3 & dhsp < 40 ~ "Csa",
    cmt > 0 & wmt < 22 & m10 >= 4 & wlsp_dhsp >= 3 & dhsp < 40 ~ "Csb",
    cmt > 0 & wmt < 22 & m10 > 0 & m10 < 4 & wlsp_dhsp >= 3 & dhsp < 40 ~ "Csc",
    cmt > 0 & m10 >= 4 & wmt >= 22 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Cfa",
    cmt > 0 & m10 >= 4 & wmt < 22 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Cfb",
    cmt > 0 & m10 < 4 & m10 > 0 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Cfc",
    #continenta
    cmt <= 0 & wmt >= 22 & m10 >= 4 & whsp_dlsp >= 10 ~ "Dwa",
    cmt <= 0 & wmt < 22 & m10 >= 4 & whsp_dlsp >= 10 ~ "Dwb",
    cmt <= 0 & wmt < 22 & m10 > 0 & m10 < 4 & whsp_dlsp >= 10 ~ "Dwc",
    cmt <= 0 & cmt < -38 & m10 > 0 & m10 < 4 & whsp_dlsp >= 10 ~ "Dwd",
    cmt <= 0 & wmt >= 22 & m10 >= 4 & wlsp_dhsp >= 3 & dhsp < 30 ~ "Dsa",
    cmt <= 0 & wmt < 22 & m10 >= 4 & wlsp_dhsp >= 3 & dhsp < 30 ~ "Dsb",
    cmt <= 0 & wmt < 22 & m10 > 0 & m10 < 4 & wlsp_dhsp >= 3 & dhsp < 30 ~ "Dsc",
    cmt <= 0 & cmt < -38 & m10 > 0 & m10 < 4 & wlsp_dhsp >= 3 & dhsp < 30 ~ "Dsd",
    cmt <= 0 & m10 >= 4 & wmt >= 22 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Dfa",
    cmt <= 0 & m10 >= 4 & wmt < 22 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Dfb",
    cmt <= 0 & m10 < 4 & m10 > 0 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Dfc",
    cmt <= 0 & cmt < -38 & m10 > 0 & m10 < 4 & (whsp_dlsp < 10 | wlsp_dhsp < 3) ~ "Dfd",
    #polar
    wmt >= 0 & wmt < 10 ~ "ET",
    wmt < 0 ~ "EF" 
  ) |>
    unique()
  
}


