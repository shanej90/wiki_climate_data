#reading from wikipedia-----------------------------------------------------

#get location from wiki url
get_location_from_url <- function(.url, conn) {
  
  #get city cpde from url table
  city_code <- dbReadTable(conn, "urls") |>
    filter(url == .url) |>
    pull(city_code)
  
  #read page
  html <- read_html(.url)
  
  #get h1
  title <- html |> html_nodes("#firstHeading") |> html_text() 
  
  #get coordinates
  latitude <- html |> html_nodes(".latitude") |> html_text() |> unique()
  longitude <- html |> html_nodes(".longitude") |> html_text() |> unique()
  # coords_url <- html |> html_nodes(".external") |> html_attr("href")
  # coords_url <- coords_url[str_detect(coords_url, "geohack")]
  # coords <- coords_url[[1]] |> str_extract("(?<=params\\=)(.*)(?=region)")
  # latitude <- coords |> str_extract("(.*)(?=_S|_N)")
  # longitude <- coords |> str_extract("(?<=S_|N_)(.*)(?=_E|_W)")
  
  #extract degrees and minites
  lat_d <- str_extract(latitude, "(.*)(?=°)")
  lat_m <- str_extract(latitude, "(?<=°)(.*)(?=′)")
  #lat_d <- word(latitude, 1, sep = "_")
  #lat_m <- word(latitude, 2, sep = "_")
  lat_multiplier <- ifelse(str_detect(latitude, "N"), 1, -1)
  
  long_d <- str_extract(longitude, "(.*)(?=°)")
  long_m <- str_extract(longitude, "(?<=°)(.*)(?=′)")
  #long_d <- word(longitude, 1, sep = "_")
  #long_m <- word(longitude, 2, sep = "_")
  long_multiplier <- ifelse(str_detect(longitude, "E"), 1, -1)
 
  #update latitude/longitude
  latitude <- lat_multiplier * (as.numeric(lat_d) + (as.numeric(lat_m) / 60))
  longitude <- long_multiplier * (as.numeric(long_d) + (as.numeric(lat_m) / 60))
  
  #grab country
  country_tbl <- html |> 
    html_nodes(".infobox") |> 
    html_table()
  
  country_tbl <- as.data.frame(country_tbl[[1]]) |> clean_names()
  
  if(nrow(country_tbl) == 0) {
    
    country <- "To be determined"
    
  } else {
    
    country_data <- country_tbl %>%
    filter(.[[1]] == "Country")
    
    country <- country_data[1, 2] |> 
      str_remove("(?<=\\[)(.*)(?=\\])") |> 
      str_remove("\\[") |> str_remove("\\]")
    
    if(is.na(country)) {country <- "To be determined"}
    
    if(country %in% c("England", "Scotland", "Wales", "Northern Ireland")) {
      
      country <- "United Kingdom"
    }
    
    if(country == "Georgia") {country <- "Georgia_(country)"}
    
    if(nrow(country_data) == 0) {
      
      country <- "To be determined"
      
    }
    
    
  }
  
  #country html
  if(country != "To be determined") {
    
    country_html <- read_html(paste0("https://en.wikipedia.org/wiki/", str_replace_all(country, " ", "_")))
    
  }
  
  #get country alpha2 from infobox
  if(country != "To be determined") {
    
    country_iso2 <- country_html |>
      html_nodes(".infobox") |>
      html_table()
    
    country_iso2 <-  as.data.frame(country_iso2[[1]])
    
    colnames(country_iso2) <- paste0("X", c(1:length(colnames(country_iso2))))
      
    country_iso2 <- country_iso2 |>
      filter(X1 == "ISO 3166 code") |>
      pull(X2)
    
    if(is_empty(country_iso2)) {country_iso2 <- "TB"}
    
  } else {country_iso2 <- "TB"}
  
  #final dataframe
  df <- data.frame(
    "id" = city_code,
    "city_name" = title |> str_remove("Climate of "),
    "alpha2" = substr(country_iso2, 1, 2),
    "latitude" = ifelse(is_empty(latitude), NA_real_, latitude),
    "longitude" = ifelse(is_empty(longitude), NA_real_, longitude)
  )
  
  
}

#scrape wikipedia data-
get_climate_data <- function(.url) {
  
  #set url
  #url <- wikiDataset$url[match(location, wikiDataset$city_name)]
  
  #connect to db
  conn <- connect_to_db()
  
  #pull urls table to get city code
  city <- dbReadTable(conn, "urls") |>
    filter(url == .url) |>
    pull(city_code)
  
  #read page
  html <- read_html(.url)
  
  #get tables
  all_tbls <- html_nodes(html, "table")
  climate_tbl <- all_tbls[grep("climate data", all_tbls, ignore.case = T)]
  climate_df <- html_table(climate_tbl[[1]]) #always take the first one
  
  #fix for if months are header rather than first row
  if("Month" %in% colnames(climate_df)) {
    
    first_row <- colnames(climate_df)
    names(first_row) <- colnames(climate_df)
    
    climate_df <- bind_rows(first_row, climate_df)
    
  }
  
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
    pivot_longer(cols = !starts_with("Month"), names_to = "metric", values_to = "value") |>
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
      time_period = timeband, 
      value = ifelse(is.na(as.numeric(value)), -as.numeric(substr(value, 2, length(value))), as.numeric(value))
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
    filter(!is.na(units)) |>
    transmute(
      city_code = city,
      month = Month,
      metric,
      units,
      value,
      time_period
    ) |>
    filter(!is.na(value)) |>
    distinct()
  
  
}



#update sqlite db-------------------------------------------------------

#connect to database
connect_to_db <- function() {
  
  dbConnect(RSQLite::SQLite(), "wiki-urls.db")
  
}

#add urls
add_url <- function(conn, .url) {
  
  #check existing data
  current_data <- dbReadTable(conn, "urls")
  
  #is current url already in?
  if(.url %in% current_data$url) {
    
    return("URL already in database. Upload aborted.")
    
  }
  
  #create upload df
  upload <- data.frame(
    "url" = .url,
    "city_code" = ifelse(
      max(current_data$city_code) >= 1,
      max(current_data$city_code) + 1,
      1
    )
  )
  
  #write to sqlite
  dbAppendTable(conn, "urls", upload)
  
  
}

#add cities
add_city <- function(conn, city_data) {
  
  #pull the countries table
  countries <- dbReadTable(conn, "countries")
  
  #existing cities data
  cities <- dbReadTable(conn, "cities")
  
  #make sure city is in url tanle
  urls <- dbReadTable(conn, "urls")
  
  if(!unique(city_data$id) %in% urls$city_code) {
    
    return("No matching URL found - upload this first. Aborting process.")
  
}
  
  #check if city alread exists
  if(city_data$id %in% cities$id) {
    
    return("City already in DB, aborting process.")
    
  }
  
  #get coutry code, remove iso2
  upload_df <- city_data |>
    left_join(
      countries |>
        select(alpha2, country_code),
      by = "alpha2"
    ) |>
    transmute(
      id,
      city_name,
      country_code,
      latitude,
      longitude
    )
  
  #write to sqllite
  dbAppendTable(conn, "cities", upload_df)
  
  
}

#add climate data
add_climate_data <- function(conn, climate_data) {
  
  #cities table
  cities <- dbReadTable(conn, "cities")
  
  #check city is in table
  if(!unique(climate_data$city_code) %in% cities$id) {
    
    return("City not found in 'cities' table - upload first. Aborting process.")
    
  }
  
  #climate data table
  current_data <- dbReadTable(conn, "climate_data")
  
  #check if climate data already in
  if(unique(climate_data$city_code) %in% current_data$city_code) {
    
    return("City already has a record in DB, aborting process.")
    
  }
  
  #send in data
  dbAppendTable(conn, "climate_data", climate_data)
  
}

#add climate classifications
add_climate_classes <- function(conn, city_id) {
  
  #climate data
  raw_data <- dbReadTable(conn, "climate_data") |>
    filter(city_code == city_id) |>
    distinct(
      city_code, 
      month, 
      metric, 
      units,
      value, 
      time_period, 
      .keep_all = T
      )
  
  #check data avilable
  if(!city_id %in% raw_data$city_code) {
    
    return("City not found in database, aborting process.")
    
  }
  
  #city data to identify country
  cities <- dbReadTable(conn, "cities") |>
    filter(id == city_id)
  
  #country data to get hemisphere
  country <- dbReadTable(conn, "countries") |>
    filter(country_code == cities$country_code)
  
  #checx both daily mean and average precipiation are available
  if(
    !"Daily mean" %in% raw_data$metric | 
    (
      !"Average rainfall" %in% raw_data$metric &
      !"Average precipitation" %in% raw_data$metric
      )
    ) {
    
    return("Not enough data to classify, aborting process.")
    
  }
  
  #reformat data
  reformatted <- raw_data |>
    #sort out where both rainfall and precipitation included
    mutate(
      metric = ifelse(
        metric %in% c("Average rainfall", "Average precipitation"),
        "Average precipitation",
        metric
      )
    ) |>
    group_by(month, metric, units) |>
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
    mutate(country_code = unique(country$country_code)) |>
    #add oin hemisphere
    left_join(
      country |>
        select(country_code, hemisphere),
      by = "country_code"
    )
  
  #check there is data left to process
  if(nrow(reformatted) == 0) {
    
    
    return("Not enough data for city to classify, aborting process.")
    
  }
  
  #check hemisphere and stop if TBD
  if(unique(reformatted$hemisphere) == "TBD") {
    
    return("Hemisphere not set so cannot determine climate class, aborting process.")
    
  }
  
  #ktc class
  ktc_class <- get_ktc_class(
    reformatted,
    month,
    `Daily mean`,
    `Average precipitation`,
    hemisphere
    )
  
  #kgc class
  kgc_class <- get_kgc_class(
    reformatted,
    month,
    `Daily mean`,
    `Average precipitation`,
    hemisphere
  )
  
  #climate class lookup
  class_lookup <- dbReadTable(conn, "climate_class")
  
  #convert ktc/kgc to number
  ktc_num <- class_lookup |> 
    filter(classification_id == 1 & class_code == ktc_class) |>
    pull(class_id) |>
    as.numeric()
  
  kgc_num <- class_lookup |>
    filter(classification_id == 2 & class_code == kgc_class) |>
    pull(class_id) |>
    as.numeric()
  
  #upload df
  upload_df <- data.frame(
    "class_id" = c(ktc_num, kgc_num),
    "city_id" = rep(city_id, 2)
  )
  
  #upload
  dbAppendTable(conn, "classes_per_city", upload_df)
    
  
}

#climatye sclasses----------------------------------------------

#ktc
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
    pull({{temp_col}}) |>
    unique()
  
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
    pull({{temp_col}}) |>
    unique()
  
  #coldest month check already done as part of tropical checks
  
  #driest summer month
  dsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #driest winter month
  dwp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #wettest summer month
  wsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #wettest winter month
  wwwp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #temperature of warmest four months
  wfm <- df |>
    arrange(desc({{temp_col}})) |>
    slice(1:4) |>
    pull({{temp_col}}) |>
    unique()
  
  #driest month prcp
  dmp <- df |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #give classification###########################################
  
  ktcc <- case_when(
    #tropical
    cmt >= 18 & dmn < 3 ~ "Ar",
    cmt >= 18 & sdm >= 3 ~ "As",
    cmt >= 18 & wdm >= 3 ~ "Aw",
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
    m10 >= 8 & cmt < 18 ~ "Cf",
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

#add koppen-geigfer classification
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
  cmt <- df |> pull({{temp_col}}) |> min(na.rm = T) |> unique()
  
  #driest month precipitation
  dmp <- df |> pull({{prcp_col}}) |> min(na.rm = T) |> unique()
  
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
    pphs >= 70 ~ (mt * 20) + 280,
    pphs < 70 & pphs >= 30 ~ (mt * 20) + 140,
    TRUE ~ mt * 20
  ) |> 
    unique()
  
  #warmest month temp
  wmt <- df |> pull({{temp_col}}) |> max(na.rm = T) |> unique()
  
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
    pull({{prcp_col}}) |>
    unique()
  
  #driest low-sub month prcp
  dlsp <- df |>
    filter(!{{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #wettest high sun month prcp
  whsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == max({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
  #driest high sun month precipitation
  dhsp <- df |>
    filter({{month_col}} %in% high_sun_months) |>
    filter({{prcp_col}} == min({{prcp_col}}, na.rm = T)) |>
    pull({{prcp_col}}) |>
    unique()
  
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
    cmt >= 18 & dmp < 60 & dmp < tmwt & pphs >= 50 ~ "Aw",
    cmt >= 18 & dmp < 60 & dmp < tmwt & pphs < 50 ~ "As",
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

#FULL UPLOAD PROCESS-------------------------------------------------------

full_data_upload <- function(URL) {
  
  #connect to database
  dbCon <- connect_to_db()
  
  #upload URL
  add_url(dbCon, URL)
  
  message("Uploaded URL")
  
  #get location from the URL
  CITY <- get_location_from_url(URL, dbCon)
  
  #upload the city
  add_city(dbCon, CITY)
  
  message("Uploaded city")
  
  #scrtape the climate data
  CLIMATE <- get_climate_data(URL)
  
  #upload climate data
  add_climate_data(dbCon, CLIMATE)
  
  message("Uploaded climate data")
  
  #add climate classes
  add_climate_classes(dbCon, CITY$id)
  
  message("Uploaded climate classifications - complete")
  
  #disconnect
  DBI::dbDisconnect(dbCon)
  
}




