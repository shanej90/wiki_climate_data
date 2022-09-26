#app packages-----------
library(DT)
library(leaflet)
library(plotly)
library(shiny)
library(shinydashboard)

#source data (and processing packages)-----------
source("R/app_data_prep.R")
#source("R/reactive_functions.R")

#################
#UI
#################

ui <- dashboardPage(
  
  #header-----------------------------------
  dashboardHeader(title = "Climate explorer"),
  
  #sidebar-----------------------------------
  dashboardSidebar(
    
    #choose metric
    selectInput(
      inputId = "metric",
      label = "Metric",
      choices = metrics |> arrange(ui_label) |> pull(ui_label) |> unique(),
      selected = "Daily mean (°C)"
    ),
    
    #choose locations
    selectInput(
      inputId = "location1",
      label = "Settlement 1",
      choices = city_list |> arrange(ui_label) |> pull(ui_label),
      selected = "Exeter (United Kingdom of Great Britain and Northern Ireland)"
    ),
    selectInput(
      inputId = "location2",
      label = "Settlement 2",
      choices = city_list |> arrange(ui_label) |> pull(ui_label),
      selected = "Plymouth (United Kingdom of Great Britain and Northern Ireland)"
    )
    
  ),
  
  #body---------------------------------------
  dashboardBody(
    
    fluidRow(
      
      #place by place comparison
      box(
        tabsetPanel(
          type = "tabs",
          tabPanel("Line plot", plotlyOutput("comp_plot")),
          tabPanel("Heatmap", plotlyOutput("heatmap"))
        ),
        width = 12
        )
      
    ),
    
    fluidRow(
      
      #settlement 1 summary
      box(
        "Climate classifications", br(),
        DTOutput("cc1"), br(),
        "Metric summary",
        DTOutput("sum1"),
        title = "Settlement 1 info",
        width = 4
      ),
      
      #settlement 2 summary
      box(
        "Climate classifications", br(),
        DTOutput("cc2"), br(),
        "Metric summary",
        DTOutput("sum2"),
        title = "Settlement 2 info",
        width = 4
      ),
      
      #map
      box(
        leafletOutput("map"),
        title = "Settlement map",
        width = 4
      )
      
    )
    
  )


)

################
# SERVER
##################

server <- function(input, output) {
  
  #set functions-----------------------
  
  #Comparison plot
  make_comp_plot <- reactive({
    
    #get the climate data and filter to relevant metric and settlements
    df <- metrics |>
      left_join(
        city_list |> transmute(id, city_name = ui_label),
        by = c("city_code" = "id")
      ) |>
      filter(
        ui_label == input$metric &
          city_name %in% c(input$location1, input$location2)
      ) |>
      mutate(
        month = factor(month, levels = month.abb),
        city_name = factor(city_name, levels = c(input$location1, input$location2))
        )
    
    #plot
    plot <- ggplotly(
      ggplot(
        df,
        aes(x = month, y = value, group = city_name, color = city_name)
      ) +
        #geoms
        geom_line(size = 1.5) +
        geom_point(
          aes(
            text = paste(
              "Settlement: ", city_name,
              "<br>Metric: ", metric,
              "<br>Month: ", month,
              "<br>Value: ", paste0(round(value, 2), units),
              "<br>Time period: ", ifelse(!is.na(time_period), time_period, "unknown")
            )
          ),
          size = 1.5
        ) +
        #scales
        scale_x_discrete(name = "Month") +
        scale_y_continuous(name = paste0("Value (", unique(df$units), ")")) +
        scale_color_manual(
          name = "Settlement", 
          values = c("#1ecbe1", "#e1341e"),
          labels = str_wrap(unique(df$ui_label), 25)
          )+
        #formatting
        coord_cartesian(
          ylim = c(
            ifelse(min(df$value) < 0, min(df$value), 0),
            NA
          )
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        ),
      tooltip = "text"
    )
    
  })
  
  #heatmap version
  
  make_heatmap <- reactive({
    
    #set colour options
    if(str_detect(input$metric, "°C|°F")) {
      
      colours <- c("low" = "#4575b4", "mid" = "#ffffbf", "high" = "#d73027")
      
    }
    
    if(str_detect(input$metric, "mm|inches|days")) {
      
      colours <- c("low" = "#f7fbff", "mid" = "#6baed6", "high" = "#08306b")
      
    }
    
    if(str_detect(input$metric, "hours")) {
      
      colours <- c("low" = "#fff5f0", "mid" = "#fb6a4a", "high" = "#67000d")
      
    }
    
    #limits for chosen metric
    value_lims <- metrics |>
      left_join(
        city_list |> transmute(id, city_name = ui_label),
        by = c("city_code" = "id")
      ) |>
      filter(ui_label == input$metric)
    
    #get the climate data and filter to relevant metric and settlements
    df <- metrics |>
      left_join(
        city_list |> transmute(id, city_name = ui_label),
        by = c("city_code" = "id")
      ) |>
      filter(
        ui_label == input$metric &
          city_name %in% c(input$location1, input$location2)
      ) |>
      mutate(
        month = factor(month, levels = month.abb),
        city_name = factor(city_name, levels = c(input$location2, input$location1))
        )
    
    #plot
    plot <- ggplotly(
      ggplot(
        df,
        aes(x = month, y = city_name, fill = value)
      ) +
        #geoms
        geom_tile(
          colour = "lightgray",
          aes(
            text = paste(
              "Settlement: ", city_name,
              "<br>Metric: ", metric,
              "<br>Month: ", month,
              "<br>Value: ", paste0(round(value, 2), units),
              "<br>Time period: ", ifelse(!is.na(time_period), time_period, "unknown")
              )
            )
          ) +
        #scales
        scale_x_discrete(name = "Month") +
        scale_y_discrete(name = "Settlement") +
        scale_fill_gradient2(
          name = paste0("Value (", unique(df$units), ")"), 
          limits = c(min(value_lims$value, na.rm = T), max(value_lims$value, na.rm = T)),
          low = colours["low"],
          mid = colours["mid"],
          high = colours["high"]
        ) +
        theme_minimal() +
        theme(
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        ),
      tooltip = "text"
    )
    
    
  })
  
  
  #climate class tbl 1
  make_cc1_tbl <- reactive({
    
    #city code
    cc1 <- city_list |> filter(ui_label == input$location1) |> pull(id)
    
    #climate classes
    cc_df1 <- climate_classes |> filter(id == cc1) |>
      transmute(
        classification_type = class_description.y,
        class_code,
        class_name = class_description.x
      ) |>
      datatable(
        rownames = F,
        colnames = c("Classification type", "Climate code", "Climate description"),
        options = list(dom = "t")
      )
    
  })
  
  #climate class tbl 2
  make_cc2_tbl <- reactive({
    
    #city code
    cc2 <- city_list |> filter(ui_label == input$location2) |> pull(id)
    
    #climate classes
    cc_df2 <- climate_classes |> filter(id == cc2) |>
      transmute(
        classification_type = class_description.y,
        class_code,
        class_name = class_description.x
      ) |>
      datatable(
        rownames = F,
        colnames = c("Classification type", "Climate code", "Climate description"),
        options = list(dom = "t")
      )
    
  })
  
  #metric summary 1
  make_sum1_tbl <- reactive({
    
    #get city codes
    cc1 <- city_list |> filter(ui_label == input$location1) |> pull(id)
    
    #summary dataframes
    sum_df1 <- ann_summaries |> 
      mutate(measure = paste0(metric, " (", units, ")")) |>
      filter(city_code == cc1 & measure == input$metric) |>
      transmute(
        measure,
        agg = str_to_sentence(stat),
        value = round(summary_value, 1)
      ) |>
      datatable(
        rownames = F,
        colnames = c("Metric (units)", "Aggregation method (per year)", "Aggregated value"),
        options = list(dom = "t")
      )
    
  })
  
  #metric summary 2
  make_sum2_tbl <- reactive({
    
    #get city codes
    cc2 <- city_list |> filter(ui_label == input$location2) |> pull(id)
    
    #summary dataframes
    sum_df2 <- ann_summaries |> 
      mutate(measure = paste0(metric, " (", units, ")")) |>
      filter(city_code == cc2 & measure == input$metric) |>
      transmute(
        measure,
        agg = str_to_sentence(stat),
        value = round(summary_value, 1)
      ) |>
      datatable(
        rownames = F,
        colnames = c("Metric (units)", "Aggregation method (per year)", "Aggregated value"),
        options = list(dom = "t")
      )
    
  })
  
  #functions to get inputs
  
  print_location1 <- reactive({
    
    paste0(input$location1, " info")
    
  })
  
  print_location2 <- reactive({
    
    paste0(input$location2, " info")
    
  })
  
  print_metric <- reactive({
    
    paste0(input$metric, " summary")
    
  })
  
  #map
  
  make_map <- reactive({
    
    #df
    df <- city_list |>
      filter(ui_label %in% c(input$location1, input$location2)) |>
      transmute(city_name, lat = latitude, long = longitude) 
    
    #map
    leaflet(data = df) |>
      setMaxBounds(
        min(df$long) - 5,
        min(df$lat) - 5,
        max(df$long) + 5,
        max(df$lat) + 5
      ) |>
      addTiles() |>
      addMarkers(~long, ~lat, label = ~city_name)
    
  })
  
  # run functions---------------------
  
  #comparison plot
  output$comp_plot <- renderPlotly(make_comp_plot())
  output$heatmap <- renderPlotly(make_heatmap())
  
  #summary tables
  output$cc1 <- renderDT(make_cc1_tbl())
  output$cc2 <- renderDT(make_cc2_tbl())
  output$sum1 <- renderDT(make_sum1_tbl())
  output$sum2 <- renderDT(make_sum2_tbl())
  
  #print inputs
  output$location1_o <- renderText(print_location1())
  output$location2_o <- renderText(print_location2())
  output$metric_o <- renderText(print_metric())
  
  #map
  output$map <- renderLeaflet(make_map())
  

}

# Run the application 
shinyApp(ui = ui, server = server)
