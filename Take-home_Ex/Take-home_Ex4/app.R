##### Set up #####
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, sf, terra, viridis)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% st_transform(crs = 3414)

##### Header and sidebar #####
header <- dashboardHeader(title = "Singapore Weather Analytics (2021-2023)")

sidebar <- dashboardSidebar(
  width = 100,
  tags$head(tags$style(HTML(".sidebar-menu > li > a {white-space: normal; line-height: 1.2;}"))
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage"),
    menuItem("EDA & CDA", tabName = "EDACDA"),
    menuItem("Univariate Forecasting", tabName = "Univariate"),
    menuItem("Spatial Interpolation", tabName = "Geospatial")
    )
  )

##### Geospatial UI  #####
GeospatialUI <- fluidPage(
  
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_selected_var", "Choose variable", variables),
        radioButtons("GS_time_resolution", "Time resolution", c("Day", "Month", "Year")),
        uiOutput("GS_dynamic_time_resolution"), 
        actionButton("GS_updatetmap", "Show map")
      
    ),
    box(title = "Map",  width = 10, status = "primary", solidHeader = TRUE,
        tmapOutput("GS_tmap")
        )
  ),
  fluidRow(
    box(title = "IDW Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_show_IDW", "Show IDW?", c("Yes", "No"), selected = "Yes", inline = TRUE),
        uiOutput("GS_dynamic_IDW"),
        actionButton("GS_updateIDW", "Show Result")
        
    ),
    box(title = "IDW Map",  width = 10, status = "primary", solidHeader = TRUE,
        plotOutput("GS_IDW_map")
    )
    
  ),
  fluidRow(
    box(title = "Kriging Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_show_OK", "Show Kriging?", c("Yes", "No"), selected = "Yes", inline = TRUE),
        uiOutput("GS_dynamic_OK"),
        actionButton("GS_updateOK", "Show Result")
        
    ),
    box(title = "Ordinary Kriging Map",  width = 10, status = "primary", solidHeader = TRUE,
        plotOutput("GS_OK_map")
    )
    
  )
)


##### Body content #####
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "LandingPage",
            fluidRow(
              box()
            )
    ),
    
    # Second tab content
    tabItem(tabName = "EDACDA",
            h2("EDACDA content")
    ),    
    # Second tab content
    tabItem(tabName = "Univariate",
            h2("Univariate content")
    ),    
    # Second tab content
    tabItem(tabName = "Geospatial",
            h2("Spatial Interpolation"),
            GeospatialUI
    )
  )
)

##### UI #####
ui <- dashboardPage(
  header,
  sidebar,
  body
)

##### server #####
server <- function(input, output) {
  ##### Spatial Interpolation ##### 
  
  # tmap
  # Dynamic UI for selected time_resolution
  output$GS_dynamic_time_resolution <- renderUI({
    if (input$GS_time_resolution == "Day") {
        airDatepickerInput("GS_selected_date", label = "Select date", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM-dd")
    } else if (input$GS_time_resolution == "Month") {
        airMonthpickerInput("GS_selected_month", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$GS_time_resolution == "Year") {
      airYearpickerInput("GS_selected_year", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  output$GS_tmap <- renderTmap({
    req(input$GS_updatetmap) # Ensure action button has been pressed. This needs to be re-worked. 
    selected_var <- input$GS_selected_var 
    
    # Prepare data based on user input
    if (input$GS_time_resolution == "Day") {
      selected_date <- as.Date(input$GS_selected_date)
      variable_data <- weather_data %>%
        filter(Date == selected_date)
      # Group and summarise data, selecting for the correctly variable
      variable_data <- variable_data %>%
        group_by(Station, Date, LAT, LONG) %>%
        summarise(ValueToPlot = if(grepl("Rainfall", selected_var)) {
          sum(.data[[selected_var]], na.rm = TRUE) # Sum or average based on the variable
        } else if (grepl("Temperature", selected_var)) {
          mean(.data[[selected_var]], na.rm = TRUE)
        }, .groups = 'drop')
    } else if (input$GS_time_resolution == "Month") {
      selected_month <- format(as.Date(input$GS_selected_month), "%m")
      selected_year <- format(as.Date(input$GS_selected_month), "%Y")
      variable_data <- weather_data %>%
        filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month))
      # Group and summarise data, selecting for the correctly variable
      variable_data <- variable_data %>%
        group_by(Station, Year, Month, LAT, LONG) %>%
        summarise(ValueToPlot = if(grepl("Rainfall", selected_var)) {
          sum(.data[[selected_var]], na.rm = TRUE) # Sum or average based on the variable
        } else if (grepl("Temperature", selected_var)) {
          mean(.data[[selected_var]], na.rm = TRUE)
        }, .groups = 'drop')
    } else if (input$GS_time_resolution == "Year") {
      selected_year <- as.character(format(input$GS_selected_year, "%Y"))
      print(selected_year)
      variable_data <- weather_data %>%
        filter(format(Date, "%Y") == selected_year)
      # Group and summarise data, selecting for the correctly variable
      variable_data <- variable_data %>%
        group_by(Station, Year, LAT, LONG) %>%
        summarise(ValueToPlot = if(grepl("Rainfall", selected_var)) {
          sum(.data[[selected_var]], na.rm = TRUE) # Sum or average based on the variable
        } else if (grepl("Temperature", selected_var)) {
          mean(.data[[selected_var]], na.rm = TRUE)
        }, .groups = 'drop')
    }

    # Replace 'mpsz2019' with your spatial boundary data if different
    
    # Check if there's data to plot
    if ("ValueToPlot" %in% names(variable_data) && nrow(variable_data) > 0) {
      variable_data_sf <- st_as_sf(variable_data, coords = c("LONG", "LAT"), crs = 4326) %>% st_transform(crs = 3414)
      
      tmap_options(check.and.fix = TRUE) # Use this code to fix topo error without changing the data. 
      tmap_mode("view") # Make map interactive
      tm <- tm_shape(mpsz2019) + tm_borders() + tm_shape(variable_data_sf) + tm_dots(col = 'ValueToPlot') # Use color to differentiate
      tm
    } else {
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 0.5, "No data available for the selected criteria", cex = 1.2)
    }
  })
  
  # IDW
  output$GS_dynamic_IDW <- renderUI({
    if (input$GS_show_IDW == "Yes") {
      list(
        sliderInput("GS_IDW_res", "Resolution", 
                    min = 30, max = 80, value = 50),
        sliderInput("GS_nmax" , "nmax",
                    min = 1, max = 10, value = 3)
        
      )
    }
    
  })
  
  # OK
  output$GS_dynamic_OK <- renderUI({
    if (input$GS_show_OK == "Yes") {
      model_choices <- c("Nug", "Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl")
      # min_nugget <- min(variable_data$ValueToPlot, na.rm = TRUE)
      # max_nugget <- max(variable_data$ValueToPlot, na.rm = TRUE)
      # mid_nugget  <- mean(variable_data$ValueToPlot, na.rm = TRUE)
      list(
        sliderInput("GS_OK_res", "Resolution", min = 30, max = 80, value = 50),
        selectInput("GS_model" , "model", model_choices),
        sliderInput("GS_OK_psill", "psill", min = 0.5, max = 10, value = 0.5, step = 0.5),
        sliderInput("GS_OK_range", "range", min = 500, max = 10000, value = 8000, step = 500),
        sliderInput("GS_OK_nugget", "nugget", min = 0.1, max = 10, value = 0.1, step = 0.1)
      )
    }
    
  })
  
}

# Run App
##########
# Run the application 
shinyApp(ui, server)