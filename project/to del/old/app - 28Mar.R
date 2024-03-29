# Section 1: Set up ----
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggthemes, plotly, sf, terra, viridis, ggHoriPlot, ggstatsplot, rstantools, ISOweek, DT, nortest, ggridges)

# Section 1.1: Variables and Functions ----
## Import data 
weather_data <- read_rds("data/weather_imputed_11stations.rds") 
variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

## Used for Time Series module
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)
Station <- weather_tsbl %>% distinct(Station)
ForecastTS_model_choices <- list("STL Naive" = "STL_Naive","STL ARIMA" = "STL_Arima","STL ETS" = "STL_ETS","AUTO ARIMA" = "ARIMA","AUTO Prophet" = "prophet","AUTO ETS" = "ETS")

## Used for Spatial Interpolation module
GS_model_choices <- c("Nug", "Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl")
mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% st_transform(crs = 3414)

## Function to calculate ValueToPlot based on  selected variable.
## Used across Time Series and Spatial Interpolation module 
calculateValueToPlot <- function(data, var) {
  if (grepl("Rainfall", var)) {
    sum(data[[var]], na.rm = TRUE)
  } else if (grepl("Temperature", var)) {
    round(mean(data[[var]], na.rm = TRUE), 2)
  }
}

## Function to prepare variable data and plot elements. Used for Spatial Interpolation module
GS_prepareVariableData <- function(selected_var, time_resolution, selected_time, weather_data) {
  variable_data <- NULL
  
  if (time_resolution == "Day") {
    selected_date <- as.Date(selected_time)
    variable_data <- weather_data %>%
      filter(Date == selected_date)
  } else if (time_resolution == "Month") {
    selected_month <- format(as.Date(selected_time), "%m")
    selected_year <- format(as.Date(selected_time), "%Y")
    variable_data <- weather_data %>%
      filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month))
  } else if (time_resolution == "Year") {
    selected_year <- as.character(format(selected_time, "%Y"))
    variable_data <- weather_data %>%
      filter(format(Date, "%Y") == selected_year)
  }
  
  # Group and summarise data based on the selected variable
  if (!is.null(variable_data)) {
    variable_data <- variable_data %>%
      group_by(Station, LAT, LONG) %>%
      summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop')
  } else {variable_data <- NULL}
  
  # Add legend_title to variable
  legend_title <- if(grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if(grepl("Temperature", selected_var)) {"Temperature (°C)"}
  
  # Update variable_data before converting to sf
  if (!is.null(variable_data)) {
    variable_data[[legend_title]] <- variable_data$ValueToPlot
    variable_data_sf <- st_as_sf(variable_data, coords = c("LONG", "LAT"), crs = 4326) %>%
      st_transform(crs = 3414)
  } else {variable_data_sf <- NULL}
  
  
  time_title <- switch(time_resolution, "Day" = "Daily", "Month" = "Monthly", "Year" = "Yearly", NA)
  
  var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} 
  else if (grepl("Temperature", selected_var)) {if(time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
  
  # Date title with a check
  # date_input <- switch(time_resolution, "Day" = input$GS_selected_date, "Month" = input$GS_selected_month, "Year" = input$GS_selected_year,NA) 
  
  date_title <- format(as.Date(selected_time), switch(time_resolution,"Day" = "%d %B %Y","Month" = "%B %Y","Year" = "%Y",NA))
  
  main_title <- paste(time_title, var_title, "for\n", date_title, "across stations in Singapore")
  
  return(list(variable_data = variable_data, variable_data_sf = variable_data_sf, legend_title = legend_title, main_title = main_title))
}

# Function to get raster layer
GS_rasterlayer <- function(res){
  # Specify resolution
  bbox <- as.list(st_bbox(mpsz2019))
  nrows =  (bbox$ymax - bbox$ymin)/res
  ncols = (bbox$xmax - bbox$xmin)/res
  # Create raster layer, 'grid'
  grid <- rast(mpsz2019, nrows = nrows, ncols = ncols)
  # Generate coordinates for each cell of the raster
  xy <- xyFromCell(grid,1:ncell(grid))
  # Converting coordinates of raster into a spatial (sf) object
  coop <- st_as_sf(as.data.frame(xy), coords = c("x","y"), crs = st_crs(mpsz2019))
  # Filter to only only includes points within mpsz2019
  coop <- st_filter(coop, mpsz2019) 
  
  return(list(grid = grid, coop = coop))
}

# Section 2: Header and sidebar ----
header <- dashboardHeader(title = "Singapore Weather Analytics (2021-2023)")

sidebar <- dashboardSidebar(
  width = 200,
  tags$head(tags$style(HTML(".sidebar-menu > li > a {white-space: normal; line-height: 1.2;}"))
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage"),
    menuItem("EDA & CDA", tabName = "EDACDA"),
    menuItem("Univariate Forecasting", tabName = "Univariate",
    menuSubItem("Exploratory Time Series", tabName = "ExploreTS"),
    menuSubItem("Time Series Decomposition", tabName = "DecomposeTS"),
    menuSubItem("Forecasting", tabName = "ForecastTS")),
    menuItem("Spatial Interpolation", tabName = "Geospatial")
    )
  )

# Section 3 CDA compare across stations UI  ----
CDAUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("CDA_selected_var", "Choose variable", variables, selected = NULL, multiple = FALSE),
        selectInput("CDA_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"),multiple = FALSE),
        selectInput("CDA_selectedConflevel", "Confidence Level", choices = c("90%"="0.90","95%"="0.95", "99%"="0.99"),multiple = FALSE),
        selectInput("CDA_plotType", "Plot Type",choices = c("Boxviolin" = "boxviolin", "Box" = "box", "Violin" = "violin"),
                    selected = "boxviolin",multiple = FALSE),
        textInput("CDA_plot_title","Plot Title", placeholder = "Enter plot title"),
    ),
    tabBox(
      title = "", width = 10,
      # The id lets us use input$EDACDA_tab on the server to find the current tab
      id = "EDACDA_tab", height = "250px",
      tabPanel("Check Normality Across stations",
               fluidRow(
                 column(3,
                        radioButtons("CDA_checknormality_AS_time_resolution", label = "Select time resolution", c("Month", "Year")),
                        uiOutput("CDA_checknormality_AS_dynamic_time_resolution"),
                        checkboxGroupInput("CDA_checknormality_AS_stationSelection", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1]),
                        actionButton("CDA_checknormality_AS_updateplot", "Update plot")), 
                 column(9, plotOutput("CDA_checknormality_AS_Plot")),
                 column(3, tableOutput("CDA_ADtest_AS_results"))
               )
      ),
      tabPanel("Check Normality Across Time",
               fluidRow(
                 column(3,
                        radioButtons("CDA_checknormality_AT_time_resolution", label = "Select time resolution", c("Month", "Year")),
                        uiOutput("CDA_checknormality_AT_dynamic_time_resolution"),
                        selectInput("CDA_checknormality_AT_stationSelection", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1], multiple = FALSE),
                        actionButton("CDA_checknormality_AT_updateplot", "Update plot")), 
                 column(9, plotOutput("CDA_checknormality_AT_Plot")),
                 column(3, tableOutput("CDA_ADtest_AT_results"))
               )
      ),
      tabPanel("Compare Across Stations",
               fluidRow(
                 column(3,
                        radioButtons("CDA_acrossstations_time_resolution", label = "Select time resolution", c("Month", "Year")),
                        uiOutput("CDA_acrossstations_dynamic_time_resolution"),
                        checkboxGroupInput("CDA_acrossstations_stationSelection", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1]),
                        actionButton("CDA_acrossstations_updateplot", "Update plot")), 
                 column(9, plotlyOutput("CDA_acrossstations_Plot")) 
               )
      ),
      tabPanel("Compare Across Years",
               fluidRow(
                 column(3, 
                        checkboxGroupInput("CDA_acrossyears_selected_years", label = "Select years",choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1]),
                        selectInput("CDA_acrossyears_stationSelection", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1], multiple = FALSE),
                        actionButton("CDA_acrossyears_updateplot", "Update plot")),
                 column(9,plotlyOutput("CDA_acrossyears_Plot"))
               )
      ),
      tabPanel("Compare Across Months",
               fluidRow(
                 column(3, 
                        checkboxGroupInput("CDA_acrossmonths_selected_month", label = "Select month", choices = unique(weather_data$Month_Name)),
                        selectInput("CDA_acrossmonths_stationSelection", "Select Station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1], multiple = FALSE),
                        actionButton("CDA_acrossmonths_updateplot", "Update plot")),
                 column(9,plotlyOutput("CDA_acrossmonths_Plot"))
               )
      )
      
    )
  )
)

# Section 4.1: ExploreTS UI ----
ExploreTSUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("ExploreTS_selected_var", "Choose variable", variables, selected = NULL, multiple = FALSE),
        uiOutput("ExploreTS_dynamic_time_resolution"),
        checkboxGroupInput("ExploreTS_selectstation", "Select Station", choices = unique(weather_tsbl$Station),  selected = unique(weather_tsbl$Station)[1]),
        dateInput("ExploreTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-31"),
        dateInput("ExploreTS_endDate", "End Date", value = "2023-12-31", min = "2021-01-02", max = "2023-12-31")
        ),
    tabBox(
      title = "", width = 10,
      # The id lets us use input$ExploreTS_tab on the server to find the current tab
      id = "ExploreTS_tab", height = "250px",
      tabPanel("Line graph",
               fluidRow(plotlyOutput("ExploreTS_timeSeriesPlot") 
               ),
               fluidRow(box(title = "datatable"))
      ),
      tabPanel("Horizon plot",
               fluidRow(
                 column(3, box(title = "parameters compare across year? compare across stations")),
                 column(9,box(title = "plot"))
               )
      )
    )
  )
)

# Section 4.2: DecomposeTS UI ----
DecompTSUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("DecompTS_selected_var", "Choose variable", variables),
        uiOutput("DecompTS_dynamic_time_resolution"),
        selectInput("DecompTS_selectstation", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
        dateInput("DecompTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-30"),
        HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
         <strong>End Date</strong><br>
         <div style='border: 1px solid #ccc; padding: 5px 10px; margin-top: 5px; display: inline-block; width: 200px;'>
           2023-12-31
         </div>
       </div>")
        ),
    tabBox(
      title = "", width = 10,
      # The id lets us use input$DecomposeTS_tab on the server to find the current tab
      id = "DecompTS_tab", height = "250px",
      tabPanel("ACF & PACF",
               fluidRow(
                 column(3, title = "parameters",     
                        sliderInput("DecompTS_lags", "Number of Lags", min = 1, max = 365, value = 20)), 
                 column(9, plotlyOutput("DecompTS_ACFPlot"),
                        plotlyOutput("DecompTS_PACFPlot"))
               )
      ),
      tabPanel("STL Decomposition",
               fluidRow(
                 column(3, title = "parameters",
                        radioButtons("DecompTS_chooseautoSTL", label = "Use Auto STL?" ,c("Yes", "No")),
                        uiOutput("DecompTS_autoSTL")),
                 column(9,plotlyOutput("DecompTS_STLPlot"))
               )
      )
    )
  )
)

# Section 4.3: ForecastTS UI ----
ForecastTSUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("ForecastTS_selected_var", "Choose variable", variables),
        uiOutput("ForecastTS_dynamic_time_resolution"),
        selectInput("ForecastTS_selectstation", "Select Station", choices = unique(weather_tsbl$Station), selected = unique(weather_tsbl$Station)[1]),
        dateInput("ForecastTS_startDate", "Start Date", value = "2021-01-01", min = "2021-01-01", max = "2023-12-30"),
        HTML("<div style='margin-top: 15px; margin-bottom: 15px;'>
         <strong>End Date</strong><br>
         <div style='border: 1px solid #ccc; padding: 5px 10px; margin-top: 5px; display: inline-block; width: 200px;'>
           2023-12-31
         </div>
       </div>"),
        checkboxGroupInput("ForecastTS_selectmodel", "Select Forecasting Models", choices = ForecastTS_model_choices),
        uiOutput("ForecastTS_dynamic_model_parameters"),
        sliderInput("ForecastTS_train_test_split", "Select Train-Test Split", min = 0, max = 1, value = 0.8),
        actionButton("ForecastTS_build_model", "Build Model")
        ),
    tabBox(
      title = "", width = 10,
      # The id lets us use input$ForecastTS_tab on the server to find the current tab
      id = "ForecastTS_tab", height = "250px",
      tabPanel("Model Calibration",
               fluidRow(box(title = "plot")
               )
      ),
      tabPanel("Forecast Result",
               fluidRow(
                 column(3,uiOutput("ForecastTS_dynamic_forecast_period"),
                        actionButton("ForecastTS_updateplot", "Forecast")),
                 column(9,box(title = "plot"))
               )
      )
    )
  )
)



# Section 5: Geospatial UI ----

GeospatialUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_selected_var", "Choose variable", variables),
        radioButtons("GS_time_resolution", "Time resolution", c("Day", "Month", "Year")),
        uiOutput("GS_dynamic_time_resolution")
    ),
    tabBox(
      title = "Plots", width = 10,
      # The id lets us use input$Geospatial_tab on the server to find the current tab
      id = "Geospatial_tab", height = "250px",
      tabPanel("tmap",
               fluidRow(
                 column(3, actionButton("GS_updatetmap", "Update map")), # Adjust the width as needed
                 column(9, tmapOutput("GS_tmap")) # Adjust the width so that the total does not exceed 12
               )
               ),
      tabPanel("Inverse Distance Weighted Interpolation Method",
               fluidRow(
               column(3, 
                        sliderInput("GS_IDW_res", "Resolution", min = 100, max = 200, value = 100, step = 50),
                      sliderInput("GS_IDW_nmax" , "nmax", min = 1, max = 10, value = 5),
                      actionButton("GS_updateIDW", "Show Result")
                      ),
               column(9,plotOutput("GS_IDW_map"))
               )
               ),
      tabPanel("Ordinary Kriging Method",
               fluidRow(
                 column(2, 
                        sliderInput("GS_OK_res", "Resolution", min = 100, max = 200, value = 100, step = 50),
                        selectInput("GS_OK_model" , "model", GS_model_choices, selected = "Gau"),
                        sliderInput("GS_OK_psill", "psill", min = 0.5, max = 10, value = 0.5, step = 0.5),
                        sliderInput("GS_OK_range", "range", min = 2000, max = 10000, value = 5000, step = 500),
                        sliderInput("GS_OK_nugget", "nugget", min = 0.1, max = 10, value = 0.1, step = 0.1),
                        actionButton("GS_updateOK", "Show Result")
                 ),
                 column(10,
                        fluidRow(
                          column(5,plotOutput("GS_OK_variogram")),
                          column(5,plotOutput("GS_OK_fitted_variogram"))
                        ),
                        fluidRow(
                          column(5,plotOutput("GS_OK_map")),
                          column(5,plotOutput("GS_OK_prediction_variance"))
                        ))
               )
      )
    )
  )
)

# Section 6: Dashboard Body and UI ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "LandingPage",
            fluidRow(
              box()
            )
    ),
    
    tabItem(tabName = "EDACDA",
            h2("EDACDA content"),
            CDAUI
    ),    
    tabItem(tabName = "Univariate"
    ),
    tabItem(tabName = "ExploreTS", 
            h2("Exploring timeseries for a single station"),
            ExploreTSUI
            ),
    tabItem(tabName = "DecomposeTS", 
            h2("Time Series Decomposition and ACF PACF"),
            DecompTSUI
    ),
    tabItem(tabName = "ForecastTS", 
            h2("Forecasting with different models"),
            ForecastTSUI
    ),
    tabItem(tabName = "Geospatial",
            h2("Spatial Interpolation"),
            GeospatialUI)
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)


# Section 7: Server code ----
server <- function(input, output) {
  
  # CDA:  CDA_acrossstations_Plot----
  
  ## 1. Dynamic UI
  output$CDA_acrossstations_dynamic_time_resolution <- renderUI({
    if(input$CDA_acrossstations_time_resolution == "Month") {
      airMonthpickerInput("CDA_acrossstations_selected_month", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$CDA_acrossstations_time_resolution == "Year") {
      airYearpickerInput("CDA_acrossstations_selected_year", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  ## 2. Prepare and store reactive data
  CDA_acrossstations_reactiveData <- reactiveValues()
  observeEvent(input$CDA_acrossstations_updateplot, {
    
    selected_var <- input$CDA_selected_var
    
    # If Time resolution is Month 
    if (input$CDA_acrossstations_time_resolution == "Month") {
      
      selected_month <- format(as.Date(input$CDA_acrossstations_selected_month), "%m")
      selected_year <- format(as.Date(input$CDA_acrossstations_selected_month), "%Y")
      
      # Select data
      variable_data <- weather_data %>%
        filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month),
               Station %in% input$CDA_acrossstations_stationSelection)
      
      # Set plot title
      title <- input$CDA_plot_title
      # If Time resolution is Year
    } else if (input$CDA_acrossstations_time_resolution == "Year") {
      
      selected_year <- as.character(format(input$CDA_acrossstations_selected_year, "%Y"))
      
      # Select data
      variable_data <- weather_data %>%
        filter(format(Date, "%Y") == selected_year,
               Station %in% input$CDA_acrossstations_stationSelection)
      # Set plot title
      title <- input$CDA_plot_title
    } 
    
    # Update reactive variable
    CDA_acrossstations_reactiveData$variable_data <- variable_data
    CDA_acrossstations_reactiveData$selected_var <- selected_var
    CDA_acrossstations_reactiveData$title <- title
  })
  
  
  
  ## 3. Output plotly plot
  output$CDA_acrossstations_Plot <- renderPlotly({
    
    req(CDA_acrossstations_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_acrossstations_reactiveData$selected_var)
    
    
    ggbetweenstats(data = CDA_acrossstations_reactiveData$variable_data,
                   x = "Station",
                   y = !!var_symbol, 
                   type = input$CDA_selectedStatApproach,
                   mean.ci = TRUE,
                   conf.level = input$CDA_selectedConflevel,
                   violin.args = if(input$CDA_plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(trim=TRUE,alpha = 0.2)},
                   boxplot.args = if(input$CDA_plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(alpha = 0.2)},
                   pairwise.comparisons = TRUE, 
                   pairwise.annotation = TRUE,
                   pairwise.display = "none", 
                   sig.level = NA,
                   p.adjust.method = "fdr",
                   messages = FALSE,
                   title = CDA_acrossstations_reactiveData$title)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # CDA:  CDA_acrossyears_Plot----
  
  ## 1. Prepare and store  data
  CDA_acrossyears_reactiveData <- reactiveValues()
  observeEvent(input$CDA_acrossyears_updateplot, {
    
    selected_var <- input$CDA_selected_var
    
    # Select data
    variable_data <- weather_data %>%
      filter(Year %in% input$CDA_acrossyears_selected_years,
             Station %in% input$CDA_acrossyears_stationSelection)
    
    # Set plot title
    title <- input$CDA_plot_title
    
    # Update reactive variable
    CDA_acrossyears_reactiveData$variable_data <- variable_data
    CDA_acrossyears_reactiveData$selected_var <- selected_var
    CDA_acrossyears_reactiveData$title <- title
  })
  
  
  
  ## 3. Output plotly plot
  output$CDA_acrossyears_Plot <- renderPlotly({
    
    req(CDA_acrossyears_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_acrossyears_reactiveData$selected_var)
    
    ggbetweenstats(data = CDA_acrossyears_reactiveData$variable_data,
                   x = "Year",
                   y = !!var_symbol, 
                   type = input$CDA_selectedStatApproach,
                   plot.type = input$CDA_plotType,
                   mean.ci = TRUE,
                   conf.level = input$CDA_selectedConflevel,
                   violin.args = if(input$CDA_plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(trim=TRUE,alpha = 0.2)},
                   boxplot.args = if(input$CDA_plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(alpha = 0.2)},
                   pairwise.comparisons = TRUE, 
                   pairwise.annotation = "p.value",
                   pairwise.display = "none", 
                   sig.level = NA,
                   p.adjust.method = "fdr",
                   messages = FALSE,
                   title = CDA_acrossyears_reactiveData$title)
    
  })
  
  # CDA:  CDA_acrossmonths_Plot----
  
  ## 1. Prepare and store  data
  CDA_acrossmonths_reactiveData <- reactiveValues()
  observeEvent(input$CDA_acrossmonths_updateplot, {
    
    selected_var <- input$CDA_selected_var
    
    
    # Select data
    variable_data <- weather_data %>%
      filter(Month_Name %in% input$CDA_acrossmonths_selected_month,
             Station %in% input$CDA_acrossmonths_stationSelection)
    
    # Set plot title
    title <- input$CDA_plot_title
    
    # Update reactive variable
    CDA_acrossmonths_reactiveData$variable_data <- variable_data
    CDA_acrossmonths_reactiveData$selected_var <- selected_var
    CDA_acrossmonths_reactiveData$title <- title
  })
  
  
  
  ## 3. Output plotly plot
  output$CDA_acrossmonths_Plot <- renderPlotly({
    
    req(CDA_acrossmonths_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_acrossmonths_reactiveData$selected_var)
    
    ggbetweenstats(data = CDA_acrossmonths_reactiveData$variable_data,
                   x = "Month_Name",
                   y = !!var_symbol, 
                   type = input$CDA_selectedStatApproach,
                   mean.ci = TRUE,
                   conf.level = input$CDA_selectedConflevel,
                   violin.args = if(input$CDA_plotType == "box"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(trim=TRUE,alpha = 0.2)},
                   boxplot.args = if(input$CDA_plotType == "violin"){list(width = 0, linewidth = 0,alpha = 0)} 
                   else {list(alpha = 0.2)},
                   pairwise.comparisons = TRUE, 
                   pairwise.annotation = "p.value",
                   pairwise.display = "all", 
                   sig.level = NA,
                   p.adjust.method = "fdr",
                   messages = TRUE,
                   xlab= "Month",
                   title = CDA_acrossmonths_reactiveData$title,
                   results.subtitle = TRUE)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  # CDA:  CDA_checknormality_AS_Plot----
  
  ## 1. Dynamic UI
  output$CDA_checknormality_AS_dynamic_time_resolution <- renderUI({
    if(input$CDA_checknormality_AS_time_resolution == "Month") {
      airMonthpickerInput("CDA_checknormality_AS_selected_month", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$CDA_checknormality_AS_time_resolution == "Year") {
      airYearpickerInput("CDA_checknormality_AS_selected_year", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  ## 2. Prepare and store reactive data
  CDA_checknormality_AS_reactiveData <- reactiveValues()
  observeEvent(input$CDA_checknormality_AS_updateplot, {
    
    selected_var <- input$CDA_selected_var
    
    # If Time resolution is Month 
    if (input$CDA_checknormality_AS_time_resolution == "Month") {
      
      selected_month <- format(as.Date(input$CDA_checknormality_AS_selected_month), "%m")
      selected_year <- format(as.Date(input$CDA_checknormality_AS_selected_month), "%Y")
      
      # Select data
      variable_data <- weather_data %>%
        filter(format(Date, "%Y-%m") == paste0(selected_year, "-", selected_month),
               Station %in% input$CDA_checknormality_AS_stationSelection)
      
      # Set plot title
      title <- input$CDA_plot_title
      # If Time resolution is Year
    } else if (input$CDA_checknormality_AS_time_resolution == "Year") {
      
      selected_year <- as.character(format(input$CDA_checknormality_AS_selected_year, "%Y"))
      
      # Select data
      variable_data <- weather_data %>%
        filter(format(Date, "%Y") == selected_year,
               Station %in% input$CDA_checknormality_AS_stationSelection)
      # Set plot title
      title <- input$CDA_plot_title
    } 
    
    # Update reactive variable
    CDA_checknormality_AS_reactiveData$variable_data <- variable_data
    CDA_checknormality_AS_reactiveData$selected_var <- selected_var
    CDA_checknormality_AS_reactiveData$title <- title
  })
  
  
  
  ## 3. Output plot
  ## Normality plot
  output$CDA_checknormality_AS_Plot <- renderPlot({
    
    req(CDA_checknormality_AS_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_checknormality_AS_reactiveData$selected_var)
    
    ggplot(data = CDA_checknormality_AS_reactiveData$variable_data,
           aes(x = !!var_symbol, y = Station)) +
      geom_density_ridges(fill = "lightblue",alpha = 0.9) +
      labs(title = CDA_checknormality_AS_reactiveData$title) +
      theme_ridges()+
      theme(legend.position = "none") })
  
  ## Anderson-Darling test result
  output$CDA_ADtest_AS_results <- renderTable({
    
    req(CDA_checknormality_AS_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_checknormality_AS_reactiveData$selected_var)
    
    ad_results <- CDA_checknormality_AS_reactiveData$variable_data %>%
      group_by(Station) %>%
      summarise("Statistic" = ad.test(!!var_symbol)$statistic,
                "p Value" = ad.test(!!var_symbol)$p.value
      ) %>%
      ungroup() 
    # Return the results to be displayed as a table
    ad_results
  })
  
  # CDA:  CDA_checknormality_AT_Plot----
  
  ## 1. Dynamic UI
  output$CDA_checknormality_AT_dynamic_time_resolution <- renderUI({
    if(input$CDA_checknormality_AT_time_resolution == "Month") {
      checkboxGroupInput("CDA_checknormality_AT_selected_month", label = "Select month", choices = unique(weather_data$Month_Name))
    } else if (input$CDA_checknormality_AT_time_resolution == "Year") {
      checkboxGroupInput("CDA_checknormality_AT_selected_years", label = "Select years",choices = unique(format(weather_data$Date, "%Y")),selected = unique(format(weather_data$Date, "%Y"))[1])
    }
  })
  
  ## 2. Prepare and store reactive data
  CDA_checknormality_AT_reactiveData <- reactiveValues()
  observeEvent(input$CDA_checknormality_AT_updateplot, {
    
    selected_var <- input$CDA_selected_var
    
    # If Time resolution is Month 
    if (input$CDA_checknormality_AT_time_resolution == "Month") {
      
      # Select data
      variable_data <- weather_data %>%
        filter(Month_Name %in% input$CDA_checknormality_AT_selected_month,
               Station %in% input$CDA_checknormality_AT_stationSelection)
      
      # Set plot title
      title <- input$CDA_plot_title
      # If Time resolution is Year
    } else if (input$CDA_checknormality_AT_time_resolution == "Year") {
      
      # Select data
      variable_data <- weather_data %>%
        filter(Year %in% input$CDA_checknormality_AT_selected_years,
               Station %in% input$CDA_checknormality_AT_stationSelection)
      # Set plot title
      title <- input$CDA_plot_title
    } 
    
    # Update reactive variable
    CDA_checknormality_AT_reactiveData$variable_data <- variable_data
    CDA_checknormality_AT_reactiveData$selected_var <- selected_var
    CDA_checknormality_AT_reactiveData$title <- title
  })
  
  
  
  ## 3. Output plot
  ## Normality plot
  output$CDA_checknormality_AT_Plot <- renderPlot({
    
    req(CDA_checknormality_AT_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_checknormality_AT_reactiveData$selected_var)
    
    ggplot(data = CDA_checknormality_AT_reactiveData$variable_data,
           aes(x = !!var_symbol,
               y = if(input$CDA_checknormality_AT_time_resolution == "Month"){Month_Name}else{Year})) +
      geom_density_ridges(fill = "lightblue",alpha = 0.9) +
      labs(title = CDA_checknormality_AT_reactiveData$title) +
      theme_ridges()+
      theme(legend.position = "none") })
  
  ## Anderson-Darling test result
  output$CDA_ADtest_AT_results <- renderTable({
    
    req(CDA_checknormality_AT_reactiveData$variable_data)
    
    var_symbol <- rlang::sym(CDA_checknormality_AT_reactiveData$selected_var)
    
    ad_results <- CDA_checknormality_AT_reactiveData$variable_data %>%
      group_by(if(input$CDA_checknormality_AT_time_resolution == "Month"){Month_Name}else{Year}) %>%
      summarise("Statistic" = ad.test(!!var_symbol)$statistic,
                "p Value" = ad.test(!!var_symbol)$p.value
      ) %>%
      ungroup() 
    # Return the results to be displayed as a table
    ad_results
  })
  
  # Time Series: Exploratory Plots ----
  
  ## 1. Dynamic UI for ExploreTS
  output$ExploreTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Week", "Month"))
    } else if (grepl("Temperature", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Day" ,"Week", "Month"))
    }
  })
  
  ## 2. Prepare data and plot line graph
  output$ExploreTS_timeSeriesPlot <- renderPlotly({
    
    # Prepare Data
    selected_var <- input$ExploreTS_selected_var
    
    char_startDate <- as.character(input$ExploreTS_startDate)
    char_endDate <- as.character(input$ExploreTS_endDate)
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station %in% input$ExploreTS_selectstation)
    
    if (input$ExploreTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$ExploreTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
      mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    } else if (input$ExploreTS_time_resolution == "Month") {
      variable_data <- variable_data %>%
        as_tibble() %>%
        group_by(Station, Year, Month) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = as.Date(paste(Year, Month, "01", sep = "-")))
    }
    
    # Update plot annotations
    yaxis_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var )) {"Temperature (°C)"}
    time_title <- switch(input$ExploreTS_time_resolution,"Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$ExploreTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    title <- paste(time_title, var_title, "\n", char_startDate, "to", char_endDate)
    
    # Create plotly plot
    plot_ly(variable_data, x = ~Date, y = ~ValueToPlot, 
            type = 'scatter', mode = 'lines', 
            color = ~Station, hoverinfo = 'text',
            text = ~paste("<b>Station:</b>", Station,
                          "<br><b>Date:</b>", Date,
                          "<br><b>", selected_var, ":</b>", ValueToPlot)) %>%
      layout(title = title,
             xaxis = list(title = "", 
                          range = c(char_startDate, char_endDate), 
                          rangeslider = list(type = "date", range = c(char_startDate, char_endDate))),
             yaxis = list(title = yaxis_title))
    
  })
  
  ## 3. Plot horizon plot 
  
  
  
  # Time Series: Decomposition ----
  
  ## 1. Dynamic UI for DecompTS_dynamic_time_resolution
  output$DecompTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$DecompTS_selected_var)) {
      radioButtons("DecompTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
   
  ## 1.1. Dynamic UI for DecompTS_autoSTL
  output$DecompTS_autoSTL <- renderUI({
    if (input$DecompTS_chooseautoSTL == "No") {
      list(sliderInput("DecompTS_TrendWindow", "Trend Window", min = 1, max = 365, value = 20),
           sliderInput("DecompTS_SeasonWindow", "Season Window", min = 1, max = 365, value = 20))
    } 
  })
  
  ## 2. Data Preparation using Reactive Expression
  DecompTS_reactiveVariable <- reactive({

    # Prepare Data
    selected_var <- input$DecompTS_selected_var
    char_startDate <- as.character(input$DecompTS_startDate)
    char_endDate <- as.character("2023-12-31")
    
    variable_data <- weather_tsbl %>%
      filter_index(char_startDate ~ char_endDate) %>%
      filter(Station == input$DecompTS_selectstation)
    
    if (input$DecompTS_time_resolution == "Day") {
      variable_data <- variable_data %>%
        mutate(ValueToPlot = .data[[selected_var]])
    } else if (input$DecompTS_time_resolution == "Week") {
      variable_data <- variable_data %>%
        group_by_key() %>%
        index_by(year_week = ~ yearweek(.)) %>%
        summarise(ValueToPlot = calculateValueToPlot(cur_data(), selected_var), .groups = 'drop') %>%
        mutate(Date = floor_date(as.Date(year_week), unit = "week"))
    }
    
    time_title <- switch(input$DecompTS_time_resolution, "Day" = "Daily", "Week" = "Weekly", "Month" = "Monthly")
    var_title <- if (grepl("Rainfall", selected_var)) {"Total Rainfall (mm)"} else if (grepl("Temperature", selected_var)) {
      if(input$DecompTS_time_resolution == "Day"){selected_var} else {paste("Average", selected_var)}}
    
    title <- paste(time_title, var_title, "for", input$DecompTS_selectstation, "\n", char_startDate, "to", char_endDate)

    list(variable_data = variable_data, title = title)

  })
  
  ## 3. ACF Plot and PACF Plot
  ### ACF plot
  output$DecompTS_ACFPlot <- renderPlotly({
    # Extract data and variables from reactive expression
    res <- DecompTS_reactiveVariable()
    variable_data <- res$variable_data
    title <- res$title
    
    ACF <- variable_data %>%
      ACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("ACF plot of", title)) +
      theme_minimal()
    
    ggplotly(ACF)
  })
  ### PACF plot
  output$DecompTS_PACFPlot <- renderPlotly({ 
    # Extract data and variables from reactive expression
    res <- DecompTS_reactiveVariable()
    variable_data <- res$variable_data
    title <- res$title    
    PACF <- variable_data %>%
      PACF(ValueToPlot, lag_max = input$DecompTS_lags) %>%
      autoplot() +
      labs(title = paste("PACF plot of", title)) +
      theme_minimal()
    
    ggplotly(PACF)
  })
  
  # 4. DecompTS_STLPlot
  output$DecompTS_STLPlot <- renderPlotly({
    # Ensure the reactive variable is available before proceeding
    req(DecompTS_reactiveVariable())

    # Extract data from the reactive expression
    res <- DecompTS_reactiveVariable()
    variable_data <- res$variable_data
    title <- res$title
    
    if(input$DecompTS_chooseautoSTL == "No") {
      trend_window <- input$DecompTS_TrendWindow
      season_window <- input$DecompTS_SeasonWindow
    } else { 
      trend_window <- NULL
      season_window <- NULL
      }
      
    # Note that variable name is always 'ValueToPlot', because of the reactive variable. So, use it directly in STL
    stl_fit <- variable_data %>%
      model(STL(ValueToPlot ~ season(window = season_window) + trend(window = trend_window))) %>%
      components()

    # Convert to tibble and perform common transformations
    stl_fit_tibble <- as_tibble(stl_fit) %>%
      mutate(trend = round(trend, 2),
             season_adjust = round(season_adjust, 2),
             season_year = round(season_year, 4),
             remainder = round(remainder, 4))
    
    # Conditionally mutate season_week if the resolution is "Day"
    if(input$DecompTS_time_resolution == "Day") {
      stl_fit_tibble <- stl_fit_tibble %>%
        mutate(season_week = round(season_week, 4))
    }

    # Visualize the STL components
    plot_stl <- stl_fit %>%
      autoplot() +
      labs(title = title)

    # Convert the ggplot object to a plotly object
    ggplotly(plot_stl) %>%
      layout(plot_bgcolor="#edf2f7")
  })
  # Time Series: Forecasting ----
  
  ## 1. Dynamic UI for ForecastTS_dynamic_time_resolution
  output$ForecastTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Week"))
    } else if (grepl("Temperature", input$ForecastTS_selected_var)) {
      radioButtons("ForecastTS_time_resolution", label = "Select time resolution", c("Day" ,"Week"))
    }
  })
  
  ## 1.1. Dynamic UI for ForecastTS_dynamic_model_parameters
  output$ForecastTS_dynamic_model_parameters <- renderUI({
    # Check if any of the STL options are selected
    if (!is.null(input$ForecastTS_selectmodel) &&
        any(grepl("STL", input$ForecastTS_selectmodel))
        ) {
      list(
        sliderInput("trendWindow", "Trend Window", min = 1, max = 365, value = 20),
        sliderInput("seasonWindow", "Season Window", min = 1, max = 365, value = 20)
      )}
  })
  ## 1.2. Dynamic UI for ForecastTS_dynamic_forecast_period
  output$ForecastTS_dynamic_forecast_period <- renderUI({
    
    end_date <- as.Date("2023-12-31")
    total_days <- as.numeric(difftime(end_date, input$ForecastTS_startDate, units = "days"))
    
    # Calculate the length of the test data based on the selected train-test split
    max_forecast_period_days <- floor(total_days * (1-input$ForecastTS_train_test_split)/ 3) # To discuss
    
    sliderInput("forecast_period_days", 
                "Select Forecast Period", min = 1, max = max(max_forecast_period_days, 1), value = min(10, max_forecast_period_days), step = 1, post = " days")
    # sliderInput("days", "Select Forecast Period", min = 0, max = 365, value = 10, post = " days")
  })
  
  
  # Geospatial: tmap ----
  
  ## 1. Dynamic UI: selected time_resolution
  output$GS_dynamic_time_resolution <- renderUI({
    if (input$GS_time_resolution == "Day") {
        airDatepickerInput("GS_selected_date", label = "Select date", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM-dd")
    } else if (input$GS_time_resolution == "Month") {
        airMonthpickerInput("GS_selected_month", label = "Select month and year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy-MM")
    } else if (input$GS_time_resolution == "Year") {
      airYearpickerInput("GS_selected_year", label = "Select year", value = "2021-01-01", maxDate = "2023-12-31", minDate = "2021-01-01", dateFormat = "yyyy")
    }
  })
  
  ## 2. Prepare and store reactive data
  GS_reactiveDataTmap <- reactiveValues()
  observeEvent(input$GS_updatetmap, {
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)

    # Update reactive variable
    GS_reactiveDataTmap$variable_data <- results$variable_data
    GS_reactiveDataTmap$variable_data_sf <- results$variable_data_sf
    GS_reactiveDataTmap$legend_title <- results$legend_title
    GS_reactiveDataTmap$main_title <- results$main_title

  })
  
  ## 3. Output plot
  output$GS_tmap <- renderTmap({
    
    req(GS_reactiveDataTmap$variable_data_sf) # Check if reactive variable is not NULL to avoid errors before the first button press
    
    dynamicPopupVars <- setNames(list(GS_reactiveDataTmap$legend_title), GS_reactiveDataTmap$legend_title)
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("view")
    tm <- tm_shape(mpsz2019) +
      tm_borders() +
      tm_shape(GS_reactiveDataTmap$variable_data_sf) +
      tm_dots(col = 'ValueToPlot', title = GS_reactiveDataTmap$legend_title, popup.vars = dynamicPopupVars) +
      tm_layout(title = GS_reactiveDataTmap$main_title) 
    tm
  })
  
  
  # Geospatial: IDW ----
  
  ## 1. Dynamic UI: IDW Parameters
  ### Removed
  ## 2. Prepare and store reactive data
  GS_reactiveDataIDW <- reactiveValues() # To contain variable data and IDW parameters
  observeEvent(input$GS_updateIDW, {

    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data) 
    raster_layer <- GS_rasterlayer(input$GS_IDW_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- results$main_title
    legend_title <- results$legend_title
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Set nmax value
    nmax = input$GS_IDW_nmax
    
    # Create gstat object
    res <- gstat(formula = ValueToPlot ~ 1,locations = variable_data_sf, set = list(idp = 0), nmax = nmax)
    # Predict values
    resp <- predict(res,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    pred <- rasterize(resp, grid, field="pred", fun="mean")
    
    # Update reactive variable
    GS_reactiveDataIDW$variable_data <- variable_data
    GS_reactiveDataIDW$variable_data_sf <- variable_data_sf
    GS_reactiveDataIDW$legend_title <- legend_title
    GS_reactiveDataIDW$main_title <- main_title
    GS_reactiveDataIDW$pred <- pred

  })

  # 3. Output plot
  output$GS_IDW_map <- renderPlot({
    req(GS_reactiveDataIDW$pred) # Check if reactive variable is not NULL to avoid errors before the first button press
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataIDW$pred) +
      tm_raster(title = GS_reactiveDataIDW$legend_title, alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = GS_reactiveDataIDW$main_title, main.title.position = "center", main.title.size = 1.2, legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })

  # Geospatial: OK ----
  ## 1. Dynamic UI: OK Parameters
  ### Removed
  
  ## 2. Prepare and store reactive data
  GS_reactiveDataOK <- reactiveValues() # To contain variable data and IDW parameters
  observeEvent(input$GS_updateOK, {
    results <- GS_prepareVariableData(input$GS_selected_var, input$GS_time_resolution, input$GS_selected_date, weather_data)
    raster_layer <- GS_rasterlayer(input$GS_OK_res)
    variable_data <-results$variable_data
    variable_data_sf <- results$variable_data_sf
    main_title <- results$main_title
    legend_title <- results$legend_title
    grid <- raster_layer$grid
    coop <- raster_layer$coop
    
    # Generate Experimental Variogram
    v <- variogram(ValueToPlot ~ 1, 
                   data = variable_data_sf)
    
    # Fit Variogram
    fv <- fit.variogram(object = v,
                        model = vgm(psill = input$GS_OK_psill, model = input$GS_OK_model, range = input$GS_OK_range, nugget = input$GS_OK_nugget))
    
    # perform spatial interpolation
    k <- gstat(formula = ValueToPlot ~ 1,
               data = variable_data_sf,
               model = fv)
    
    # estimate the unknown grids
    resp <- predict(k,coop)
    resp$x <- st_coordinates(resp)[,1]
    resp$y <- st_coordinates(resp)[,2]
    resp$pred <- resp$var1.pred
    
    # create a raster surface data object
    kpred <- rasterize(resp, grid,
                       field = "pred")

    # Extracting the variance
    resp$variance <- resp$var1.var
    
    # Create a raster surface data object for variance
    kvar <- rasterize(resp, grid, field = "variance")
    
    # Update reactive variable
    GS_reactiveDataOK$variable_data <- variable_data
    GS_reactiveDataOK$variable_data_sf <- variable_data_sf
    GS_reactiveDataOK$v <- v
    GS_reactiveDataOK$fv <- fv
    GS_reactiveDataOK$legend_title <- legend_title
    GS_reactiveDataOK$main_title <- main_title
    GS_reactiveDataOK$kpred <- kpred
    GS_reactiveDataOK$kvar <- kvar
  })
  
  # 3. Output plots
  output$GS_OK_variogram <- renderPlot({
    
    # Check reactive variable
    req(GS_reactiveDataOK$v)
    plot(GS_reactiveDataOK$v, cex = 1.5)
    
  })
  output$GS_OK_fitted_variogram <- renderPlot({
    
    # Check reactive variable
    req(GS_reactiveDataOK$v)
    req(GS_reactiveDataOK$fv)
    
    plot(GS_reactiveDataOK$v, GS_reactiveDataOK$fv, cex = 1.5)
    
  })
  output$GS_OK_map <- renderPlot({
    
    # Check reactive variable
    req(GS_reactiveDataOK$kpred) 
    
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kpred) + 
      tm_raster(title = GS_reactiveDataOK$legend_title, alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = GS_reactiveDataOK$main_title, main.title.position = "center", main.title.size = 1.2, 
                legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
    
  })
  output$GS_OK_prediction_variance <- renderPlot({
    # Check reactive variable
    req(GS_reactiveDataOK$kvar)
    
    variance_title <- "Prediction Variance"
    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(GS_reactiveDataOK$kvar) + 
      tm_raster(title = variance_title,
                alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = "Kriging Prediction Variance", main.title.position = "center", main.title.size = 1.2, 
                legend.height = 0.45, legend.width = 0.35, frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
    
  })
}
# Section 8: Run the application ---- 
shinyApp(ui, server)