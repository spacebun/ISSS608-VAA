# Section 1: Set up ----
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, sf, terra, viridis,ggHoriPlot, ggstatsplot, rstantools)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% st_transform(crs = 3414)

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

# Section 3.1: CDA compare across stations UI  ----

CDAUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        selectInput("CDA_selected_var", "Choose variable", variables, selected = NULL, multiple = FALSE),
        radioButtons("CDA_selectedStatApproach", "Statistical Approach", choices = c("parametric", "nonparametric", "robust", "bayes"))
    ),
    tabBox(
      title = "Plots", width = 10,
      # The id lets us use input$EDACDA_tab on the server to find the current tab
      id = "EDACDA_tab", height = "250px",
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
                 column(3, box(title = "parameters")),
                 column(9,box(title = "plot"))
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
        uiOutput("ExploreTS_dynamic_time_resolution")
    ),
    tabBox(
      title = "Plots", width = 10,
      # The id lets us use input$ExploreTS_tab on the server to find the current tab
      id = "ExploreTS_tab", height = "250px",
      tabPanel("TAB1",
               fluidRow(
                 column(3, actionButton("TS_updateplot", "Update plot")), 
                 column(9, box(title = "plot")) 
               )
      ),
      tabPanel("TAB2",
               fluidRow(
                 column(3, box(title = "parameters")),
                 column(9,box(title = "plot"))
               )
      )
    )
  )
)

# Section 4.2: DecomposeTS UI ----
DecomposeTSUI <- fluidPage(
  # Row 1
  fluidRow(
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("TS_selected_var", "Choose variable", variables),
        radioButtons("TS_time_resolution", "Time resolution", c("Day", "Weekly")) # Should be reactive
    ),
    tabBox(
      title = "Plots", width = 10,
      # The id lets us use input$DecomposeTS_tab on the server to find the current tab
      id = "DecomposeTS_tab", height = "250px",
      tabPanel("TAB1",
               fluidRow(
                 column(3, actionButton("TS_updatetplot", "Update plot")), 
                 column(9, box(title = "plot"))
               )
      ),
      tabPanel("TAB2",
               fluidRow(
                 column(3, box(title = "parameters")),
                 column(9,box(title = "plot"))
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
        radioButtons("TS_selected_var", "Choose variable", variables),
        radioButtons("TS_time_resolution", "Time resolution", c("Day", "Weekly")) # Should be reactive
    ),
    tabBox(
      title = "Plots", width = 10,
      # The id lets us use input$ForecastTS_tab on the server to find the current tab
      id = "ForecastTS_tab", height = "250px",
      tabPanel("TAB1",
               fluidRow(
                 column(3, actionButton("ForecastTS_updatetplot", "Update plot")), 
                 column(9, box(title = "plot"))
               )
      ),
      tabPanel("TAB2",
               fluidRow(
                 column(3, box(title = "parameters")),
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
      tabPanel("IDW",
               fluidRow(
               column(3, radioButtons("GS_show_IDW", "Show IDW?", c("Yes", "No"), selected = "No", inline = TRUE),
               uiOutput("GS_dynamic_IDW")),
               column(9,plotOutput("GS_IDW_map"))
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
            DecomposeTSUI
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
      title <- paste("Compare", selected_var, "across stations for", format(as.Date(input$CDA_acrossstations_selected_month), "%B %Y"))
      # If Time resolution is Year
    } else if (input$CDA_acrossstations_time_resolution == "Year") {
      
      selected_year <- as.character(format(input$CDA_acrossstations_selected_year, "%Y"))
      
      # Select data
      variable_data <- weather_data %>%
        filter(format(Date, "%Y") == selected_year,
               Station %in% input$CDA_acrossstations_stationSelection)
      
      # Set plot title
      title <- paste("Compare", selected_var, "across stations for", format(as.Date(input$CDA_acrossstations_selected_year), "%Y"))
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
                   pairwise.comparisons = TRUE, 
                   pairwise.annotation = "p.value",
                   pairwise.display = "none", 
                   sig.level = NA,
                   p.adjust.method = "fdr",
                   messages = FALSE,
                   title = CDA_acrossstations_reactiveData$title)
    
  })
  
  # Time Series: Exploratory Horizon Plot ----
  
  ## 1. Dynamic UI
  output$ExploreTS_dynamic_time_resolution <- renderUI({
    if (grepl("Rainfall", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Week", "Month"))
    } else if (grepl("Temperature", input$ExploreTS_selected_var)) {
      radioButtons("ExploreTS_time_resolution", label = "Select time resolution", c("Day" ,"Week", "Month"))
    }
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
    selected_var <- input$GS_selected_var 
    
    # If Time resolution is Day
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
      
    # If Time resolution is Month
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
      
    # If Time resolution is Year
    } else if (input$GS_time_resolution == "Year") {
      selected_year <- as.character(format(input$GS_selected_year, "%Y"))
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
    
    legend_title <- if(grepl("Rainfall", input$GS_selected_var )) {"Rainfall (mm)"} else if(grepl("Temperature", input$GS_selected_var )) {"Temperature (°C)"}

    # # Update variable_data before converting to sf
    variable_data[[legend_title]] <- variable_data$ValueToPlot

    variable_data_sf <- st_as_sf(variable_data, coords = c("LONG", "LAT"), crs = 4326) %>%
    st_transform(crs = 3414)
    
    main_title <- if(grepl("Rainfall", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily Rainfall (mm) for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly Rainfall (mm) for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly Rainfall (mm) for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    } else if(grepl("Temperature", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily", input$GS_selected_var , "for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    }
    
    # Update reactive variable
    GS_reactiveDataTmap$variable_data <- variable_data
    GS_reactiveDataTmap$variable_data_sf <- variable_data_sf
    GS_reactiveDataTmap$legend_title <- legend_title 
    GS_reactiveDataTmap$main_title <- main_title

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
      tm_layout(title = GS_reactiveDataTmap$main_title) # Add the dynamic title here
    
    tm
  })
  
  
  # Geospatial: IDW ----
  
  ## 1. Dynamic UI: IDW Parameters
  output$GS_dynamic_IDW <- renderUI({
    if (input$GS_show_IDW == "Yes") {
      list(
        sliderInput("GS_IDW_res", "Resolution", 
                    min = 30, max = 80, value = 50),
        sliderInput("GS_IDW_nmax" , "nmax",
                    min = 1, max = 10, value = 3),
        actionButton("GS_updateIDW", "Show Result")
      )
    }
    
  })

  ## 2. Prepare and store reactive data
  GS_reactiveDataIDW <- reactiveValues() # To contain variable data and IDW parameters
  observeEvent(input$GS_updateIDW, {
    variable_data <- GS_reactiveDataTmap$variable_data # Use same data from tmap
    variable_data_sf <- GS_reactiveDataTmap$variable_data_sf # Use same data from tmap
    
    # Specify resolution
    bbox <- as.list(st_bbox(mpsz2019))
    res = input$GS_IDW_res
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

    # Prepare plot elements
    legend_title <- if(grepl("Rainfall", input$GS_selected_var )) {"Rainfall (mm)"} else if(grepl("Temperature", input$GS_selected_var )) {"Temperature (°C)"}
    main_title <- if(grepl("Rainfall", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily Rainfall (mm) for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly Rainfall (mm) for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly Rainfall (mm) for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    } else if(grepl("Temperature", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily", input$GS_selected_var , "for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    }
    
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
  output$GS_dynamic_OK <- renderUI({
    if (input$GS_show_OK == "Yes") {
      model_choices <- c("Nug", "Exp","Sph","Gau","Exc","Mat","Ste","Cir","Lin","Bes","Pen","Per","Wav","Hol","Log","Pow","Spl")
      list(
        sliderInput("GS_OK_res", "Resolution", min = 30, max = 80, value = 50),
        selectInput("GS_OK_model" , "model", model_choices),
        sliderInput("GS_OK_psill", "psill", min = 0.5, max = 10, value = 0.5, step = 0.5),
        sliderInput("GS_OK_range", "range", min = 500, max = 10000, value = 8000, step = 500),
        sliderInput("GS_OK_nugget", "nugget", min = 0.1, max = 10, value = 0.1, step = 0.1),
        actionButton("GS_updateOK", "Show Result")
      )
    }
    
  })
  
  ## 2. Prepare and store reactive data
  GS_reactiveDataOK <- reactiveValues() # To contain variable data and IDW parameters
  observeEvent(input$GS_updateOK, {
    
    variable_data <- GS_reactiveDataTmap$variable_data # Use same data from tmap
    variable_data_sf <- GS_reactiveDataTmap$variable_data_sf # Use same data from tmap
    
    # Specify resolution
    bbox <- as.list(st_bbox(mpsz2019))
    res = input$GS_OK_res
    nrows =  (bbox$ymax - bbox$ymin)/res
    ncols = (bbox$xmax - bbox$xmin)/resx
    
    # Create raster layer, 'grid'
    grid <- rast(mpsz2019, nrows = nrows, ncols = ncols)
    # Generate coordinates for each cell of the raster
    xy <- xyFromCell(grid,1:ncell(grid))
    # Converting coordinates of raster into a spatial (sf) object
    coop <- st_as_sf(as.data.frame(xy), coords = c("x","y"), crs = st_crs(mpsz2019))
    # Filter to only only includes points within mpsz2019
    coop <- st_filter(coop, mpsz2019) 
    

    
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
    
    # plot elements
    legend_title <- if(grepl("Rainfall", input$GS_selected_var )) {"Rainfall (mm)"} else if(grepl("Temperature", input$GS_selected_var )) {"Temperature (°C)"}
    main_title <- if(grepl("Rainfall", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily Rainfall (mm) for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly Rainfall (mm) for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly Rainfall (mm) for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    } else if(grepl("Temperature", input$GS_selected_var )) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily", input$GS_selected_var , "for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly", input$GS_selected_var , "for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    }
    
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