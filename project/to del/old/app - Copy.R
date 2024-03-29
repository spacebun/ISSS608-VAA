# Section 1: Set up ----
pacman::p_load(gstat)
pacman::p_load(tmap)
pacman::p_load(shiny, shinydashboard, shinyWidgets, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, sf, terra, viridis)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

mpsz2019 <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>% st_transform(crs = 3414)

# Section 2: Header and sidebar ----
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

# Section 5: Geospatial UI ----
GeospatialUI <- fluidPage(
  # Row 1
  fluidRow(
    # Inputs Control Box
    box(title = "Data Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_selected_var", "Choose variable", variables),
        radioButtons("GS_time_resolution", "Time resolution", c("Day", "Month", "Year")),
        uiOutput("GS_dynamic_time_resolution"), 
        actionButton("GS_updatetmap", "Update map")
        ),
    # Output Plot Box
    box(title = "Map",  width = 10, status = "primary", solidHeader = TRUE,
        tmapOutput("GS_tmap")
        )
    ),
  # Row 2
  fluidRow(
    # Inputs Control Box
    box(title = "IDW Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_show_IDW", "Show IDW?", c("Yes", "No"), selected = "Yes", inline = TRUE),
        uiOutput("GS_dynamic_IDW"),
        actionButton("GS_updateIDW", "Show Result")
        ),
    # Output Plot Box
    box(title = "IDW Map",  width = 10, status = "primary", solidHeader = TRUE,
        plotOutput("GS_IDW_map")
        )
    ),
  # Row 3
  fluidRow(
    # Inputs Control Box
    box(title = "Kriging Selection Parameters",  width = 2, status = "primary", solidHeader = TRUE,
        radioButtons("GS_show_OK", "Show Kriging?", c("Yes", "No"), selected = "Yes", inline = TRUE),
        uiOutput("GS_dynamic_OK"),
        actionButton("GS_updateOK", "Show Result")
        ),
    # Output Plot Box
    box(title = "Ordinary Kriging Map",  width = 10, status = "primary", solidHeader = TRUE,
        plotOutput("GS_OK_map")
        )
    )
  )


# Section 6: Dashboard Body and UI ----
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

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Section 7: Server code ----
server <- function(input, output) {
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
    
    legend_title <- if(grepl("Rainfall", selected_var)) {"Rainfall (mm)"} else if(grepl("Temperature", selected_var)) {"Temperature (°C)"}
    
    # # Update variable_data before converting to sf
    variable_data[[legend_title]] <- variable_data$ValueToPlot

    variable_data_sf <- st_as_sf(variable_data, coords = c("LONG", "LAT"), crs = 4326) %>%
    st_transform(crs = 3414)

    # Update reactive variable
    GS_reactiveDataTmap$variable_data <- variable_data
    GS_reactiveDataTmap$variable_data_sf <- variable_data_sf
    GS_reactiveDataTmap$legend_title <- legend_title 
    GS_reactiveDataTmap$main_title <- if(grepl("Rainfall", selected_var)) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily Rainfall (mm) for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly Rainfall (mm) for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly Rainfall (mm) for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    } else if(grepl("Temperature", selected_var)) {
      if(input$GS_time_resolution == "Day") {
        paste("Daily", selected_var, "for", format(as.Date(input$GS_selected_date), "%d %B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Month") {
        paste("Monthly", selected_var, "for", format(as.Date(input$GS_selected_month), "%B %Y"), "across stations in Singapore")
      } else if(input$GS_time_resolution == "Year") {
        paste("Yearly", selected_var, "for", format(as.Date(input$GS_selected_year), "%Y"), "across stations in Singapore")
      }
    }

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
                    min = 1, max = 10, value = 3)
      )
    }
    
  })

  ## 2. Prepare and store reactive data
  GS_reactiveDataIDW <- reactiveValues() # To contain variable data and IDW parameters
  observeEvent(input$GS_updateIDW, {

    # Use same variable data as in the tmap plot
    GS_reactiveDataIDW$variable_data <- variable_data
    
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

    # Title of plot


  })

  ## 3. Output plot
  output$GS_tmap <- renderTmap({

    req(GS_reactiveDataIDW$variable_data) # Check if reactive variable is not NULL to avoid errors before the first button press

    # Use reactive variable  for plotting

    tmap_options(check.and.fix = TRUE)
    tmap_mode("plot")
    tm_shape(pred) +
      tm_raster(title = raster_title,
                alpha = 0.6, palette = "viridis") +
      tm_layout(main.title = main_title, main.title.position = "center", main.title.size = 1.2,
                legend.height = 0.45, legend.width = 0.35,
                frame = TRUE) +
      tm_compass(type="8star", size = 2) + tm_scale_bar() + tm_grid(alpha =0.2)
  })

  # Geospatial: OK ----
  ## 1. Dynamic UI: OK Parameters
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
# Section 8: Run the application ---- 
shinyApp(ui, server)