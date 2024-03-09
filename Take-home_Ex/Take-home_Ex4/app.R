##### Set up #####
pacman::p_load(shiny, shinydashboard, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, ggHoriPlot, dtwclust, factoextra)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

weather_data <- weather_data %>% 
  mutate(Date_mine = make_date(2023, month(Date), day(Date)),
         Month_Name = factor(months(Date), levels = month.name)
  )

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

##### Header and sidebar #####
header <- dashboardHeader(title = "Singapore Weather Analytics (2021-2023)")

sidebar <- dashboardSidebar(
  width = 100,
  tags$head(tags$style(HTML("
      .sidebar-menu > li > a {
        white-space: normal; 
        line-height: 1.2;
      }
    "))
  ),
  sidebarMenu(
    menuItem("Landing Page", tabName = "LandingPage"),
    menuItem("EDA & CDA", tabName = "EDACDA"),
    menuItem("Univariate Forecasting", tabName = "Univariate"),
    menuItem("Cluster & Group Forecasting", tabName = "Cluster")
    )
  )

##### HoriPlot ##### 
HoriPlot <-fluidPage(
  # Row 1
  fluidRow(
    # Control 
    box(
      title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("compareAcrossHorizon", "Compare across", c("Stations", "Years")),
      uiOutput("dynamicUIHoriPlot"),  # Placeholder for dynamic UI components
      actionButton("updateHoriPlot", "Update Plot")
    ),
    
    # Output Plot
    box(
      title = "Horizon Plot", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("horizonPlot")
    )
  ),
  
  # Row 2
  fluidRow(
    # Output table
    box(
      title = "Table", width = 12, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      DT::dataTableOutput("horizonDataTable")
    )
    
  )
)

##### Clustering ##### 
Clustering <-fluidRow(
  box(title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("clusterBy", "Cluster by", c("Stations", "Months") ),
      selectInput("selectedClusterMethod", "Choose cluster method", c("partitional", "hierarchical", "tadpole","fuzzy")),
      sliderInput("numClusters", "Number of Clusters", 
                  min = 2, max = 10, value = 6),
      uiOutput("dynamicUIClustering"),  # Placeholder for dynamic UI components
      actionButton("updateClustering", "Update Clustering")
      ),
  box(title = "Clusters", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
    plotOutput("clusterDendrogram")
  )
)

##### Forecasting ##### 
Forecasting <-fluidRow(
  box()
)

# Define sub tabs needed for "Cluster & Group Forecasting" tabName "Cluster"
clusterSubTabs <- tabsetPanel(
  tabPanel("Horizon Plot",
           HoriPlot),
  tabPanel("Clustering",
           Clustering),
  tabPanel("Forecasting",
           Forecasting)
)


# Body content
body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "LandingPage",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
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
    tabItem(tabName = "Cluster",
            h2("Clustering and Group Forecasting"),
            clusterSubTabs
            
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
  
  ##### Dummy #####
  set.seed(122)
  histdata <- rnorm(500)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  ##### For HoriPlot #####
  # Dynamic UI
  output$dynamicUIHoriPlot <- renderUI({
    if (input$compareAcrossHorizon == "Years") {
      list(
        selectInput("selectedStation", "Choose station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1])
      )
    } else if (input$compareAcrossHorizon == "Stations") {
      list(
        selectInput("selectedStations", "Choose stations", 
                    choices = unique(weather_data$Station), 
                    multiple = TRUE,
                    selected = unique(weather_data$Station)[1]),
        selectInput("selectedYear", "Choose Year", choices = unique(weather_data$Year), selected = unique(weather_data$Year)[1])
      )
    }
  })
  
  # Reactive update button
  reactiveDataHoriPlot <- eventReactive(input$updateHoriPlot, {
    
    selected_var <- input$selectedVariable
    
    
    if (input$compareAcrossHorizon == "Years") {
      filtered_data <- weather_data %>%
        filter(Station %in% input$selectedStation)
    } else if (input$compareAcrossHorizon == "Stations") {
      filtered_data <- weather_data %>%
        filter(Station %in% input$selectedStations, Year == input$selectedYear)
    }
    
    facet_var = ifelse(input$compareAcrossHorizon == "Years", "Year", "Station")
    
    title <- ifelse(input$compareAcrossHorizon == "Years",
                            paste(selected_var, "for", input$selectedStation, "across the years 2021 to 2023"),
                            paste(selected_var, "for", input$selectedYear, "across station(s)"))
    
    return(list(data = filtered_data, var = selected_var, facet = facet_var, title = title))
  })
  
  # Output plot
  output$horizonPlot <- renderPlot({
    # Obtain the reactive data
    res <- reactiveDataHoriPlot()
    data_to_plot <- res$data
    selected_var <- res$var
    facet_var <- res$facet
    title <- res$title
    
    
    # Compute origin and  horizon scale cutpoints: 
    cutpoints <- data_to_plot %>% 
      mutate(
        outlier = between(
          .data[[selected_var]], 
          quantile(.data[[selected_var]], 0.25, na.rm = TRUE) -
            1.5 * IQR(.data[[selected_var]], na.rm = TRUE),
          quantile(.data[[selected_var]], 0.75, na.rm = TRUE) +
            1.5 * IQR(.data[[selected_var]], na.rm = TRUE))) %>% 
      filter(outlier)
    
    ori <- sum(range(cutpoints[[selected_var]]))/2
    sca <- seq(range(cutpoints[[selected_var]])[1], range(cutpoints[[selected_var]])[2], length.out = 7)[-4]
    
    ori <- round(ori, 2) # The origin, rounded to 2 decimal places
    sca <- round(sca, 2) # The horizon scale cutpoints

    # Plot horizon plot
    data_to_plot %>% ggplot() +
      geom_horizon(aes(x = Date_mine, 
                       y = .data[[selected_var]],
                       fill = after_stat(Cutpoints)), 
                   origin = ori, horizonscale = sca) +
      scale_fill_hcl(palette = 'RdBu', reverse = T) +
      facet_grid(as.formula(paste(facet_var, "~ ."))) +
      theme_few() +
      theme(
        panel.spacing.y = unit(0, "lines"),
        strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()
      ) +
      scale_x_date(expand = c(0, 0), 
                   date_breaks = "1 month", 
                   date_labels = "%b") +
      xlab('Date') +
      ggtitle(title)
    })
  output$horizonDataTable <- DT::renderDataTable({
    res <- reactiveDataHoriPlot()
    data_to_plot <- res$data
    selected_var <- res$var
    data_to_table <- data_to_plot %>%
      select(c(Station, Date, Year, Month, Day, selected_var))
      
    DT::datatable(data_to_table,
                  class= "compact",
                  rownames = FALSE,
                  width="100%", 
                  options = list(pageLength = 10, scrollX=T))  # Assuming 'weather_data' is your dataframe
  })
  
  ##### For Clustering #####
  # Dynamic UI
  output$dynamicUIClustering <- renderUI({
    if (input$clusterBy == "Months") {
      list(
        selectInput("selectedStation", "Choose station", choices = unique(weather_data$Station), selected = unique(weather_data$Station)[1]),
        selectInput("selectedYear", "Choose Year", choices = unique(weather_data$Year), selected = unique(weather_data$Year)[1])
      )
    } else if (input$clusterBy == "Stations") {
      list(
        selectInput("selectedStations", "Choose stations", 
                    choices = unique(weather_data$Station), 
                    multiple = TRUE,
                    selected = unique(weather_data$Station)[1])
      )
    }
  })
  
  reactiveDataClustering <- eventReactive(input$updateClustering, {
    selected_var <- input$selectedVariable
    
    selected_data <- weather_data %>%
      select(Station, Date, Year, Month, Day, .data[[selected_var]])
    
    if (input$clusterBy == "Months") {
      selected_data <- selected_data %>% 
        filter(Station %in% input$selectedStation, Year == input$selectedYear) %>%
        group_by(Month) %>%
        summarise(data_list = list(.data[[selected_var]]), .groups = 'drop')
      
      list_of_series <- selected_data$data_list
    } else if (input$clusterBy == "Stations") {
      # Placeholder for when 'Stations' is the selected clusterBy option
      # Implement according to your specific requirements
    }
    
    # Ensure to return relevant data that your subsequent plotting/logic expects
    return(list(data = selected_data, var = selected_var))
  })
  
  
  # Output clusterDendrogram
  output$clusterDendrogram <- renderPlot({
    res <- reactiveDataClustering()
    list_of_series <- res$data$data_list
    print(str(list_of_series))
    n_cluster <- input$numClusters # Need to shift this to reactive
    c <- tsclust(series = list_of_series, 
                 type = "hierarchical",
                 k = n_cluster, 
                 distance = "dtw", 
                 control = hierarchical_control(method = "complete"))
    p <- fviz_dend(c, k = n_cluster,
                   cex = 0.5, 
                   k_colors = c("jco"),
                   color_labels_by_k = FALSE,
                   rect_border = "jco",
                   rect = TRUE,
                   rect_fill = TRUE)
    print(p)
  })
  
}

# Run App
##########
# Run the application 
shinyApp(ui, server)