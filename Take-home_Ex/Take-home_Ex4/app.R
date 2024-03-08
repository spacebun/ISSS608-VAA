##### Set up #####
pacman::p_load(shiny, shinydashboard, tidyverse, ggplot2, dplyr, lubridate, plotly, ggHoriPlot, dtwclust, factoextra)

weather_data <- read_rds("data/weather_imputed_11stations.rds") 

weather_data <- weather_data %>% 
  mutate(Date_mine = make_date(2023, month(Date), day(Date)))

variables <- c("Daily Rainfall Total (mm)", "Mean Temperature (°C)", "Minimum Temperature (°C)", "Maximum Temperature (°C)")

##### Header and sidebar #####
header <- dashboardHeader(title = "Singapore Weather Analytics (2021-2023)")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Landing Page", tabName = "Cluster"),
    menuItem("EDA & CDA", tabName = "EDACDA"),
    menuItem("Univariate Forecasting", tabName = "Univariate"),
    menuItem("Cluster & Group Forecasting", tabName = "LandingPage")
    )
  )

##### HoriPlot ##### 
HoriPlot <-fluidRow(
  # Control 
  box(
    title = "Control for Horizon Plot for 1 Station", width = 3, status = "primary", solidHeader = TRUE,
    radioButtons("selectedVariable", "Choose variable", 
                 variables),
    selectInput("selectedStation", "Choose station", 
                choices = unique(weather_data$Station),
                selected = unique(weather_data$Station)
                ),
    actionButton("updatePlot", "Update Plot")
  ),
  # Output Plot
  box(
    title = "Horizon Plot for 1 Station", width = 9, status = "primary", solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("horizonPlot")
  ),
)

##### Clustering ##### 
Clustering <-fluidRow(
  box()
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
  # stationData <- eventReactive(input$updatePlot, {
  #   weather_data %>%
  #     filter(Station %in% input$selectedStation)
  # })
  # 
  reactiveDataHoriPlot <- eventReactive(input$updatePlot, {
    # Filter for the selected station
    stationData <- weather_data %>% filter(Station %in% input$selectedStation)
    
    # Dynamically reference selected variable
    selected_var <- input$selectedVariable
    
    # Continue with your data processing and return the final dataset
    return(list(data = stationData, var = selected_var))
  })
  
  output$horizonPlot <- renderPlot({
    # Obtain the reactive data
    res <- reactiveDataHoriPlot()
    data_to_plot <- res$data
    selected_var <- res$var
    
    
    # Step 1: compute origin and  horizon scale cutpoints: 
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
    # ori <- sum(range(cutpoints$selected_var))/2
    # sca <- seq(range(cutpoints$selected_var)[1], 
    #            range(cutpoints$selected_var)[2], 
    #            length.out = 7)[-4]
    
    ori <- round(ori, 2) # The origin, rounded to 2 decimal places
    sca <- round(sca, 2) # The horizon scale cutpoints

    # Step 3: Plot horizon plot
    data_to_plot %>% ggplot() +
      geom_horizon(aes(x = Date_mine, 
                       y = .data[[selected_var]],
                       fill = after_stat(Cutpoints)), 
                   origin = ori, horizonscale = sca) +
      scale_fill_hcl(palette = 'RdBu', reverse = T) +
      facet_grid(~Year ~ .) +
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
      ggtitle(paste(selected_var, "for",  input$selectedStation),
              "across the years 2021 to 2023")
    })  
  
}

# Run App
##########
# Run the application 
shinyApp(ui, server)