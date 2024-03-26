##### Set up #####
pacman::p_load(shiny, shinydashboard, tidyverse, ggplot2, dplyr, lubridate, ggthemes, plotly, ggHoriPlot, dtwclust, factoextra, ggstatsplot)

weather <- read_rds("data/weather_imputed_11stations.rds") 

weather <- weather %>% 
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

##### EDAPLOT ##### 

EDAPlot <- fluidPage(
  # Row 1: Control
  fluidRow(
    box(
      title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("CompareAcross", "Compare across", c("Stations", "Years")),
      uiOutput("dynamicUIForEDA"),
      actionButton("updateEDAPlot", "Update Plot")
    ),
    # Output Plot
    box(
      title = "EDA Plot", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("edaPlot"),
    ),
    box(
      title = "Key Observation", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE
    )
  )
)

##### CDAPLOT ##### 

CDAPlot <- fluidPage(
  # Row 1: Control
  fluidRow(
    box(
      title = "Controls", width = 3, status = "primary", solidHeader = TRUE,
      radioButtons("selectedVariable", "Choose variable", variables),
      radioButtons("CompareAcross", "Compare across", c("Stations", "Years")),
      radioButtons("selectedStatApproach", "Statistical Approach",
                   choices = c("parametric", "nonparametric", "robust", "bayes")),
      uiOutput("dynamicUIForCDA"),
      actionButton("updateCDAPlot", "Update Plot")
    ),
    # Output Plot
    box(
      title = "CDA Plot", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput("cdaPlot") 
    ),
    box(
      title = "Key Observation", width = 9, status = "primary", solidHeader = TRUE,
      collapsible = TRUE
    )
  )
)

# Define sub tabs needed for "EDA&CDA" tabName "EDACDA"
EDACDASubTabs <- tabsetPanel(
  tabPanel("Exploratory Data Analysis",
           EDAPlot),
  tabPanel("Confirmatory Data analysis",
           CDAPlot)
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
            h2("Exploratory and Confirmatory Data analysis"),
            EDACDASubTabs
            
    ),    
    # Second tab content
    tabItem(tabName = "Univariate",
            h2("Univariate content")
            
    ),    
    # Second tab content
    tabItem(tabName = "Cluster",
            h2("Clustering and Group Forecasting")
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
server <- function(input, output, session) {
  
  ##### For EDA #####
  # Dynamic UI
  output$dynamicUIForEDA <- renderUI({
    if (input$CompareAcross == "Years") {
      list(
        checkboxGroupInput("stationSelectionForEDA", "Select Station",
                    choices = unique(weather$Station),
                    selected = unique(weather$Station)[1])
      )
    } else if (input$CompareAcross == "Stations") {
      list(
        checkboxGroupInput("stationSelectionForEDA", "Select Stations",
                    choices = unique(weather$Station),
                    selected = unique(weather$Station)[1]),
        selectInput("yearSelectionForEDA", "Select Year",
                    choices = unique(weather$Year), 
                    selected = unique(weather$Year)[1])
      )
    }
  })
  
  # Reactive update button
  reactiveDataEDA <- eventReactive(input$updateEDAPlot, {
    selected_var <- input$selectedVariable
    
    # Filter data based on user input
    if (input$CompareAcross == "Years") {
      selected_data <- weather %>%
        filter(Station %in% input$stationSelectionForEDA)
    } else if (input$CompareAcross == "Stations"){
      selected_data <- weather %>%
        filter(Year == input$yearSelectionForEDA,
               Station %in% input$stationSelectionForEDA)
    }
    
    facet_var = ifelse(input$CompareAcross == "Years", "Year", "Station")
    
    title <- ifelse(input$CompareAcross == "Years",
                    paste(selected_var, "for", "station(s)", "across the years 2021 to 2023"),
                    paste(selected_var, "for", input$yearSelectionForEDA, "across station(s)"))
    

    return(list(data = selected_data,var = selected_var, facet = facet_var, title = title))
  })
  
  # Output the plot
  output$edaPlot <- renderPlotly({
    # Get the reactive data
    res <- reactiveDataEDA()
    data_to_plot <- res$data
    selected_var <- res$var
    facet_var <- res$facet
    title <- res$title
    
    # Plot the rainfall
    if (input$selectedVariable == "Daily Rainfall Total (mm)") {
      ggplotly(ggplot(data_to_plot, 
                      aes(x = Station, y = `Daily Rainfall Total (mm)`)) +
                 geom_point(aes(color = `Daily Rainfall Total (mm)`,
                         text = paste('Year:', Year, 
                                      'Month:', Month_Name, 
                                      'Day:', day(Date),
                                      'Rainfall:', `Daily Rainfall Total (mm)`)),
                         size = 2.5) +
          labs(title = title,
               xaxis = list(title = facet_var),
               yaxis = list(title = selected_var)) +
          theme_minimal() +
          scale_color_gradient(low = "lightblue", high = "darkblue") +
            if (input$CompareAcross == "Years") {
            facet_wrap(~Year,
                       ncol = 1)} 
        else {NULL},
        tooltip = "text") %>%
        layout(hovermode = 'closest',
               xaxis = list(tickangle = 45,
                            tickfont = list(size = 12)),
               margin = list(l = 60, r = 60, b = 80, t = 80, pad = 4)) %>%
        config(displayModeBar = FALSE)
    }else{
      # Plot the temperature
      plot_ly(data = data_to_plot,
            x = ~get(facet_var),  
            y = ~get(selected_var),
            split = ~Station,
            type = 'violin',
            color = ~Station,
            box = list(visible = T),
            meanline = list(visible = T),
            span = I(1), 
            pointpos = 0) %>%
      layout(title = title, 
             xaxis = list(title = facet_var),
             yaxis = list(title = selected_var),
             violinmode = "group")
      }
  })
  

  ##### For CDA #####
  # Dynamic UI
  output$dynamicUIForCDA <- renderUI({
    if (input$CompareAcross == "Years") {
      list(
        checkboxGroupInput("stationSelectionForCDA", "Select Station",
                           choices = unique(weather$Station),
                           selected = unique(weather$Station)[1])
      )
    } else if (input$CompareAcross == "Stations") {
      list(
        checkboxGroupInput("stationSelectionForCDA", "Select Stations",
                           choices = unique(weather$Station),
                           selected = unique(weather$Station)[1]),
        selectInput("yearSelectionForCDA", "Select Year",
                    choices = unique(weather$Year), 
                    selected = unique(weather$Year)[1])
      )
    }
  })
  
  # Reactive update button
  reactiveDataCDA <- eventReactive(input$updateCDAPlot, {
    selected_var <- input$selectedVariable
    
    # Filter data based on user input
    if (input$CompareAcross == "Years") {
      selected_data <- weather %>%
        filter(Station %in% input$stationSelectionForCDA)
    } else if (input$CompareAcross == "Stations"){
      selected_data <- weather %>%
        filter(Year == input$yearSelectionForCDA,
               Station %in% input$stationSelectionForCDA)
    }
    
    title <- ifelse(input$CompareAcross == "Years",
                    paste(selected_var, "for", "station(s)", "across the years 2021 to 2023"),
                    paste(selected_var, "for", input$yearSelectionForCDA, "across station(s)"))
    
    
    return(list(data = selected_data,var = selected_var, title = title))
  })
  
  # Output the CDA plot
  output$cdaPlot <- renderPlotly({
    # Get the reactive data
    res <- reactiveDataCDA()
    data_to_plot <- res$data
    selected_var <- res$var
    title <- res$title
    
    # Convert the selected variable to a symbol for ggplot
    var_symbol <- rlang::sym(selected_var)
    
    if (input$CompareAcross == "Years") {
      # Generate the plot with grouped_ggbetweenstats
      grouped_ggbetweenstats(data = data_to_plot,
                                  x = "Station",
                                  y = !!var_symbol,
                                  grouping.var = "Year",
                                  type = input$selectedStatApproach, 
                                  mean.ci = TRUE, 
                                  pairwise.comparisons = TRUE, 
                                  pairwise.annotation = FALSE,
                                  pairwise.display = "none", 
                                  sig.level = NA,
                                  p.adjust.method = "fdr",
                                  messages = FALSE)
    } else {
      # Generate the plot with ggbetweenstats
      ggbetweenstats(data = data_to_plot,
        x = "Station",
        y = !!var_symbol, 
        type = input$selectedStatApproach,
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.annotation = "p.value",
        pairwise.display = "none", 
        sig.level = NA,
        p.adjust.method = "fdr",
        messages = FALSE)
    }
    
  })
  
}


# Run App
##########
# Run the application 
shinyApp(ui, server)