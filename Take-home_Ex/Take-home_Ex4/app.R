pacman::p_load(shiny, tidyverse)

data <- read_csv("data/Exam_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pupils Examination Result Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH", "Maths" = "MATHS", "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bins",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value= 10)
    ),
    mainPanel(plotOutput("distPlot")) # static graphs by default
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data,
           aes_string(x = input$variable)) +
      geom_histogram(bins = input$bins,
                    color = "black",
                    fill = "light blue") +
      labs(title = paste("Distribution of scores for",  input$variable),
           x = "Scores",
           y = "Count of Students",
           caption = "Data source: PISA 2022")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
