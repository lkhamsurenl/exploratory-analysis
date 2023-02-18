library(shiny)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)

ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("Covid cases by country"),
    
    sidebarLayout(
      sidebarPanel(
        textInput("country", "Country"),
        dateRangeInput("date_range", "Dates", start = Sys.Date() %m+% months(-1), end = Sys.Date()),
        hr(),
        actionButton("run", "Run")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot")
      )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  normalized_country <- reactive({
    req(input$country)
    gsub("\\s", "-", tolower(input$country))
  })
  
  country_df <- eventReactive(input$run, {
    shinyjs::disable("run")
    on.exit(shinyjs::enable("run"), add = TRUE)
    req(input$country)
    
    res <- GET(glue("https://api.covid19api.com/country/{normalized_country()}/status/confirmed?from={input$date_range[1]}&to={input$date_range[2]}"))
    
    fromJSON(rawToChar(res$content))
  })
  
  output$plot <- renderPlot({
    country_df() %>% 
      group_by(Date) %>% 
      summarise(Cases = sum(Cases)) %>% 
      ggplot(aes(x = as.Date(Date), y = Cases)) +
      geom_line() +
      labs(
        title = glue("Number of confirmed cases for {input$country}"),
        x = "Date",
        y = "Confirmed cases"
      )
  })
  
  ### Bookmarking
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

