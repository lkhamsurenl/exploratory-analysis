library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)

CONCEPTS_FILENAME <- "~/development/exploratory-analysis/risk_factor/concepts.csv"
MEASUREMENT_REFERENCE_RANGE_FILENAME = "~/development/exploratory-analysis/risk_factor/measurement_reference_range.csv"
MEASUREMENTS_FILENAME <- "measurements.csv"

VITALS <- c("Blood Pressure", "Body Temperature", "Body mass index", "Height", "Heart Rate", "Respiratory Rate", "Weight")
VITALS_REGEX <- paste0("(", paste(tolower(VITALS), collapse = '|'), ")")

ui <- function(request) {
  fluidPage(
    theme = shinytheme("paper"),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("PPP"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "measurementType", "Measurement Type", c("Labs", "Vitals", "Summary"),
          options = list(
            placeholder = 'Please search',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectInput("options", "Options", c()),
        hr(),
        actionButton("add", "Add new measurement")
      ),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          type = "hidden",
          tabPanel("Measurements", value = "measurement_tab", plotOutput("measurementPlot")),
          tabPanel("Summary", value = "summary_tab", dataTableOutput("summary"))
        )
      )
    )
  )
}

server <- function(input, output, session) {
  dat <- reactiveValues(
    concepts = read_csv(CONCEPTS_FILENAME, col_types = cols(concept_id = "c")) %>% 
      filter(concept_type == "MEASUREMENT"),
    measurementReferenceRanges = read_csv(MEASUREMENT_REFERENCE_RANGE_FILENAME, col_types = cols(concept_id = "c")),
    measurements = read_csv(MEASUREMENTS_FILENAME, col_types = cols(concept_id = "c"))
  )
  
  measurementConcepts <- reactive(
    dat$measurements %>% 
      inner_join(dat$concepts, by = c("concept_id")) %>% 
      filter(str_detect(tolower(concept_name), tolower(input$options)))
  )
  
  measurementRange <- reactive({
    dat$measurementReferenceRanges %>% 
      inner_join(dat$concepts, by = c("concept_id")) %>% 
      filter(str_detect(tolower(concept_name), tolower(input$options))) %>% 
      mutate(
        high = ifelse(!is.na(high_range), high_range, Inf),
        low = ifelse(!is.na(low_range), low_range, -Inf),
        range_name = glue("{concept_name} range")
      )
  })
  
  labNames <- reactive(
    dat$concepts %>% 
      filter(!str_detect(tolower(concept_name), VITALS_REGEX)) %>% 
      arrange(concept_name) %>% 
      pull(concept_name)
  )
  
  observeEvent(input$measurementType, {
    req(input$measurementType)
    if (input$measurementType == "Labs") {
      updateSelectInput(session, "options", choices = labNames())
      updateTabsetPanel(session, "tabs", selected = "measurement_tab")
    } else if(input$measurementType == "Vitals") {
      updateSelectInput(session, "options", choices = VITALS)
      updateTabsetPanel(session, "tabs", selected = "measurement_tab")
    } else {
      updateTabsetPanel(session, "tabs", selected = "summary_tab")
    }
  })
  
  ### Add new measurement
  observeEvent(input$add, {
    showModal(modalDialog(
      title = "Add new value",
      textInput("userId", "User Name"),
      dateInput("date", "Date", format = "yyyy-mm-dd"),
      tags$div(id = 'placeholder'),
      actionButton("new", "New"),
      hr(),
      actionButton("save", "Save")
    ))
  })
  
  inserted <- c()
  observeEvent(input$new, {
    btn <- input$new
    insertUI(
      selector = '#placeholder',
      ui = fluidRow(
        column(6, selectizeInput(
          glue("conceptNameId{btn}"), "Concept", choices = glue("{dat$concepts$concept_name} ({dat$concepts$concept_id})"),
          options = list(
            placeholder = 'Search',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )),
        column(2, numericInput(glue("value{btn}"), "Value", value = -Inf))
      )
    )
    inserted <<- c(btn, inserted)
  })
  
  observeEvent(input$save, {
    req(input$userId, input$date)
    
    for (i in 1:inserted) {
      req(input[[glue("conceptNameId{i}")]], input[[glue("value{i}")]])
      
      dat$measurements <- dat$measurements %>% 
        add_row(
          user_id = input$userId, 
          concept_id = sub(".+ \\(([0-9]+)\\)", "\\1", input[[glue("conceptNameId{i}")]]), 
          value = input[[glue("value{i}")]],
          date = input$date
        )
    }
    dat$measurements %>% 
      write_csv(MEASUREMENTS_FILENAME)
    
    removeModal()
  })
  
  ### Plot
  output$measurementPlot <- renderPlot({
    req(input$measurementType %in% c("Labs", "Vitals"), input$options)
    # browser()
    measurementConcepts() %>% 
      ggplot(aes(x = date, y = value, color = concept_name)) + 
      geom_line() + 
      geom_point() + 
      geom_rect(
        data = measurementRange(),
        aes(ymin = low, ymax = high, fill = range_name), 
        color = NA, 
        alpha = 0.1, 
        xmin = -Inf, xmax = Inf,
        inherit.aes = FALSE
      ) +
      geom_hline(
        data = measurementRange(), 
        aes(yintercept = low), 
        linetype = 2
      ) + 
      geom_hline(
        data = measurementRange(), 
        aes(yintercept = high), 
        linetype = 2
      ) + 
      labs(
        title = input$options,
        x = "Measurement date",
        y = "Value"
      ) 
      # scale_fill_manual("Reference range", values = rep("palegreen", nrow(measurementRange())))
  })
  
  ### Summary
  output$summary <- renderDataTable({
    measurementConcepts() %>% 
      inner_join(measurementRange(), by = c("concept_id", "concept_name", "concept_type", "note")) %>% 
      filter(value < low | value > high) %>% 
      select(user_id, date, concept_name, concept_type, note, value, low_range, high_range)
  })
}

shinyApp(ui = ui, server = server)