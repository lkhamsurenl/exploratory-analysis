library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyFeedback)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)
library(visNetwork)

CONCEPTS_FILENAME <- "concepts.csv"
MEASUREMENT_REFERENCE_RANGE_FILENAME <- "measurement_reference_range.csv"
RELATIONS_FILENAME <- "relations.csv"

CONCEPTS_TAB <- "concepts"
MEASUREMENT_REFERENCE_RANGE_TAB <- "referenceRanges"
RELATIONS_AB <- "relations"
VISUALIZATION_TAB <- "visualizations"

ui <- function(request) {
  fluidPage(
    theme = shinytheme("paper"),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    titlePanel("Non communicable disease risk factors"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("inputs"),
        hr(),
        actionButton("save", "Save", class="btn-success")
      ),
      mainPanel(
        tabsetPanel(
          id = "options",
          type = "tabs",
          tabPanel("Concepts", value = CONCEPTS_TAB, dataTableOutput("conceptsCSV")),
          tabPanel("Measurement Range", value = MEASUREMENT_REFERENCE_RANGE_TAB, dataTableOutput("referenceRangeCSV")),
          tabPanel("Relations", value = RELATIONS_AB, dataTableOutput("relationsCSV")),
          tabPanel("Visualization", value = VISUALIZATION_TAB, visNetworkOutput("graph"))
        )
      )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  dat <- reactiveValues(
    concepts = read_csv(CONCEPTS_FILENAME, col_types = cols(concept_id = "c")) %>% 
      mutate(display_name = glue("{concept_name} ({concept_id})")),
    measurementReferenceRanges = read_csv(MEASUREMENT_REFERENCE_RANGE_FILENAME, col_types = cols(concept_id = "c")),
    relations = read_csv(RELATIONS_FILENAME, col_types = cols(cause_concept_id = "c", effect_concept_id = "c"))
  )
  
  measurements <- reactive({
    dat$concepts %>% 
      filter(concept_type == "MEASUREMENT")
  })
  
  observeEvent(input$options, {
    if (input$options == VISUALIZATION_TAB) {
      shinyjs::disable("save")
    } else {
      shinyjs::enable("save")
    }
  })
  
  output$inputs <- renderUI({
    req(input$options %in% c(CONCEPTS_TAB, MEASUREMENT_REFERENCE_RANGE_TAB, RELATIONS_AB))
    
    if (input$options == CONCEPTS_TAB) {
      div(
        textInput("conceptId", "Concept ID"),
        textInput("conceptName", "Concept Name"),
        selectInput("conceptType", "Concept Type", choices = c("CONDITION", "RISK_FACTOR", "MEASUREMENT", "ACTION")),
        textInput("note", "Additional Note")
      )
    } else if (input$options == MEASUREMENT_REFERENCE_RANGE_TAB) {
      div(
        selectizeInput(
          "measurementConcept", "Measurement Concept", choices = measurements()$display_name,
          options = list(
            placeholder = 'Search',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        numericInput("lowRange", "Low Range", -Inf),
        numericInput("highRange", "High Range", Inf)
      )
    } else {
      div(
        selectizeInput(
          "effectConcept", "Effect", choices = dat$concepts$display_name,
          options = list(
            placeholder = 'Search',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        textInput("url", "Source URL"),
        hr(),
        selectInput("causeConcept", "Causes", choices = dat$concepts$display_name, multiple = TRUE)
      )
    }
  })
  
  output$conceptsCSV <- renderDataTable({
    dat$concepts
  })
  
  output$referenceRangeCSV <- renderDataTable({
    dat$measurementReferenceRanges
  })
  
  output$relationsCSV <- renderDataTable({
    dat$relations
  })
  
  # save output, depending on type
  observeEvent(input$save, {
    req(input$options %in% c(CONCEPTS_TAB, MEASUREMENT_REFERENCE_RANGE_TAB, RELATIONS_AB))
    
    if (input$options == CONCEPTS_TAB) {
      req(input$conceptId, input$conceptName, input$conceptType)
      
      # validate concept id
      isConceptIdNumeric <- grepl("^[0-9]+$", input$conceptId, perl = T)
      shinyFeedback::feedbackDanger("conceptId", !isConceptIdNumeric, "Concept is not valid numeric!")
      req(isConceptIdNumeric)
      
      isConceptIdAlreadyExists <- input$conceptId %in% dat$concepts$concept_id
      shinyFeedback::feedbackDanger("conceptId", isConceptIdAlreadyExists, "Concept is already added!")
      req(!isConceptIdAlreadyExists)
      
      dat$concepts <- dat$concepts %>% 
        add_row(
          concept_id = input$conceptId, 
          concept_name = input$conceptName, 
          concept_type = input$conceptType,
          note = input$note,
          display_name = glue("{input$conceptName} ({input$conceptId})")
        )
      
      dat$concepts %>% 
        select(-display_name) %>% 
        write_csv(CONCEPTS_FILENAME)
      
      # clear the input
      shinyjs::reset("conceptId")
      shinyjs::reset("conceptName")
    } else if (input$options == MEASUREMENT_REFERENCE_RANGE_TAB) {
      req(input$measurementConcept, input$lowRange, input$highRange)
      
      # there are list of effect concepts listed for same condition & url
      dat$measurementReferenceRanges <- dat$measurementReferenceRanges %>% 
        add_row(
          concept_id = extractConceptId(input$measurementConcept), 
          low_range = input$lowRange, 
          high_range = input$highRange
        )
      dat$measurementReferenceRanges %>% 
        write_csv(MEASUREMENT_REFERENCE_RANGE_FILENAME)
      
      # clear the input
      shinyjs::reset("measurementConcept")
      shinyjs::reset("lowRange")
      shinyjs::reset("highRange")
    } else {
      req(input$causeConcept, input$effectConcept, input$url)
      
      # there are list of effect concepts listed for same condition & url
      dat$relations <- dat$relations %>% 
        add_row(
          cause_concept_id = extractConceptId(input$causeConcept), 
          effect_concept_id = extractConceptId(input$effectConcept), 
          url = input$url
        )
      dat$relations %>% 
        write_csv(RELATIONS_FILENAME)
      
      # clear the input
      shinyjs::reset("causeConcept")
      shinyjs::reset("effectConcept")
      shinyjs::reset("url")
    }
  })
  
  ### visualization
  output$graph <- renderVisNetwork({
    nodes <- dat$concepts %>% 
      filter(concept_type != "MEASUREMENT") %>% 
      select(concept_name, concept_type) %>%
      mutate(id = concept_name, label = concept_name) %>% 
      mutate(
        font.size = 10, 
        color = ifelse(concept_type == "CONDITION", "#FF0066", ifelse(concept_type == "RISK_FACTOR", "#3399FF", "#33FF66"))
      ) 
    
    edges <- dat$relations %>% 
      inner_join(dat$concepts, by = c("cause_concept_id" = "concept_id")) %>% 
      inner_join(dat$concepts, by = c("effect_concept_id" = "concept_id")) %>% 
      transmute(
        from = concept_name.x,
        to = concept_name.y
      )

    visNetwork(nodes, edges) %>% 
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = T),
        nodesIdSelection = list(enabled = TRUE, style = "background: #f8f8f8; color: darkgreen;")
      ) %>% 
      visEdges(arrows = "to") %>% 
      visLayout(randomSeed = 12)
  })
}

extractConceptId <- function(displayName) {
  # extract ID given display name in format: `smoking (ID)`
  sub(".+ \\(([0-9]+)\\)", "\\1", displayName)
}

# Run the application 
shinyApp(ui = ui, server = server)
