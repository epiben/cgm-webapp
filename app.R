library(shiny)
library(shinyWidgets)
library(yaml)
library(readr)
library(dplyr)
library(markdown)

replace_na <- function(x, replacement) {
  ifelse(is.na(x), replacement, x)
}

vbetween <- function(x, left, right) {
  # like dplyr::between but with vectorised bounds
  replace_na(left, -Inf) <= x & x <= replace_na(right, Inf)
}

rules <- read_delim(
  "rules.csv",
  delim = ";",
  locale = locale(decimal_mark = "."),
  col_types = cols(
    target = col_character(),
    step = col_character(),
    current_bgl = col_number(),
    current_min = col_number(),
    current_max = col_number(),
    previous_bgl = col_character(),
    previous_min = col_number(),
    previous_max = col_number(),
    change_text = col_character(),
    change = col_character(),
    change_min = col_number(),
    change_max = col_number(),
    dose = col_character(),
    dose_min = col_number(),
    dose_max = col_number(),
    action = col_number()
  )
)
actions <- read_yaml("actions.yaml")
messages <- read_yaml("messages.yaml")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("INCEPT-Albumin glucose management algorithm"),
  sidebarLayout(
    sidebarPanel(
      radioGroupButtons(
        "step",
        "Type",
        choices = list("30-min check", "First BGL", "On insulin", "Off insulin")
      ),
      numericInputIcon(
        "previous_bgl",
        "Previous blood glucose level",
        value = 6.0,
        min = 0,
        step = 0.1,
        icon = list(NULL, "mmol/L")
      ),
      numericInputIcon(
      	"current_bgl",
      	"Current blood glucose level",
      	value = 6.0,
      	min = 0,
      	step = 0.1,
      	icon = list(NULL, "mmol/L")
    	),
      numericInputIcon(
      	"current_dose",
      	"Insulin dose",
      	value = 0,
      	min = 0,
      	step = 1,
      	icon = list(NULL, "units/hour")
    	),
    ),
    mainPanel(
      htmlOutput("result")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  action <- reactive({
    req(input$step, input$current_bgl, input$previous_bgl, input$current_dose)
    rule <- rules %>%
      filter(
        step == input$step,
        vbetween(input$current_bgl, current_min, current_max),
        vbetween(input$previous_bgl, previous_min, previous_max),
        vbetween(input$current_bgl - input$previous_bgl, change_min, change_max),
        vbetween(input$current_dose, dose_min, dose_max)
      )

    if (nrow(rule) > 1) {
      actions <- do.call(paste, as.list(rule$action))
      stop("ERROR! More than one rule satisfied: ", actions)
    } else if (nrow(rule) == 0) {
      stop("No rules met")
    }

    actions[[rule[["action"]]]]
  })

  insulin_dose <- reactive({
    calculation <- action()$calculation

    if (is.null(calculation)) {
      return(NULL)
    }

    res <- switch(as.character(calculation),
      "1" = input$current_dose * input$current_bgl / input$previous_bgl,
      "2" = input$current_dose * input$current_bgl / input$previous_bgl / 2,
      "3" = input$current_dose + 2,
      "4" = input$current_dose + 1,
      "5" = input$current_dose,
      "6" = input$current_dose + 2 * input$current_bgl / input$previous_bgl,
      "7" = input$current_dose / 2
    )

    round(res, 1)
  })


  output$result <- renderUI({
    res <- messages[[action()$message]]
    if (!is.null(insulin_dose())) {
      res <- paste(res, "\n\nNew suggested dose:", insulin_dose(), "units/hour")
    }
    HTML(mark(text = res))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
