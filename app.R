# app.R — HW7 Shiny (clean, novice→intermediate)

# ---- Packages ----
library(shiny)
library(shinyalert)     # alerts per assignment
library(tidyverse)      # ggplot2, dplyr, tidyr, readr, etc.
library(DT)

# ---- Source + Data ----
source("helpers.R")                 # defines get_numeric_vars(), get_categorical_vars(), etc.
df <- readRDS("my_sample_temp.rds") # make sure this file is in the same folder

# derive choices from the data (functions avoid name clash with your numeric_vars vector)
num_choices <- get_numeric_vars(df)
cat_choices <- get_categorical_vars(df)

# ---- UI ----
ui <- navbarPage(
  title = "HW7 — Shiny Practice",
  
  # ---------- Tab 1: General exploration ----------
  tabPanel(
    "Explore",
    fluidPage(
      useShinyalert(),
      sidebarLayout(
        sidebarPanel(
          h4("Plot Controls"),
          radioButtons("plot_type", "Plot type",
                       choices = c("Histogram", "Scatter"), inline = TRUE),
          
          selectInput("xvar", "X variable (numeric)", choices = num_choices),
          selectInput("yvar", "Y variable (scatter only, numeric)",
                      choices = num_choices,
                      selected = if (length(num_choices) >= 2) num_choices[2] else num_choices[1]),
          
          selectInput("group", "Group / color (optional)",
                      choices = c("None", names(df)), selected = "None"),
          
          conditionalPanel(
            condition = "input.plot_type == 'Histogram'",
            sliderInput("bins", "Bins", min = 5, max = 60, value = 30, step = 1, width = "100%")
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Scatter'",
            sliderInput("ptsize", "Point size", min = 0.5, max = 4, value = 1.8, step = 0.1, width = "100%"),
            sliderInput("ptalpha", "Point alpha", min = 0.1, max = 1, value = 0.7, step = 0.05, width = "100%")
          ),
          
          # Quick categorical filter (first categorical column, if any)
          uiOutput("filter_ui"),
          br(),
          downloadButton("download_csv", "Download table")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Plot",    br(), plotOutput("main_plot", height = "480px")),
            tabPanel("Table",   br(), DTOutput("data_table")),
            tabPanel("Summary", br(), verbatimTextOutput("summary_txt"))
          )
        )
      )
    )
  ),
  
  # ---------- Tab 2: Correlation workflow ----------
  tabPanel(
    "Correlation",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Select Variables to Find Correlation:"),
          selectInput("corr_x", "X variable", choices = num_choices),
          selectInput("corr_y", "Y variable", choices = num_choices,
                      selected = if (length(num_choices) >= 2) num_choices[2] else num_choices[1]),
          sliderInput("corr_n", "Select a sample size",
                      min = 10, max = nrow(df), value = min(200, nrow(df)), step = 1),
          actionButton("corr_sample", "Get a Sample!")
        ),
        mainPanel(
          plotOutput("corr_scatter", height = "480px"),
          conditionalPanel("input.corr_sample > 0",
                           verbatimTextOutput("corr_text")
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # keep choices synced (defensive; df is static but this is fine)
  observe({
    updateSelectInput(session, "xvar",   choices = get_numeric_vars(df))
    updateSelectInput(session, "yvar",   choices = get_numeric_vars(df))
    updateSelectInput(session, "group",  choices = c("None", names(df)))
    updateSelectInput(session, "corr_x", choices = get_numeric_vars(df))
    updateSelectInput(session, "corr_y", choices = get_numeric_vars(df))
  })
  
  # build a simple filter UI off the first categorical column, if one exists
  output$filter_ui <- renderUI({
    if (length(cat_choices) == 0) return(NULL)
    first_cat <- cat_choices[1]
    vals <- sort(unique(df[[first_cat]]))
    checkboxGroupInput("cat_filter", paste0("Filter ", first_cat),
                       choices = vals, selected = vals, inline = FALSE)
  })
  
  # apply the optional categorical filter
  data_react <- reactive({
    d <- df
    if (length(cat_choices) > 0 && !is.null(input$cat_filter)) {
      first_cat <- cat_choices[1]
      d <- d %>% filter(.data[[first_cat]] %in% input$cat_filter)
    }
    d
  })
  
  # alerts for incompatible combos (assignment allows occasional alerts)
  observeEvent(list(input$plot_type, input$xvar, input$yvar), {
    d <- data_react()
    
    if (input$plot_type == "Histogram") {
      if (!isTruthy(input$xvar) || !is.numeric(d[[input$xvar]])) {
        shinyalert("Check your choice", "Histogram needs a numeric X variable.", type = "warning")
      }
    }
    
    if (input$plot_type == "Scatter") {
      if (!isTruthy(input$xvar) || !isTruthy(input$yvar) ||
          !is.numeric(d[[input$xvar]]) || !is.numeric(d[[input$yvar]])) {
        shinyalert("Check your choices", "Scatter needs numeric X and numeric Y.", type = "warning")
      }
    }
  }, ignoreInit = TRUE)
  
  # ---- Plot (Explore) ----
  output$main_plot <- renderPlot({
    d <- data_react()
    req(input$xvar, input$plot_type)
    
    col_map <- NULL
    if (!is.null(input$group) && input$group != "None" && input$group %in% names(d)) {
      col_map <- input$group
    }
    
    if (input$plot_type == "Histogram") {
      validate(need(is.numeric(d[[input$xvar]]), "Histogram requires numeric X."))
      ggplot(d, aes(x = .data[[input$xvar]], fill = if (!is.null(col_map)) .data[[col_map]] else NULL)) +
        geom_histogram(bins = input$bins, position = "identity") +
        labs(x = input$xvar, y = "Count", fill = if (is.null(col_map)) NULL else input$group)
    } else {
      req(input$yvar)
      validate(
        need(is.numeric(d[[input$xvar]]), "Scatter requires numeric X."),
        need(is.numeric(d[[input$yvar]]), "Scatter requires numeric Y.")
      )
      ggplot(d, aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
                    color = if (!is.null(col_map)) .data[[col_map]] else NULL)) +
        geom_point(size = input$ptsize, alpha = input$ptalpha) +
        labs(x = input$xvar, y = input$yvar, color = if (is.null(col_map)) NULL else input$group)
    }
  })
  
  # ---- Table + Summary (Explore) ----
  output$data_table <- renderDT({
    datatable(data_react(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$summary_txt <- renderText({
    d <- data_react()
    lines <- c(
      sprintf("Rows: %s | Cols: %s", nrow(d), ncol(d)),
      if (isTruthy(input$xvar)) sprintf("X: %s (%s)", input$xvar, class(d[[input$xvar]])[1]) else NULL,
      if (input$plot_type == "Scatter" && isTruthy(input$yvar)) sprintf("Y: %s (%s)", input$yvar, class(d[[input$yvar]])[1]) else NULL,
      if (!is.null(input$group) && input$group != "None") sprintf("Group: %s", input$group) else NULL
    )
    paste(lines[!sapply(lines, is.null)], collapse = "\n")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() sprintf("shiny_hw_table_%s.csv", Sys.Date()),
    content  = function(file) write_csv(data_react(), file)
  )
  
  # ---- Correlation tab ----
  sampled <- eventReactive(input$corr_sample, {
    req(input$corr_x, input$corr_y)
    d <- df %>%
      dplyr::select(all_of(c(input$corr_x, input$corr_y))) %>%
      tidyr::drop_na()
    n <- min(input$corr_n, nrow(d))
    if (is.na(n) || n < 2) return(d[0, ])
    d %>% dplyr::slice_sample(n = n)
  })
  
  output$corr_scatter <- renderPlot({
    d <- sampled(); req(nrow(d) > 1)
    ggplot(d, aes(.data[[input$corr_x]], .data[[input$corr_y]])) +
      geom_point(alpha = 0.7) +
      labs(x = input$corr_x, y = input$corr_y)
  })
  
  output$corr_text <- renderText({
    d <- sampled(); req(nrow(d) > 1)
    r <- cor(d[[input$corr_x]], d[[input$corr_y]], use = "complete.obs")
    paste0("Correlation (Pearson r) between ", input$corr_x, " and ", input$corr_y, ": ", round(r, 3))
  })
}

# ---- Launch ----
shinyApp(ui, server)
