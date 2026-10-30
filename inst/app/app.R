# Shiny app for easyanalysis. Launch with easyanalysis::run_app().

library(shiny)

options(shiny.maxRequestSize = 30 * 1024^2)

no_group <- ".none"

ui <- fluidPage(
  titlePanel("easyanalysis: summary tables"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Dataset (.csv, .xlsx, .xls)",
                accept = c(".csv", ".xlsx", ".xls")),
      selectInput("x", "Group variable", choices = c("None (overall table)" = no_group)),
      selectizeInput("y", "Outcome variables", choices = NULL, multiple = TRUE),
      checkboxInput("mean", "Summarize continuous variables as mean ± SD", FALSE),
      checkboxInput("p", "Add p-values", FALSE),
      conditionalPanel(
        sprintf("input.p && input.x != '%s'", no_group),
        selectizeInput("parametric", "Always use a parametric test for",
                       choices = NULL, multiple = TRUE),
        helpText("By default each continuous variable's test is chosen from a",
                 "Shapiro-Wilk normality check across groups.")
      ),
      textInput("caption", "Caption (optional)"),
      textInput("span_header", "Spanning header (optional)"),
      downloadButton("download", "Download .docx")
    ),
    mainPanel(
      uiOutput("preview")
    )
  )
)

server <- function(input, output, session) {
  # read_dataset() is internal to the package, so reach it explicitly.
  data <- reactive({
    req(input$file)
    tryCatch(
      easyanalysis:::read_dataset(input$file$datapath, input$file$name),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })

  group_var <- reactive({
    if (is.null(input$x) || input$x == no_group) NULL else input$x
  })

  observeEvent(data(), {
    updateSelectInput(session, "x",
      choices = c("None (overall table)" = no_group, names(data())),
      selected = no_group
    )
  })

  observeEvent(list(data(), input$x), {
    y_choices <- setdiff(names(data()), group_var())
    updateSelectizeInput(session, "y",
      choices = y_choices,
      selected = intersect(input$y, y_choices),
      server = TRUE
    )
  })

  observeEvent(list(data(), input$y), {
    numeric_y <- Filter(function(v) is.numeric(data()[[v]]), input$y)
    updateSelectizeInput(session, "parametric",
      choices = numeric_y,
      selected = intersect(input$parametric, numeric_y)
    )
  })

  table <- reactive({
    df <- data()
    req(length(input$y) > 0)
    x <- group_var()

    tryCatch(
      easyanalysis::summary_table(
        x = x,
        y = input$y,
        data = df,
        p = isTRUE(input$p) && !is.null(x),
        caption = input$caption,
        span_header = if (nzchar(input$span_header)) input$span_header else NULL,
        mean = isTRUE(input$mean),
        parametric = input$parametric
      ),
      error = function(e) {
        validate(need(FALSE, paste("Could not build the table:", conditionMessage(e))))
      }
    )
  })

  output$preview <- renderUI({
    flextable::htmltools_value(table())
  })

  output$download <- downloadHandler(
    filename = function() paste0("summary_table_", Sys.Date(), ".docx"),
    content = function(file) {
      flextable::save_as_docx(table(), path = file)
    }
  )
}

shinyApp(ui, server)
