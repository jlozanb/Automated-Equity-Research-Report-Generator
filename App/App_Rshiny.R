# ································
# Report Generator - R Shiny APP:
# ································

library(shiny)
library(shinythemes)
library(rmarkdown)
library(bigrquery)
library(gargle)
library(DBI)
library(shinyjs)

# 1. CodeAPP:
# Definir datos de ejemplo para la tabla de Tickers
ticker_data <- data.frame(
  Ticker = c("AAPL", "AMZN", "SBUX", "BAC", "MA", "DDOG", "NFLX", "NVDA", "LLY", "MSFT", "NET"),
  Company = c("Apple Inc.", "Amazon Inc.", "Starbucks Corporation", "Bank of America Corporation",
             "Mastercard Inc.", "Datadog Inc.", "Netflix Inc.", "Nvidia Corporation",
             "Eli Lilly and Company", "Microsoft Corporation", "Cloudflare Inc."),
  Sector = c("Technology", "E-commerce", "Food and beverages", "Financial services",
             "Financial services", "Technology", "Entertainment", "Technology",
             "Pharmaceutical", "Technology", "Technology"))

# Interfaz de usuario (UI):
ui <- navbarPage(
  title = "Bullish Reports - Beta",
  theme = shinytheme("flatly"),
  tabPanel(
    "Home",
    fluidPage(
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "jumbotron",
          tags$h2("Welcome to Bullish Reports!", style = "color: #007bff; font-size: 18px; text-align: center"), # Cambio de color para resaltar
          tags$p(
            "Welcome to our Beta phase app. Here you will find a new way to make investment decisions in stocks through personalized financial reports.",
            style = "font-size: 16px; text-align: justify"
          ),
          tags$p(
            "Explore our main functionality, report generation, and discover how we can help you make more informed decisions in your investments.",
            style = "font-size: 16px; text-align: justify"
          )
        ),
        tags$div(
          class = "jumbotron",
          tags$h3("Who are we?", style = "color: black; background-color: #f8f9fa; padding: 10px; font-size: 20px; text-align: center"), # Título de la pregunta con fondo de color
          tags$p("We are a Beta phase application that will enhance the way you make investment decisions in stocks through personalized financial reports.", style = "font-size: 14px; text-align: justify"), # Texto actualizado
        ),
        tags$div(
          class = "jumbotron",
          tags$h3("What is our mission?", style = "color: black; background-color: #dee2e6; padding: 10px; font-size: 20px; text-align: center"), # Título de la pregunta con fondo de color
          tags$p("Our goal is to provide access to data and offer financial analysis to investors, both individuals and financial professionals, making it easier for them to make informed decisions.", style = "font-size: 14px; text-align: justify"), # Texto actualizado
        ),
        tags$div(
          class = "jumbotron",
          tags$h3("What does the Beta entail?", style = "color: black; background-color: #f8d7da; padding: 10px; font-size: 20px; text-align: center"), # Título de la pregunta con fondo de color
          tags$p("The Beta consists of an example of our website's main functionality. It allows you to generate a predefined report in PDF format, collecting only the information or analysis of a NYSE company that each user considers most important, and saving it in PDF format.", style = "font-size: 14px; text-align: justify"), # Texto actualizado
        ),
        tags$div(
          class = "jumbotron",
          tags$h4("Example:", style = "color: black; background-color: #d4edda; padding: 10px; font-size: 20px; text-align: center"), # Subtítulo de ejemplo con fondo de color
          tags$p("A user will be able to gather information from the Income Statement but will exclude dividend data. They can select the ratios they prefer and disregard other data that may cloud their analysis.", style = "font-size: 14px; text-align: justify"), # Texto actualizado
          tags$p("Stop seeing data and analysis you don't need, focus on your own.", style = "font-size: 14px; margin-top: 10px; text-align: justify"), # Texto actualizado con espacio adicional
        ),
        tags$div(
          class = "jumbotron",
          tags$h3("Who can access the final version?", style = "color: black; background-color: #cce5ff; padding: 10px; font-size: 20px; text-align: center"), # Título de la pregunta con fondo de color
          tags$p("Anyone interested in accessing this service can do so. Simply add their email in the 'Registration' tab to reserve a 100% free access in this Beta version.", style = "font-size: 14px; text-align: justify"), # Texto actualizado
        )
      )
    ) 
  ),
  
tabPanel(
    "Generate Report",
    fluidPage(
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "jumbotron",
              tags$h2("Generate Report", style = "color: #3F729B; font-weight: bold; margin-top: 52px"),
              selectInput("Ticker_input", 
                          label = "Select the company's symbol:", 
                          choices =  c("AAPL", "AMZN", "SBUX", "BAC", "MA", "DDOG", "NFLX", "NVDA", "LLY", "MSFT", "NET"),
                          selected = "AAPL",
                          multiple = FALSE,
                          selectize = TRUE,
                          width = "100%"),
              actionButton("generateReport", "Generate Report",
                           style = "background-color: #5BC0DE; color: #fff; border: none; padding: 10px 20px; border-radius: 4px; cursor: pointer; margin-top: 10px;"),
              uiOutput("accessReportLink", style = "font-size: 16px;"),
              tags$h3(
                "How does it work?",
                style = "font-weight: bold; color: #3F729B; margin-top: 30px; margin-bottom: 10px; font-size: 20px "
              ),
              tags$p(
                tags$ul(
                  tags$li("Select the company's ticker to generate a report.", style = "font-size: 14px"),
                  tags$li("In this Beta version, the reports will not be in real time.", style = "font-size: 14px"),
                ),
                style = "font-size: 12px; margin-bottom: 30px;"
              )
            )
          ),  
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "jumbotron",
              h3("List of Companies:", style = "color: #3F729B; text-align: center; font-weight: bold; font-size: 20px; text-align: left; margin-top: 20px; margin-bottom: 10px; text-align: justify"),
              tableOutput("tickerTable"),
              style = "font-size: 12px" # Cambia el tamaño del texto de la tabla a 12px
            )
          )
        )
      )
    )
  )

server <- function(input, output, session) {
  
  informe_generado <- reactiveVal(FALSE)  # Variable reactiva para controlar si el informe ha sido generado
  
  observeEvent(input$generateReport, {
    # Obtener el Ticker ingresado por el usuario
    Ticker <- input$Ticker_input
    
    cat("Checking PDF availability...\n")
    
    pdf_file <- paste0("www/Reports/", Ticker, "_Report.pdf")
    
    # Verificar si el archivo PDF existe en la carpeta 'www/Reports/'
    if (file.exists(pdf_file)) {
      # Mostrar el mensaje modal con el enlace al PDF existente
      output_url <- paste0("Reports/", Ticker, "_Report.pdf")
      showModal(
        modalDialog(
          title = tags$h4("Report Available", style = "color: #3F729B"),
          HTML(paste("The report is available. You can access it <a id='pdf_link' href='", output_url, "' target='_blank' style='color: #5BC0DE;'>here</a>.")),
          footer = NULL,
          easyClose = TRUE,
          size = "l"
        )
      )
      informe_generado(TRUE)  # Actualiza la variable reactiva para indicar que el informe ha sido generado
    } else {
      # Mostrar un mensaje de error si el archivo no existe
      showModal(
        modalDialog(
          title = "Error",
          "The report PDF does not exist. Please check the file name and try again.",
          easyClose = TRUE
        )
      )
      informe_generado(FALSE)  # Actualiza la variable reactiva para indicar que el informe no ha sido generado
    }
  })
  
  # Renderiza el enlace "Acceder al informe" debajo del botón de generar informe solo cuando el informe haya sido generado
  output$accessReportLink <- renderUI({
    if (informe_generado()) {
      tags$div(
        tags$a(
          href = paste0("Reports/", input$Ticker_input, "_Report.pdf"),
          target = "_blank",
          "Access the report",
          style = "color: #5BC0DE; margin-top: 10px; display: block;"
        )
      )
    }
  })
  
  output$tickerTable <- renderTable({
    ticker_data
  })
}

# Aplicación Shiny:
shinyApp(ui = ui, server = server)
