library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Plots"),
  sidebarLayout(
    sidebarPanel(
      fileInput('files', 'Choose CSV Files',
                multiple = TRUE,
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      sliderInput("range", "Range:",
                  min = 0, max = 25550, value = c(400,5000), step=100),
      checkboxInput("show_plot_all", "Plot profiles", value = TRUE),
      checkboxInput("show_plots", "Plot single profiles", value = FALSE),
      checkboxInput("log_trans", "Log transformations", value = TRUE),
      downloadLink('pdflink', "Download pdf")
    ),
    mainPanel(
      br(),
      plotlyOutput("plot_all"),
      textOutput("note"),
      uiOutput("plots")
    )
  )
))
