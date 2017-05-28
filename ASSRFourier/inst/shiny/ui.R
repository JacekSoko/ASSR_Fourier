library(shiny)

shinyUI(fluidPage(
  titlePanel("Plots"),
  sidebarLayout(
    sidebarPanel(
      fileInput('files', 'Choose CSV File',
                multiple = TRUE,
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      sliderInput("range", "Range:",
                  min = 0, max = 25550, value = c(400,4100)),
      checkboxInput("show_plot_all", "All on one plot", value = TRUE),
      checkboxInput("show_plots", "Single plots", value = FALSE),
      downloadLink('pdflink', "Download pdf")
    ),
    mainPanel(
      br(),
      plotOutput("plot_all"),
      textOutput("note"),
      uiOutput("plots")
    )
  )
))
