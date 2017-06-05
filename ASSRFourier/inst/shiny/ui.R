library(shiny)

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
      checkboxInput("sqrt_trans", "Square root transformations", value = TRUE),
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
