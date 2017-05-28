max_plots <- 30
library(ASSRFourier)

shinyServer(function(input, output) {


  output$pdflink <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      inFiles <- input$files
      paths <- inFiles$datapath
      names <- sub(".csv", "", inFiles$name)
      freq <- input$range

      ASSRFourier::GenerateReport(paths,freq=freq, title=names,
                                 plot_all = input$show_plot_all,
                                 plot_single = input$show_plots)

      file.copy("report.pdf", file)
    }
  )
  output$plot_all <- renderPlot({
    inFiles <- input$files
    if (is.null(inFiles)|| input$show_plot_all==FALSE)
      return(NULL)
    path <- inFiles$datapath
    name <- sub(".csv", "", inFiles$name)
    freq <- input$range
    plot <- ASSRFourier::FouierCSVToPlot(path, freq=freq,title="", names=inFiles$name)
    return(plot)
  })

  output$plots <- renderUI({
    #loading files
      inFiles <- input$files
      if (is.null(inFiles) || input$show_plots==FALSE)
        return(NULL)
    #number of files
      max_plots <- nrow(inFiles)

    plot_output_list <- lapply(1:max_plots, function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname)
    })
    #note display
    output$note <- renderText({
      if(input$show_plots==TRUE) return("Note that plots below have different y-axis.")
      return("")
    })
    # Converting the list to a tagList.
    do.call(tagList, plot_output_list)
  })



    # Without `local`, the value of i in the renderPlot() will be the same across
    # all instances, because of when the expression is evaluated.
  for (i in 1:max_plots) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")

        output[[plotname]] <- renderPlot({
          inFiles <- input$files
          path <- inFiles[[my_i, 'datapath']]
          name <- sub(".csv", "", inFiles[[my_i, 'name']])
          freq <- input$range
          plot <- ASSRFourier::FouierCSVToPlot(path, freq=freq, title=name)
          return(plot)
        })


      })
  }

})
