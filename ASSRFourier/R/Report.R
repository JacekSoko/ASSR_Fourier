#' @title Generating reports form files
#'
#' @description Function \code{GenerateReport} generates report from CSV files with format... see....
#'
#' @param data.path vector of paths to files.
#' @param freq frequencies to show on x axis
#' @param title a vecor of titles of the plots
#' @param plot_all show plot with all samples
#' @param plot_single show single plots
#'
#' @return A pdf report.
#'
#' @importFrom rmarkdown render
#'
#' @export
GenerateReport <- function(data.path, freq=c(400,4100), title=NA, plot_all=TRUE, plot_single=FALSE){
  directory <- getwd()

  environment <- new.env()

  environment$data.path <- data.path
  environment$freq <- freq
  environment$title <- title
  environment$plot_all <- plot_all
  environment$plot_single <- plot_single



  render(paste0(path.package("ASSRFourier"), "/templates/report.Rmd"),
                    "pdf_document", output_file = paste0(directory, "/report.pdf"),
                    envir = environment)
}
