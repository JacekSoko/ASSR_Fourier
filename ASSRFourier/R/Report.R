#' @title Generating reports form files
#'
#' @description Function \code{GenerateReport} generates report from CSV files with format... see....
#'
#' @param data.path vector of paths to files.
#' @param freq frequencies to show on x axis
#' @param title a vecor of titles of the plots
#' @param plot.all show plot with all samples
#' @param plot.single show single plots
#' @param log.trans square root axist transformation
#' @param names names for plot legend
#'
#' @return A pdf report.
#'
#' @importFrom rmarkdown render
#'
#' @export
GenerateReport <- function(data.path, freq=c(400,4100), title=NA, plot.all=TRUE, plot.single=FALSE, names=NULL, log.trans=TRUE){
  directory <- getwd()

  environment <- new.env()

  environment$data.path <- data.path
  environment$freq <- freq
  environment$title <- title
  environment$plot.all <- plot.all
  environment$plot.single <- plot.single
  environment$log.trans <- log.trans



  render(paste0(path.package("ASSRFourier"), "/templates/report.Rmd"),
                    "pdf_document", output_file = paste0(directory, "/report.pdf"),
                    envir = environment)
}
