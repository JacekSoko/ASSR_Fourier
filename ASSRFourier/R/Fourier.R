#' @title Reading csv file
#'
#' @description Function \code{FourierReadCSV} reads data from CSV file with format... see....
#'
#' @param data.paths path to file or vector of paths to files
#' @param dir directory with files
#'
#' @return An object of class data.frame, containing values of ... for specified frequencies.
#'
#' @importFrom utils read.csv2
#'
#' @export
FourierReadCSV <- function(data.paths = NULL, dir){
  if(is.null(data.paths)) data.paths <- list.files(dir, full.names = TRUE)
  dat <- list()
  for(i in 1:length(data.paths)){
  dat[[i]] <- read.csv2(data.paths[i], row.names = NULL, skip=2, header=T)[,-1]
  }
  return(dat)
}


#' @title Means for frequencies
#'
#' @description Function \code{FouierMeans} computes means for specified frequencies.
#'
#' @param data data.table or a list of data.tables to compute means for frequencies
#' @param names The names corresponding to the samples
#'
#' @return An object of class data.frame containing means for specified frequencies.
#'
#' @export
FourierMeans <- function(data, names=NULL){
  result <- data[[1]][,1]
  for(i in 1:length(data)){
    means <- rowMeans(data[[i]][,-c(1)], na.rm = TRUE)
    result <- as.data.frame(cbind(result,means))
  }
  colnames(result)[1] <- c("Frequency")
  if(!is.null(names)) colnames(result)[2:(length(data)+1)] <- names
  return(result)
}




#' @title Draw a plot
#'
#' @description Function \code{FouierPlotMany} draws a plot.
#'
#' @param data.means data.frame with means for frequencies
#' @param freq frequencies to show on x axis
#' @param title title of the plot
#' @param sqrt.trans square root axist transformation
#'
#' @return An object of class ggplot.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 coord_trans
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr filter
#' @importFrom reshape2 melt
#'
#'
#' @export

FourierDraw <- function(data.means, freq=c(400,4100), title="", sqrt.trans=TRUE){
  dat.means.s <- filter(data.means, Frequency < freq[2], Frequency > freq[1])
  data_long <- melt(dat.means.s, id.vars=c("Frequency"))
  colnames(data_long) <- c("Frequency", "names", "avg")

  plot <- ggplot(data_long, aes(Frequency, avg, color=names)) +
    geom_line() + geom_point() +
    ggtitle(title) +
    theme_bw()+
    theme(legend.title =element_blank())
  if(sqrt.trans)  plot <- plot +
                            scale_x_continuous(breaks = c(100,200,500,1000,2000,3000,4000,5000)) +
                            coord_trans(x = "sqrt", y="sqrt")

  if(ncol(data.means)==2){plot <- plot + theme(legend.position = "none")}
  return(plot)
}

#' @title Draw a plot from CSV file
#'
#' @description Function \code{FouierCSVToPlot} draws a plot.
#'
#' @param data.path path to file
#' @param freq frequencies to show on x axis
#' @param title title of the plot
#' @param names names for plot legend
#' @param sqrt.trans square root axist transformation
#'
#' @return An object of class ggplot.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 aes
#' @importFrom dplyr %>%
#'
#' @export
FouierCSVToPlot <- function(data.path, freq=c(400,4100), title="", names=NULL, sqrt.trans=TRUE){
  p <- FourierReadCSV(data.path) %>% FourierMeans(names=names) %>% FourierDraw(freq,title, sqrt.trans=sqrt.trans)
  }


