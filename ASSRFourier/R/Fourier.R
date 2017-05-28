#' @title Reading csv file
#'
#' @description Function \code{FourierReadCSV} reads data from CSV file with format... see....
#'
#' @param data.path path to file
#'
#' @return An object of class data.frame, containing values of ... for specified frequencies.
#'
#' @importFrom utils read.csv2
#'
#' @export
FourierReadCSV <- function(data.path){
  dat <- list()
  for(i in 1:length(data.path)){
  dat[[i]] <- read.csv2(data.path[i], row.names = NULL, skip=2, header=T)
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
  if(class(data)=="data.frame") data <- list(data)
  result <- data[[1]][,2]
  for(i in 1:length(data)){
    means <- rowMeans(data[[i]][,-c(1,2)], na.rm = TRUE)
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
#'
#' @return An object of class ggplot.
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom reshape2 melt
#'
#'
#' @export

FourierDraw <- function(data.means, freq=c(400,4100), title=""){
  dat.means.s <- subset(data.means, (Frequency < freq[2]) & (Frequency > freq[1]))
  data_long <- melt(dat.means.s, id.vars=c("Frequency"))
  colnames(data_long) <- c("Frequency", "names", "avg")
  plot <- ggplot(data_long, aes(x=Frequency,y=avg, group=names, col=names)) +
    geom_line() +
    geom_point()+
    theme_bw()+
    ggtitle(title)

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
FouierCSVToPlot <- function(data.path, freq=c(400,4100), title=NA, names=NULL){
  p <- FourierReadCSV(data.path) %>% FourierMeans(names=names) %>% FourierDraw(freq,title)
  }


