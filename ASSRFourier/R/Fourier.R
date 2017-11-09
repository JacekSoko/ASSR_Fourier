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
  names <- gsub(data.paths, pattern =".*ASSR_", replacement = "")
  names <- gsub(names, pattern =".csv", replacement = "")
  names(dat) <- names
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
  if(is.null(names)) names <- names(data)
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
#' @param log.trans log axist transformation
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
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom dplyr filter
#' @importFrom reshape2 melt
#'
#'
#' @export

FourierDraw <- function(data.means, freq=c(400,5000), title="", log.trans=TRUE){
  dat.means.s <- filter(data.means, Frequency < freq[2], Frequency > freq[1])
  data_long <- melt(dat.means.s, id.vars=c("Frequency"))
  colnames(data_long) <- c("Frequency", "names", "avg")

  plot <- ggplot(data_long, aes(Frequency, avg, color=names, shape=names)) +
    geom_line() +
    #geom_point(data=data_long, mapping=aes(Frequency, avg, color=names,fill=names), size=3, shape=21)+
    geom_point(size=3) +
    scale_shape_manual(values=LETTERS)+
    ggtitle(title) +
    theme_bw()+
    theme(legend.title =element_blank(),
          legend.position="top")
  if(log.trans)  plot <- plot +
                            scale_x_continuous(breaks = c(100,200,500,1000,2000,3000,4000,5000)) +
                            coord_trans(x = "log", y="log")

  if(ncol(data.means)==2){plot <- plot + theme(legend.position = "none")}
  return(plot)
}

#' @title Draw a FourierPlot using plotly
#'
#' @description Function \code{FouierPlotPlotly} draws a plot.
#'
#' @param data.means data.frame with means for frequencies
#' @param freq frequencies to show on x axis
#' @param title title of the plot
#' @param log.trans log axist transformation
#'
#' @return An object of class ggplot.
#'
#' @importFrom dplyr filter
#' @importFrom reshape2 melt
#' @importFrom plotly plot_ly
#'
#'
#' @export

FourierDrawPlotly <- function(data.means, freq=c(400,5000), title="", log.trans=TRUE){
  dat.means.s <- filter(data.means, Frequency < freq[2], Frequency > freq[1])
  data_long <- melt(dat.means.s, id.vars=c("Frequency"))
  colnames(data_long) <- c("Frequency", "names", "avg")

 plotl <-   plot_ly(data = data_long, x = ~Frequency, y = ~avg, color = ~names,
             type = 'scattergl', mode = 'lines+markers') %>%
      layout(showlegend = FALSE, title = title)

 if(log.trans) plotl <- layout(plotl, xaxis = list(type = "log", tickvals = c(100,200,500,1000,2000,3000,4000,5000)))

  return(plotl)
}




#' @title Draw a plot from CSV file
#'
#' @description Function \code{FouierCSVToPlot} draws a plot.
#'
#' @param data.path path to file
#' @param dir directory with files
#' @param freq frequencies to show on x axis
#' @param title title of the plot
#' @param names names for plot legend
#' @param log.trans square root axist transformation
#' @param plotly if TRUE plot be generated in plotly
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
FouierCSVToPlot <- function(data.path=NULL, dir=NULL , freq=c(400,5000), title="", names=NULL, log.trans=TRUE, plotly = FALSE){
  if(!is.null(dir)){
  files <- list.files(dir, full.names = TRUE)
  names <- gsub(files, pattern =".*/", replacement = "")
  names <- gsub(names, pattern =".csv", replacement = "")
  title = tail(strsplit(dir, split="/")[[1]], 1)
  }else{
    if(is.null(names)){
      names <- gsub(data.path, pattern =".*/", replacement = "")
      names <- gsub(names, pattern =".csv", replacement = "")
    }
  }
    if(plotly == FALSE){
      p <- FourierReadCSV(data.path,dir) %>% FourierMeans(names=names) %>% FourierDraw(freq,title, log.trans=log.trans)
    }else{
      p <- FourierReadCSV(data.path,dir) %>% FourierMeans(names=names) %>% FourierDrawPlotly(freq,title, log.trans=log.trans)
    }
  }

#' @title Saving plots from Mean Data
#'
#' @description Function \code{FouierAggrDir} saves plots from data generated by \code{FourierMeans} function.
#'
#' @param data data generated by \code{FourierMeans} function
#' @param freq frequencies on x axis
#' @param log.trans square root axis
#'
#' @return Saved plot
#'
#' @importFrom ggplot2 ggsave
#'
#' @export
FourierAggregatePlot <- function(data, freq=c(400,5000), log.trans=TRUE){
  names <- names(data)
  for(i in 1:length(data)){
    plot <- FourierDraw(data[[i]], freq=freq, log.trans=log.trans, title=names[i])
    ggsave(paste0(names[i],".png"), plot = plot, width = 60, height=30, units = "cm")
  }
}

#' @title Aggregatiing data from csv files
#'
#' @description Function \code{FouierAggr} aggregates data and generate plots.
#'
#' @param dir directory with folders related to patients
#' @param saving data as .rda
#' @param name rda file name if NULL gets folder name
#' @param plots Drawing plots TRUE/FALSE
#' @param freq frequencies on x axis for plotting
#' @param log.trans square root axist transformation for plotting
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




FourierAggr <- function(dir, save=FALSE, name=NULL, plots=TRUE, freq=c(400,5000), log.trans=TRUE){
  files <- list.files(dir, full.names = TRUE)
  profiles <- list()
  #csv
    data.path <- files[which(substr(files, nchar(files)-3, nchar(files))== ".csv")]
    if(length(data.path)>0){
    nam <- GetName(data.path[1])
    cat(nam,"\n")
    data <- FourierReadCSV(data.path)
    profiles[[nam]] <- FourierMeans(data)
    }
  #subfolders
    dirs <- files[which(substr(files, nchar(files)-3, nchar(files))!= ".csv")]
    for(i in 1:length(dirs)){
      dirs.files <- list.files(dirs[i], full.names = TRUE)
      nam <- GetName(dirs[i])
      cat(nam,"\n")
      data <- FourierReadCSV(dirs.files)
      profiles[[nam]] <- FourierMeans(data)
    }
  #saving
    if(save){
    if (is.null(name)) name<- gsub(dir, pattern =".*/", replacement = "")
    assign(name, profiles)
    save(list=c(name) , file=paste0(name,".rda"))
  #plotting
    if(plots) FourierAggregatePlot(profiles, freq, log.trans)
    }else{
      return(profiles)
    }
}





