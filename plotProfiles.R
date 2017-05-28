#' @param dir directory to files with data
plotProfiles <- function(dir) {
  # read data
  files <- list.files(dir, full.names = TRUE)
  datasets <- lapply(files, function(file) {
    dat <- read.csv(file, header = FALSE, skip = 2, sep=";", dec=",")
    dat[1,2] <- "Freq"
    colnames(dat) <- dat[1,]
    dat <- gather(dat[-1,-1], time, value, -Freq)
    data.frame(dat, file = file)
  })
  #process data
  datasets <- do.call(rbind, datasets)
  datasets$file <- gsub(datasets$file, pattern =".*ASSR.", replacement = "")
  datasets$file <- gsub(datasets$file, pattern =".csv", replacement = "")
  
  datasets %>% 
    group_by(Freq, file) %>%
    summarise(avg = mean(value, na.rm=TRUE),
              sd = sqrt(var(value, na.rm=TRUE))) %>%
    ungroup() %>%
    mutate(Freq = as.numeric(as.character(Freq))) %>%
    filter(Freq > 100, Freq < 5001) ->
    datasetsAvgs
  
  #plot profiles
  title <- tail(strsplit(dir, split="/")[[1]], 1)
  ggplot(datasetsAvgs, aes(Freq, avg, color=file)) +
    geom_line() + geom_point() + scale_x_continuous(breaks = c(100,200,500,1000,2000,3000,4000,5000)) + 
    coord_trans(x = "sqrt", y="sqrt") +
    ggtitle(title) + theme_bw()
}


library(tidyr)
library(dplyr)
library(ggplot2)
plotProfiles("~/Dropbox/Biecek_Sokolowski/Pomiary_lepsze/Glinka_ASSR_Fourier/")


