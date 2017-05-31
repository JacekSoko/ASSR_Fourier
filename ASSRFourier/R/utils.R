#' Getting name of from file pattern
#'
#' @noRd
GetName <- function(path){
  nam <- gsub(path, pattern =".*/", replacement = "")
  nam <- gsub(nam, pattern ="_ASSR.*", replacement = "")
  return(nam)
}
