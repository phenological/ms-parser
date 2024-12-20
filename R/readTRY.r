#' readTRY
#' 
#' @param file character file path
#' @param optns list of options
#' \itemize{
#'    \item codePosition - position of the code in the AnalysisName for sampleID. 
#'    Default is where cal is found in the AnalysisName.
#'    \item platePosition - position of the code in the AnalysisName for the plateID.
#'    The default will use the position prior to that used for sampleID (codePosition).
#'    \item projectName - the name of the project. For example "covid19". The default
#'    will take it from the first position in AnalysisName.
#'    \item cohortName - the name of the cohort. For example "harvardC2". The default 
#'    will take it from the second position in AnalysisName
#'    \item sampleMatrixType - sample matrix type, usually "PLA", "SER" or "URI". 
#'    The default will take it from the AnalysisName
#' }
#' @return a data frame in long format
#' @export
#' @import utils
#' @import crayon
#' @import stats
#' @import stringr
#' @import readr 
#' @importFrom reshape2 dcast

readTRY <- function(file, optns = list()){
  
  ####read in the file####
  if (grepl("\\.txt$", tolower(file), ignore.case = TRUE)) {
    rawData <- readTXT(file)
  }
  
  if (grepl("\\.xml$", tolower(file), ignore.case = TRUE)) {
    rawData <- readXML2(file)
  }
  
  if (grepl("\\.tsv$", tolower(file), ignore.case = TRUE)) {
    coding <- guess_encoding(file)
    
    rawData <- read.delim2(file = file,
                           fileEncoding = coding[[1]][1],
                           header = TRUE,
                           check.names = FALSE)
  }
  
  #####colnames#######
  rawData <- columnTranslation(file = file, rawData = rawData)
  ####parse####
  rawData <- parseTargetedMS(rawData = rawData, optns = optns)
  
  ######AnalyteName#########
  #internal standards only require [IS] confirmed by MS manager
  
  rawData$AnalyteName <- ifelse(grepl("SIL",  rawData$AnalyteName), 
                                paste0(rawData$AnalyteName, "[IS]"), 
                                rawData$AnalyteName)
  
  rawData$AnalyteName <- str_trim(str_replace_all(rawData$AnalyteName, c("SIL" = "",
                                                                         "\\." = "-",
                                                                         "13C215N" = "",
                                                                         "13C5" = "",
                                                                         "13C" = "",
                                                                         "alpha-" = "alpha ",
                                                                         "beta-" = "beta ",
                                                                         "gamma-" = "gamma ",
                                                                         "," = "",
                                                                         "\\bD[0-9]+\\b" = "",          
                                                                         "\\b[0-9]+C[0-9]+\\b" = "")))
  #########long format###########
  rawData <- longFormat(rawData = rawData)
  
  return(rawData)
}