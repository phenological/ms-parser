#' readBA
#' Read bile acids from TSV or xml
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
#' @return data frame in long format
#' 
#' @export
#' @import utils
#' @import crayon
#' @import stats
#' @import stringr

readBA <- function(file, optns = list()){
  
  ####read in the file####
  if (grepl("\\.txt$", file, ignore.case = TRUE)) {
    rawData <- readTXT(file)
  }
  
  if (grepl("\\.xml$", file, ignore.case = TRUE)) {
    rawData <- readXML2(file)
  }
  
  if (grepl("\\.tsv$", file, ignore.case = TRUE)) {
    rawData <- read.delim2(file = file,
                           fileEncoding = "latin1",
                           header = TRUE,
                           check.names = FALSE)
  }
  
  #####colnames#######
  rawData <- columnTranslation(file = file, rawData = rawData)
  ####parse####
  rawData <- parseTargetedMS(rawData = rawData, optns = optns)
  
  ######AnalyteName#########
  #internal standards only require [IS] confirmed by MS manager
  if (grepl("\\.xml$", file, ignore.case = TRUE)) {
  rawData$AnalyteName <- str_trim(str_replace_all(rawData$AnalyteName, c("-D4" = " [IS]",
                                                                         "-d5" = "[IS]",
                                                                         "\\." = "-",
                                                                         "_" = "-",
                                                                         "Acid" = "acid",
                                                                         "a-" = "alpha ",
                                                                         "b-" = "beta ",
                                                                         "g-" = "gamma ",
                                                                         "," = "",
                                                                         "\\bD[0-9]+\\b" = "",          
                                                                         "\\b[0-9]+C[0-9]+\\b" = "")))
  
  rawData$AnalyteName <- str_replace_all(rawData$AnalyteName, c("-" =  "", 
                                                                "Glycochnodeoxycholic" = "Glycochenodeoxycholic"))
  }
  
  
  if (grepl("\\.xml$", file, ignore.case = TRUE)) {
    rawData$AnalyteName <- str_trim(str_replace_all(rawData$AnalyteName, c("-d4" = "",
                                                                           "\\." = "-",
                                                                           "_" = "-",
                                                                           "Acid" = "acid",
                                                                           "a-" = "alpha ",
                                                                           "b-" = "beta ",
                                                                           "g-" = "gamma ",
                                                                           "," = "",
                                                                           "\\bD[0-9]+\\b" = "",          
                                                                           "\\b[0-9]+C[0-9]+\\b" = "")))
    
    rawData$AnalyteName <- str_replace(
      string = rawData$AnalyteName,
      pattern = "^HDCA$",  # Matches only the string "HDCA" exactly
      replacement = "Hyodeoxycholic acid (HDCA)"
    )
  }
  
  #########long format###########
  rawData <- longFormat(rawData = rawData)
  
  return(rawData)
}


