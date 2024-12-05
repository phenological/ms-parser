#' readddhN
#' 
#' Takes TSV, TXT or xml for ddhN.
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
#' @return data frame in long format
#' 
#' @export
#' @import utils
#' @import crayon
#' @import stats
#' @import stringr

readddhN <- function(file, optns = list()){
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
  
  #########long format###########
  rawData <- longFormat(rawData = rawData)
  
  return(rawData)
}