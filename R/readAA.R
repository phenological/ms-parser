#' readAA
#' Read amino acid data from tsv's
#'
#' @param file - the file path to be imported
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
#' @return rawData read from TSV file
#'
#' @export
#' @import utils
#' @import crayon
#' @import stats
#' @import readr
#' @importFrom stringr str_replace_all
#' @importFrom reshape2 dcast

readAA <- function(file, optns = list()) {

####read in the file####
coding <- guess_encoding(file)

    rawData <- read.delim2(file = file,
                           fileEncoding = coding[[1]][1],
                           header = TRUE,
                           check.names = FALSE)

    if (nrow(rawData) == 0 || ncol(rawData) == 0) {
      warning(paste("file", file, "is empty"))
      return(invisible(NULL))  # Return invisible NULL to allow further execution
    }
    
    #####colnames######
    colnames(rawData) <- str_replace_all(colnames(rawData), c(";" = "",
                                                              "Area to height ratio indicates whether this is a real chromatographic peak." = "Area to height ratio"))
    ####parse####
    rawData <- parseTargetedMS(rawData = rawData, optns = optns)
    
    #####AnalyteName######
    #fix "Pheny alanine"
    rawData$AnalyteName <- str_replace_all(rawData$AnalyteName, c("Pheny alanine" = "Phenylalanine")) 
    
    #fix "4--hydroxyproline"
    rawData$AnalyteName <- str_replace_all(rawData$AnalyteName, c("4--hydroxyproline" = "4-hydroxyproline")) 
    
    #internal standards only requir [IS] and AccQTag does not need to be present in any names confirmed by MS manager
    rawData$AnalyteName <- str_replace_all(rawData$AnalyteName, c("-SIL" = "",
                                                                  "\\." = "-", 
                                                                   ".13C.." = "", 
                                                                   ".AccQTag" = "", 
                                                                   "15N." = "",
                                                                   "alpha-" = "alpha ",
                                                                   "beta-" = "beta ",
                                                                   "gamma-" = "gamma ",
                                                                  "," = ""))
   
    rawData$AnalyteName <- gsub("([^ ])\\[IS]", "\\1 [IS]", rawData$AnalyteName)
     
    #########long format###########
    rawData <- longFormat(rawData = rawData)
    
    return(rawData)
}
