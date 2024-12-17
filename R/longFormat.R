#' longFormat
#' 
#' Takes data that has been parsed and tidied and puts it into a long format for
#' database storage. 
#' @param rawData data in wide format
#' @return data frame in long format. "sampleID", "AnalyteName", "AnalysisName",
#' "cohortName","projectName","sampleMatrixType", "sampleType" are still columns 
#' but the rest are stored in paramName and paramValue.
#' @export
#' @import reshape2

longFormat <- function(rawData){
  #########long format###########
  fixed_columns <- c("sampleID", 
                     "AnalyteName", 
                     "AnalysisName",
                     "cohortName",
                     "projectName",
                     "sampleMatrixType",
                     "sampleType")
  
  varying_columns <- setdiff(names(rawData), fixed_columns)
  
  rawData <- reshape(rawData, 
                     varying = varying_columns,
                     v.names = "paramValue",
                     timevar = "paramName",
                     times = varying_columns,
                     direction = "long",
                     idvar = fixed_columns)
  
  rownames(rawData) <- 1:(nrow(rawData))
  
  return(rawData)
}
