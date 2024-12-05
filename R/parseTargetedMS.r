#' importation function for targeted MS assays
#'
#' @param rawData data read from machine export such as TSV or xml.
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
#'
#' @import utils 
#' @import stringr
#' @importFrom crayon red blue yellow white green bold

parseTargetedMS <- function(rawData, optns = list()) {
  
  ######Empty Lines#########
  cat(paste("fusion:", nrow(rawData),
            "line(s) read\n"))
  
  fi <- !is.na(rawData$AnalyteName)
  rawData <- rawData[fi,]
  cat(paste("fusion:",
            sum(!fi),
            "empty line(s) removed\n"))
  
  #split up the analsis name for project, cohort and sampleID
  idx <- grep(pattern = "cal", x = tolower(rawData$AnalysisName))[1]
  pos <- strsplit(rawData[idx,"AnalysisName"], "_")[[1]]
  
  ######projectName######
  if("projectName" %in% names(optns)){
    rawData$projectName <- optns$projectName
  }else{
    rawData$projectName <- pos[1]
  }
  #######cohortName#######
  if("cohortName" %in% names(optns)){
    rawData$cohortName <- optns$cohortName
  }else{
    rawData$cohortName <- pos[2]
  }
  
  ########plateID##########
  if("platePosition" %in% names(optns)){
    platePosition <- optns$platePosition
  } else {
    platePosition <- which(grepl("p[0-9]+", pos))
    
    if(length(platePosition) == 0){
      platePosition <- which(grepl("PLASMA", pos))
    }
  }
  
  rawData$plateID <- sapply(rawData$AnalysisName, function(name) {
    # Split by "_"
    parts <- strsplit(name, "_")[[1]]
    return(parts[platePosition]) 
  })
  
  #############sampleMatrixType##########
  
  if("sampleMatrixType" %in% names(optns)){
    rawData$sampleMatrixType <- optns$sampleMatrixType
  }else{
    rawData$sampleMatrixType <- ifelse(grepl("PLA", rawData$AnalysisName), "PLA",
                                       ifelse(grepl("URI", rawData$AnalysisName), "URI",
                                              ifelse(grepl("SER", rawData$AnalysisName), "SER", NA)))
  }
  
  #NAs
  if(sum(is.na(rawData$sampleMatrixType)) > 0){
    print(paste0(unique(rawData$sampleMatrixType)," sampleMatrixType found"))
    
    #replace NAs
    rawData$sampleMatrixType <- unique(na.omit(rawData$sampleMatrixType))
    
    print(paste0(unique(rawData$sampleMatrixType)," replaced NA sampleMatrixTypes"))
  }
  
  #multiple types
  if(length(unique(rawData$sampleMatrixType)) > 1){
    print(paste0(unique(rawData$sampleMatrixType)," sampleMatrixType found"))
    
    t <- table(rawData$sampleMatrixType)
    m <- max(t)
    smt <- which(t == m)
    smt <- names(smt)
    
    #replace with majority sampleMatrixType 
    rawData$sampleMatrixType <- smt
    
    print(paste0(unique(rawData$sampleMatrixType)," replaced sampleMatrixTypes"))
  }
  ######number of compounds############
  compoundList <- unique(rawData$AnalyteName)
  numberOfCompounds <- length(compoundList)
  cat(paste("fusion:",
            numberOfCompounds,
            "compound(s) found\n"))
  
  #######number of samples############
  cat(paste("fusion:",
            length(unique(rawData$AnalysisName)),
            "sample(s) found\n"))
  
  ###### looking for duplicated lines########
  idx <- which(duplicated(rawData[, which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]) |
                 duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))], fromLast = TRUE))
  
  if (length(idx) > 0) {
    cat(crayon::red("fusion: " %+%
                      crayon::red(idx) %+%
                      crayon::red(" duplicated line(s) found")),
        fill = TRUE)
  }
  idx <- which(duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]))
  
  if (length(idx) > 0) {
    rawData <- rawData[-idx,]
    cat(crayon::white("fusion: " %+%
                        crayon::white(idx) %+%
                        crayon::white(" duplicated line(s) removed")),
        fill = TRUE)
  }
  
  #############sampleID########
  #get sampleID position in title
  if ("codePosition" %in% names(optns)) {
    codePosition <- optns$codePosition
  } else {
    codePosition <- grep(pattern = "cal", x = tolower(pos))
  }
  
  #double blanks, LTRs, QC and CAL need further individualizing since they are not unique yet.
  rawData$sampleID <- sapply(rawData$AnalysisName, function(name) {
    # Split by "_"
    parts <- strsplit(name, "_")[[1]]
    
    # Check if "Blank", "LTR", "QC" or "CAL" is in the split parts. this covers sltr, vltr and pqc as is 
    if(any(grep("blank|ltr|qc|cal|chk|chck|check|sb", tolower(parts)))){
      
      # Find the position of "Blank" or "LTR" and take the part with the number after it
      idx <- which(grepl("blank|ltr|qc|cal|chk|chck|check|sb", tolower(parts)))
      
      return(paste(parts[idx], parts[idx + 1], sep = "_"))  # Combine "Blank" or "LTR" with the next number
    } else {
      # For all other cases, use the default code extraction
      return(parts[codePosition]) 
    }
  })
  
  #NA sampleIDs
  idx <- which(is.na(rawData$sampleID))
  noSampleID <- paste(unique(rawData[idx, "AnalysisName"]), collapse = ", ")
  if(length(idx) > 0){
    cat(crayon::red("The following AnalysisName have no sampleID and were not processed:: " %+%
                      crayon::red(noSampleID)))
    
    notProcessed <- rawData[idx,]
    rawData <- rawData[-idx,]
  }
  
  #clean up the names
  rawData$sampleID <- cleanNames(rawData$sampleID)
  
  #############sampleType##########
  #rename column correctly
  colnames(rawData)[which(colnames(rawData) == "SampleType")] <- "sampleType"
  
  #assign correct type
  idx <- grep("blank|sb", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "blank"
  
  idx <- grep("chk|chck|check", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "check"
  
  idx <- grep("sltr", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "sltr"
  
  idx <- grep("^ltr", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "ltr"
  
  idx <- grep("^pqc", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "pqc"
  
  idx <- grep("^qc", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "qc"
  
  idx <- grep("^cal", tolower(rawData$sampleID))
  rawData$sampleType[idx] <- "cal"
  
  idx <- grep("sltr|^ltr|^pqc|^qc|^cal|blank|chk|chck|check|sb", tolower(rawData$sampleID), invert = T)
  rawData$sampleType[idx] <- "sample"
  
  rawData$sampleType <- as.factor(rawData$sampleType)
  
  sample_types <- paste(levels(rawData$sampleType), collapse = ", ")
  
  cat(bold(blue("The following sample types were found: ")) %+% 
        bold(blue(sample_types)), fill = TRUE)
  
  #NA sampleTypes
  idx <- which(is.na(rawData$sampleType))
  if(length(idx) > 0){
    cat(paste("The following ", length(idx), " sample(s) have no sampleType:", rawData[idx, "AnalysisName"]))
  }
  
  return(rawData)
}
