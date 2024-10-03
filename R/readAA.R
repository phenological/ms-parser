#' Read amino acid data from tsv's
#'
#' @param file - the file path to be imported
#' @param optns list of options
#' \itemize{
#'    \item codePosition - position of the code in the file name. Default is 
#'    where cal is found in the analysis name.
#' }
#' @return rawData read from TSV file
#'
#' @export
#' @import utils
#' @import crayon
#' @importFrom reshape2 dcast

readAA <- function(file, optns = list()) {

####read in the file####

    rawData <- read.delim2(file = file,
                           fileEncoding = "latin1",
                           header = TRUE,
                           check.names = FALSE)

    if (nrow(rawData) == 0 || ncol(rawData) == 0) {
      warning(paste("file", file, "is empty"))
      return(invisible(NULL))  # Return invisible NULL to allow further execution
    }
    
    cat(paste("fusion:", nrow(rawData),
              "line(s) read\n"))
    
    fi <- !is.na(rawData$AnalyteName)
    rawData <- rawData[fi,]
    cat(paste("fusion:",
              sum(!fi),
              "empty line(s) removed\n"))
    
    ####### get sampleID position in title#######
    if ("codePosition" %in% names(optns)) {
      codePosition <- optns$codePosition
    } else {
      idx <- grep(pattern = "cal", x = tolower(rawData$AnalysisName))[1]
      pos <- strsplit(rawData[idx,"AnalysisName"], "_")[[1]]
      codePosition <- grep(pattern = "cal", x = tolower(pos))
    }
    
    #####number of compounds############
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
    
    #############adding sampleID########
    #double blanks, LTRs, QC and CAL need further individualizing since they are not unique yet.
    
    rawData$sampleID <- sapply(rawData$AnalysisName, function(name) {
      # Split by "_"
      parts <- strsplit(name, "_")[[1]]
      
      # Check if "Blank", "LTR", "QC" or "CAL" is in the split parts. this covers sltr, vltr and pqc as is 
      if(any(grep("blank|ltr|qc|cal", tolower(parts)))){
        
        # Find the position of "Blank" or "LTR" and take the part with the number after it
        idx <- which(grepl("blank|ltr|qc|cal", tolower(parts)))
     
        return(paste(parts[idx], parts[idx + 1], sep = "_"))  # Combine "Blank" or "LTR" with the next number
      } else {
        # For all other cases, use the default code extraction
        return(parts[codePosition]) 
      }
    })
    
    #clean up the names
    rawData$sampleID <- cleanNames(rawData$sampleID)
    
    #############No blank sampleTypes#########
    idx <- which(is.na(rawData$sampleType))
    if(length(idx) > 0){
      cat(paste("The following ", length(idx), " sample(s) have no sampleType:", rawData[idx, "AnalysisName"]))
    }
    
    #############cleaning sampleType##########
    #rename column correctly
    colnames(rawData)[which(colnames(rawData) == "SampleType")] <- "sampleType"
    
    idx <- grep("blank", tolower(rawData$sampleID))
    rawData$sampleType[idx] <- "blank"
    
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
    
    idx <- grep("sltr|^ltr|^pqc|^qc|^cal|blank", tolower(rawData$sampleID), invert = T)
    rawData$sampleType[idx] <- "sample"
    
    rawData$sampleType <- as.factor(rawData$sampleType)
    
    sample_types <- paste(levels(rawData$sampleType), collapse = ", ")
    
    cat(bold(blue("The following sample types were found: ")) %+% 
          bold(blue(sample_types)), fill = TRUE)
    
    #############sampleMatrixType##########
    rawData$sampleMatrixType <- ifelse(grepl("PLA", rawData$AnalysisName), "PLA",
                                       ifelse(grepl("URI", rawData$AnalysisName), "URI",
                                              ifelse(grepl("SER", rawData$AnalysisName), "SER", NA)))
    
    # recasting to get data
    idx <- which(tolower(names(rawData)) %in% c("sampleid", "quantity", "analysisname", "analytename"))
    
    newData <-
      dcast(rawData[, idx],
            AnalysisName + sampleID ~ AnalyteName, 
            value.var = "Quantity")
    
    aa <- 
    list("data" = newData,
         "meta" = rawData)
    
    return(aa)
}
