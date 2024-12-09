
test_that("older AA sample names cleaned properly",{
  file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV"
  rawData <- read.delim2(file = file,
                         fileEncoding = "latin1",
                         header = TRUE,
                         check.names = FALSE)
  
  ####### get sampleID position in title#######
  
  idx <- grep(pattern = "cal", x = tolower(rawData$AnalysisName))[1]
  pos <- strsplit(rawData[idx,"AnalysisName"], "_")[[1]]
  codePosition <- grep(pattern = "cal", x = tolower(pos))
  
  ###### looking for duplicated lines########
  idx <- which(duplicated(rawData[, which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]) |
                 duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))], fromLast = TRUE))
  
  idx <- which(duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]))
  
  #############adding sampleID########
  #double blanks, LTRs, QC and CAL need further individualizing since they are not unique yet
  
  rawData$sampleID <- sapply(rawData$AnalysisName, function(name) {
    # Split by "_"
    parts <- strsplit(name, "_")[[1]]
    
    # Check if "Blank" or "LTR" is in the split parts
    if(any(grep("blank|ltr|qc|cal", tolower(parts)))){
      # if (any(grepl("Blank|LTR", parts))) {
      
      # Find the position of "Blank" or "LTR" and take the part with the number after it
      idx <- which(grepl("blank|ltr|qc|cal", tolower(parts)))
      # idx <- which(grepl("Blank|LTR", parts))
      return(paste(parts[idx], parts[idx + 1], sep = "_"))  # Combine "Blank" or "LTR" with the next number
    } else {
      # For all other cases, use the default code extraction
      return(parts[codePosition])  # Adjust 'codePosition' as needed
    }
  })
  
  length(unique(rawData$sampleID)) == length(unique(cleanNames(rawData$sampleID)))
  
  names <- unique(rawData$sampleID)
  #clean up the names
  cNames <- cleanNames(names)
  
  #no space in blank sampleID
  expect_true(length(grep("SINGLEBLANK", cNames)) >= 1)
  
  #the sampleIDs are unique
  expect_true(length(unique(cNames)) == length(unique(names)))
  
  #LTR is LTR#*
  expect_true(c("LTR#1" %in% cNames))

  #CAL is CAL0*
  expect_true(length(grep("CAL01", cNames)) >= 1) 
  
  #QC is QC0*
  expect_true(length(grep("QC01", cNames)) >= 1) 
  
})


test_that("newer AA sample names cleaned properly",{
  file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV"
  rawData <- read.delim2(file = file,
                         fileEncoding = "latin1",
                         header = TRUE,
                         check.names = FALSE)
  
  ####### get sampleID position in title#######
  
  idx <- grep(pattern = "cal", x = tolower(rawData$AnalysisName))[1]
  pos <- strsplit(rawData[idx,"AnalysisName"], "_")[[1]]
  codePosition <- grep(pattern = "cal", x = tolower(pos))
  
  ###### looking for duplicated lines########
  idx <- which(duplicated(rawData[, which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]) |
                 duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))], fromLast = TRUE))
  
  idx <- which(duplicated(rawData[,which(colnames(rawData) %in% c("AnalysisName", "AnalyteName"))]))
  
  #############adding sampleID########
  #double blanks, LTRs, QC and CAL need further individualizing since they are not unique yet
  
  rawData$sampleID <- sapply(rawData$AnalysisName, function(name) {
    # Split by "_"
    parts <- strsplit(name, "_")[[1]]
    
    # Check if "Blank" or "LTR" is in the split parts
    if(any(grep("blank|ltr|qc|cal", tolower(parts)))){
      # if (any(grepl("Blank|LTR", parts))) {
      
      # Find the position of "Blank" or "LTR" and take the part with the number after it
      idx <- which(grepl("blank|ltr|qc|cal", tolower(parts)))
      # idx <- which(grepl("Blank|LTR", parts))
      return(paste(parts[idx], parts[idx + 1], sep = "_"))  # Combine "Blank" or "LTR" with the next number
    } else {
      # For all other cases, use the default code extraction
      return(parts[codePosition])  # Adjust 'codePosition' as needed
    }
  })
  
  expect_true(length(unique(rawData$sampleID)) == length(unique(cleanNames(rawData$sampleID)))) 
  
  
  names <- unique(rawData$sampleID)
  #clean up the names
  cNames <- cleanNames(names)
  
  #the sampleIDs are unique
  expect_true(length(unique(cNames)) == length(unique(names)))
  
  #makes sure SLTR is maintained
  expect_true(length(grep("SLTR", cNames)) >= 1)
  
  #LTR is LTR#*
  expect_true(c("LTR#1" %in% cNames))
  
  #CAL is CAL0*
  expect_true(length(grep("CAL01", cNames)) >= 1) 
  
  #QC is QC0*
  expect_true(length(grep("QC01", cNames)) >= 1) 
  
})


