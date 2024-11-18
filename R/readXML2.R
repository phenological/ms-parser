#' readXML2
#'
#' Reads xml outputs for mass spectrometry. 
#' 
#' @param path character for path to file
#' @return data frame with no empty or all zero columns
#' @export
#' @import xml2
readXML2 <- function(path){
  xml <- read_xml(path)
  
  # retrieving sample information from SAMPLELISTDATA
  sample <- xml_children(xml_find_all(xml, "//GROUPDATA/GROUP/SAMPLELISTDATA"))
  sample <- data.frame(do.call("rbind", lapply(xml_attrs(sample), function(x) unlist(x))))
  
  sampleInfo <- sample[c("id",
                         "name",
                         "createdate",
                         "createtime",
                         "type",
                         "desc",
                         "stdconc",
                         "vial",
                         "inletmethodname",
                         "msmethodname",
                         "tunemethodname",
                         "instrument")]
  colnames(sampleInfo)[colnames(sampleInfo) == "name"] <- "AnalysisName"
  colnames(sampleInfo)[colnames(sampleInfo) == "type"] <- "sampleType"
  
  # retrieving data for each samples from SAMPLELISTDATA
  compound <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND"))
  COM <- data.frame(do.call("rbind", lapply(compound, function(x) unlist(x))))
  colnames(COM)[colnames(COM) == "name"] <- "AnalyteName"
  
  peak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK"))
  PEAK <- data.frame(do.call("rbind", lapply(peak, function(x) unlist(x))))
  
  ispeak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK/ISPEAK"))
  ISPEAK <- data.frame(do.call("rbind", lapply(ispeak, function(x) unlist(x))))
  colnames(ISPEAK) <- paste0("IS", colnames(ISPEAK))
  
  #bind together
  COMPOUND <- cbind(COM, PEAK, ISPEAK)
  
  #convert to proper NA
  COMPOUND[COMPOUND == "N/A" | COMPOUND == "n/a" | COMPOUND == "NaN"] <- NA
  
  #remove empty cols
  empty_cols <- sapply(COMPOUND, function(col) all(is.na(col) | col == ""))
  COMPOUND <- COMPOUND[, !empty_cols]
  
  #convert to numeric where needed
    for (col_name in names(COMPOUND)) {
      # Check if the column is character and can be coerced to numeric
      if (is.character(COMPOUND[[col_name]])) {
        # Try converting to numeric
        converted <- suppressWarnings(as.numeric(COMPOUND[[col_name]])) 
        
        # If conversion is successful (no NA introduced), replace the column
        if (all(!is.na(converted))) {
          COMPOUND[[col_name]] <- converted
        }
      }
    }
  
  COMPOUND$stdconc <- NULL
  
  #remove all zero cols
  zero_cols <- sapply(COMPOUND, function(col) all(col == 0 | col == 0.0, na.rm = TRUE))
  
  COMPOUND <- COMPOUND[, !zero_cols]
  
  #merge data
  merged_data <- merge(COMPOUND, sampleInfo, by.x = "sampleid", by.y = "id", all.x = TRUE)
  
  #remove some columns
  cols_to_remove <- c("id", "sampleid", "groupid", "vial")
  
  # Remove specified columns by name
  merged_data <- merged_data[, !(names(merged_data) %in% cols_to_remove)]
 
  
  return(merged_data)
}


