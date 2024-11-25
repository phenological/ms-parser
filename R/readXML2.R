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
  
  ####Acquisition Date#####
  merged_data$`Acquisition Date` <- as.POSIXct(
                                      paste(merged_data$createdate, merged_data$createtime), 
                                      format = "%d-%b-%y %H:%M:%S", 
                                      tz = "Australia/Perth"
                                    )
  ####rsquared####
  #get the rsquared values
  rsquared_nodes <- xml_find_all(xml, "//GROUPDATA/GROUP/CALIBRATIONDATA/COMPOUND/CURVE/DETERMINATION")
  rsquared_values <- xml_attr(rsquared_nodes, "rsquared")
  
  #get the "name" of the compounds from /GROUPDATA/GROUP/CALIBRATIONDATA/COMPOUND/@name
  name_nodes <- xml_find_all(xml, "//GROUPDATA/GROUP/CALIBRATIONDATA/COMPOUND")
  name_values <- xml_attr(name_nodes, "name")
  
  #use /GROUPDATA/GROUP/CALIBRATIONDATA/COMPOUND/CURVE/@type instead with only entries that say "linear" not "RF", they have rsquared values so need to match up 
  response_nodes <- xml_find_all(xml, "//GROUPDATA/GROUP/CALIBRATIONDATA/COMPOUND/CURVE")
  type_values <- xml_attr(response_nodes, "type")
  
  names <- data.frame(
    name = name_values,
    type = type_values,
    stringsAsFactors = FALSE
  )
  
  #those that say "linear" in type are the compounds with rsquared values (most are not IS but on occasion there are)
  idx <- which(grepl(pattern = "linear", x = tolower(names$type)))
  names <- names[idx,]
  
  # Combine into a data frame to align rsquared with the analytes
  calibration <- data.frame(
    name = names[,"name"],
    rsquared = rsquared_values,
    stringsAsFactors = FALSE
  )
  
  #add rsquared to the merged data
  merged_data$rsquared <- calibration$rsquared[match(merged_data$AnalyteName, calibration$name)]
  
  return(merged_data)
}


