#' function to clean names for importation into databases
#' @param names - a name or an array of character strings.
#' @return clean name(s)
#' @examples
#' originalID <- c("ddd.aaa", "ddd uuu", "ddd+aaa", "ddd*yyy", "ddd#dd", "ddd_fff")
#' originalID <-c(originalID, "ddd$ddd", "ddd@ddd", "dd_aa", "dad*")
#' cleanNames(originalID)
#' @export

cleanNames <- function(names) {
  #######cleaning LTR and double blank sampleIDs########
  names <- gsub("PLASMA|SERUM|URINE|-PLA|-URI|-SER", "", names)
  names <- gsub("\\\\", " ", names)
  
  # first we remove trailing spaces
  names <- gsub("\\s+$", "", names)
  
  # second we remove spaces at the beginning of each lines
  names <- gsub("^\\s+", "", names)
  
  # third we remove double spaces
  names <- gsub("\\s+", " ", names)
  
  # last we curate from other weird characters
  names <- toupper(names)
  # names <- tolower(names)
  make.unique(names, sep = "#")
  names <- gsub("[*]$", "-S", names)
  names <- gsub("[*]", "T", names)
  names <- gsub("[+]", "P", names)
  
  # we remove all except # for replicates
  names <- gsub("[^A-Za-z0-9\\W#]", "-", names)
  
  names <- gsub("-+", "#", names)
  names <- gsub("^-", "", names)
  
  # last we remove trailing dashes
  names <- gsub("[-]*$", "", names)
  
  
  #LTR and SLTR
  #number LTR and SLTR
  # names <- sub("^(LTR#|SLTR#)\\d+", "\\1", names)
   names <- sub("LTR#.*", "LTR#", names)
  ltr_indices <- which(names == "LTR#")
  sltr_indices <- which(names == "SLTR#")
  # Create sequential numbers for each "LTR#"
  names[ltr_indices] <- paste0("LTR#", seq_along(ltr_indices))
  names[sltr_indices] <- paste0("SLTR#", seq_along(sltr_indices))
  
  #CAL and QC
  #if CAL or QC is immediately followed by a #, put a 0 there instead. only have 1 to 8 
  names <- sub("CAL#", "CAL0", names)
  names <- sub("QC#", "QC0", names)
  
  return(names)
}
