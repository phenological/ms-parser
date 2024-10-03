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

  make.unique(names, sep = "#")
  names <- gsub("[*]$", "-S", names)
  names <- gsub("[*]", "T", names)
  names <- gsub("[+]", "P", names)
  
  # remove all except # for replicates
  names <- gsub("[^A-Za-z0-9\\W#]", "-", names)
  
  names <- gsub("-+", "#", names)
  names <- gsub("^-", "", names)
  
  # last we remove trailing dashes
  names <- gsub("[-]*$", "", names)
  
  #######BLANKS##########
  #if there is single blank or double blank, make it one word 
  names <- sub("#BLANK", "BLANK", names)
  
  #######CAL and QC#######
  #if CAL or QC is immediately followed by a #, put a 0 there instead. only have 1 to 8 
  names <- sub("CAL#", "CAL0", names)
  names <- sub("QC#", "QC0", names)
  
  #########PQC##########
  
  ##########LTR#########
  idx <- grep("LTR", names)
  idx <- idx[!grepl("SLTR|VLTR", names[idx])]
  unique_ltrs <- unique(names[idx])
  
  # Create a mapping of original LTRs to new LTR#1, LTR#2, etc.
  new_ltr_mapping <- setNames(paste0("LTR#", seq_along(unique_ltrs)), unique_ltrs)
  
  # Replace only the LTRs, leave the others unchanged
  names[idx] <- new_ltr_mapping[names[idx]]
  
  #######SLTR########
  idx <- grep("SLTR", names)
  unique_sltrs <- unique(names[idx])
  
  # Create a mapping of original SLTRs to new SLTR#1, SLTR#2, etc.
  new_sltr_mapping <- setNames(paste0("SLTR#", seq_along(unique_sltrs)), unique_sltrs)
  
  # Replace only the SLTRs, leave the others unchanged
  names[idx] <- new_sltr_mapping[names[idx]]
  
  #######VLTR########
  idx <- grep("VLTR", names)
  unique_vltrs <- unique(names[idx])
  
  # Create a mapping of original SLTRs to new SLTR#1, SLTR#2, etc.
  new_vltr_mapping <- setNames(paste0("VLTR#", seq_along(unique_vltrs)), unique_vltrs)
  
  # Replace only the SLTRs, leave the others unchanged
  names[idx] <- new_vltr_mapping[names[idx]]
  
  return(names)
}
