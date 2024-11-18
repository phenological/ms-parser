#' readTXT
#' Reads .txt outputs for mass spectrometry
#' @param path character for path to file
#' @return data frame with no empty or all zero columns
#' @export
#' @import utils

readTXT <- function(path){
  # Read the entire file into lines
  lines <- readLines(path)
  
  # Initialize a vector to store the compound names
  compound_names <- character(0)
  
  # Use regular expression to extract compound names
  compound_lines <- grep("^Compound", lines, value = TRUE)
  
  # Extract compound names using sub to remove "Compound X: " and capture the compound name
  compound_names <- sub("Compound \\d+:\\s*(.*)", "\\1", compound_lines)
  
  # Identify where the data rows start (usually after the last "Compound X:" line)
  data_start <- which(grepl("^Compound", lines))  # Find the first line containing "Compound"
  
  # Extract data starting from data_start to the end
  # data_lines <- lines[data_start:length(lines)]  # Get the data lines
  data_lines <- lines[data_start[1]:length(lines)]
  
  # Combine data lines into a single string to read them as a table
  data_text <- paste(data_lines, collapse = "\n")
  
  # Read the data into a data frame
  data <- read.table(text = data_text, header = FALSE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE,
                     col.names = c("Index", "NameIndex", "Name", "Type", "SampleText", "PercentDev",
                                   "StdConc", "Conc", "PrimaryFlags", "Area", "ISArea", "Response",
                                   "CoeffOfDetermination", "RT", "Vial"))
  
  # Now, for each compound, assign the compound name to all rows related to that compound
  compound_count <- length(compound_names)
  
  # Initialize the AnalyteName column with NAs
  data$AnalyteName <- NA
  
  # Variables for tracking the row positions
  current_row <- 1
  
  # Find all rows where 'Index' contains "Compound" followed by a number
  compound_rows <- grep("^Compound", data$Index)
  
  # Loop over the compound rows and assign their names to the relevant rows in the data
  for (i in 1:length(compound_rows)) {
    # Get the compound number (e.g., 35 from "Compound 35: Melatonin")
    compound_index <- gsub("^Compound \\d+: (.*)", "\\1", data$Index[compound_rows[i]])
    
    # Determine the next compound row (or the end of the data)
    next_compound_row <- if (i < length(compound_rows)) compound_rows[i + 1] - 1 else nrow(data)
    
    # Assign the compound name to the relevant rows in the data frame
    data$AnalyteName[current_row:next_compound_row] <- compound_index
    
    # Move to the next set of rows for the next compound
    current_row <- next_compound_row + 1
  }
  
  #remove rows that are totally empty or just have compound:: #
  data <- data[!grepl("Compound", data$Index) & data$Index != "" & !is.na(data$Index), ]
  return(data)
}


