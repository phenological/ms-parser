#' Column Name Translation Function
#'
#' This function translates column names in `rawData` based on the file type detected from the input file.
#' It uses the column name mappings from the JSON file.
#'
#' @param file A character string specifying the path to the file whose column names need to be translated.
#' @param rawData A data frame whose column names will be translated.
#' @return A data frame with the translated column names.
#' @export
columnTranslation <- function(file, rawData) {
  
  # Load the JSON translation file from the package's inst/extdata folder
  json_file <- system.file("extdata", "columnTranslation.json", package = "ms.parser")
  
  # If not found (i.e., in development mode), use the relative path
  if (json_file == "") {
    json_file <- file.path("inst", "extdata", "columnTranslation.json")
    json_file <- "~/git/phenological/ms-parser/inst/extdata/columnTranslation.json"
    if (!file.exists(json_file)) {
      stop("Translation JSON file not found in either installed or development modes.")
    }
  }
  
  # Read the JSON file into a list
  translation_data <- jsonlite::fromJSON(json_file)
  
  # Assuming translation_data is a data frame with columns: xml, tsv, txt
  translation_dict <- lapply(seq_len(nrow(translation_data)), function(i) {
    row <- translation_data[i, ]
    list(xml = row$xml, tsv = row$tsv, txt = row$txt)
  })
  
  # Detect file type from the extension
  file_extension <- tolower(tools::file_ext(file))
  
  # Ensure the file extension is supported
  if (!(file_extension %in% c("txt", "xml", "tsv"))) {
    stop("Unsupported file type. Only .txt, .xml, and .tsv files are supported.")
  }
  
  # Convert to named list for easier translation
  if(file_extension == "xml"){
    translation_dict <- setNames(
      lapply(translation_data$xml, function(xml_key) {
        list(tsv = translation_data$tsv[translation_data$xml == xml_key],
             txt = translation_data$txt[translation_data$xml == xml_key])
      }),
      translation_data$xml
    )
  }
  
  if(file_extension == "txt"){
    translation_dict <- setNames(
      lapply(translation_data$txt, function(txt_key) {
        list(tsv = translation_data$tsv[translation_data$txt == txt_key],
             xml = translation_data$xml[translation_data$txt == txt_key])
      }),
      translation_data$txt
    )
  }
  
  # Translate the column names based on the file type
  colnames(rawData) <- sapply(colnames(rawData), function(colname) {
    # Get the translation for the current column name
    translation <- translation_dict[[colname]]
    
    if (!is.null(translation)) {
      # Apply translation based on the file extension
      if (file_extension == "txt") {
        return(ifelse(!is.null(translation$tsv) && translation$tsv != "", 
                      translation$tsv, 
                      translation$xml))# Translate for txt files
      } else if (file_extension == "xml") {
        return(ifelse(!is.null(translation$tsv) && translation$tsv != "", 
                      translation$tsv, 
                      colname))  # Return original xml column name
      }
    } else {
      return(colname)  # No translation found, keep the original column name
    }
  })
  
  # Return the modified data frame with translated column names
  return(rawData)
}

