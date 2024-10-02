#' getAATable
#' 
#' Gets the names and description of Amino Acids for publication purposes. 
#' 
#' @param matrixType Character for the type of matrix. Often serum ("SER"), 
#' plasma ("PLA") or urine ("URI"). 
#' @return A data table with information about amino acids. 
#' \describe{
#'  \item{\code{Compound}}{Name of the compound.}
#'  \item{\code{Unit}}{Unit of measurement taken from instrument SOP.}
#'  \item{\code{Max Value (ref.)}}{Biologically based maximum value for a compound. Based on regular population.}
#'  \item{\code{Min Value (ref.)}}{Biologically based minimum value for a compound. Based on regular population.}
#'  \item{\code{Reference Unit}}{Unit of measurement taken from instrument SOP.}
#'  \item{\code{Reference Range}}{Reference minimum to reference maximum already in the table.}
#'  }
#' @importFrom utils read.delim2
#' @importFrom data.table setDT
#' @export

getAATable <- function(matrixType = "SER"){
  
  #file is in inst, extdata folder
  tsvFile <- system.file("extdata", "plaAA.TSV", package = "ms.parser")
  if (tsvFile == "") stop("File not found in package")

  #read in tsv
  tab <- 
    read.delim2(file = tsvFile,
                fileEncoding = "latin1",
                header = TRUE,
                check.names = FALSE)
  
  #get compound names
  aa <- list()
  Compound <- unique(tab$AnalyteName)
  idx <- grep(pattern = "IS", x = unique(tab$AnalyteName))
  Compound <- Compound[-idx]
  Unit <- rep_len(x = "uM", length.out = length(Compound)) 
  
  #fill in blanks TSV does not provide for. Same format as getSmTable 
  data <- as.data.frame(cbind(Compound = Compound, Unit = Unit))
  
  #Ref Max
  data$`Max Value (ref.)` <- rep_len(x = NA, length.out = length(Compound))
  
  #Ref Min
  data$`Min Value (ref.)`<- rep_len(x = NA, length.out = length(Compound)) 
  
  #Unit
  data$`Reference Unit` <- rep_len(x = "uM", length.out = length(Compound)) 
  
  #Range
  data$`Reference Range [Unit]`<- rep_len(x = paste(data$`Min Value (ref.)`, " - ", data$`Max Value (ref.)`, "(uM)"), 
                                          length.out = length(Compound)) 
  
  #need to add reange for max and min, maybe a column with doi reference for where range came from. different for URI SER PLA?
  
  aa[["data"]] <- data
  
  #SOP where units are taken from 
  aa[["version"]] <- "SOP.TE.014a Rev:3"
  
  setDT(aa$data)
  
  return(aa$data)
}

