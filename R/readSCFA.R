#' extract targeted MS data from task export xml file
#'
#' @param path - the path to the expName folder
#' @param optns list of optns
#' \itemize{
#'    \item codePosition - position of the code in the file name. Default is 7.
#'    \item columnList - list of columns to be selected
#' }
#' @return a dataElement
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_attrs
#' @importFrom dplyr %>%
#' 
#' 
readSCFA <- function(path, optns = list()) {
  id <- name <- createdate <- createtime <- type <- desc <- stdconc <- NULL
  vial <- inletmethodname <- msmethodname <- tunemethodname <- instrument <- NULL
  
  # get list of metabolites if not default
  if ("columnsList" %in% names(optns)) {
    columnsList <- optns$columnsList
  } else {
    columnsList <- c(
      "Acetic acid-d4 (1)",
      "Acetic acid",
      "Propionic acid (1)",
      "Propionic acid 13C3 (1)",
      "Isobutyric acid-d7",
      "Isobutyric acid (1)",
      "Butyric acid-d7 (1)",
      "Butyric acid (1)",
      "2Methylbutyric acid (1)",
      "Isovaleric acid",
      "Valeric acid (1)",
      "Isovaleric acid-d9 (1)",
      "3Methylvaleric acid (1)",
      "Hexanoic acid (1)",
      "Hexanoic acid-d11 (1)"
    )
  }
  
  xml <- read_xml(path)
  
  # retrieving sample information from SAMPLELISTDATA
  sample <- xml_children(xml_find_all(xml, "//GROUPDATA/GROUP/SAMPLELISTDATA"))
  sample <- data.frame(do.call("rbind", lapply(xml_attrs(sample), function(x)
    unlist(x))))
  sampleInfo <- sample %>% dplyr::select(
    c(
      id,
      name,
      createdate,
      createtime,
      type,
      desc,
      stdconc,
      vial,
      inletmethodname,
      msmethodname,
      tunemethodname,
      instrument
    )
  )
  
  # retrieving data for each samples from SAMPLELISTDATA
  compound <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND"))
  COM <- data.frame(do.call("rbind", lapply(compound, function(x)
    unlist(x))))
  
  peak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK"))
  PEAK <- data.frame(do.call("rbind", lapply(peak, function(x)
    unlist(x))))
  
  ispeak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK/ISPEAK"))
  ISPEAK <- data.frame(do.call("rbind", lapply(ispeak, function(x)
    unlist(x))))
  
  COMPOUND <- cbind(COM, PEAK, ISPEAK)
  
  # retrieving compound names
  compoundNames <- unique(COMPOUND$name)
  
  # performing a few data integrity checks
  # checking for missing metabolites
  missing <- setdiff(columnsList, compoundNames)
  if (length(missing) > 0) {
    msg <- paste(" >> Missing metabolites:",
                 missing,
                 "\n")
    message(crayon::red(msg))
  }
  ## nrow(COMPOUND) must be a ntuple of the number of compounds
  if (!nrow(COMPOUND) / nrow(sampleInfo) == nrow(COMPOUND) %/% nrow(sampleInfo)) {
    cat(crayon::red(" >> dimension problems"))
  } else {
    N <- nrow(COMPOUND) / nrow(sampleInfo)
  }
  
  ## all samples are expected to have the same measurements
  if (!sum(unname(table(COMPOUND$sampleid)) == N) == nrow(sampleInfo)) {
    cat(crayon::red(" >> dimension problems with compounds"))
  }
  
  # matching sample IDs from SAMPLELISTDATA
  idx <- match(COMPOUND$sampleid, sampleInfo$id)
  if (!identical(order(unique(idx)), unique(idx))) {
    cat(crayon::red(" >> sampleInfo not in the same order as data"))
  }
  
  # creating proper columns for dataElement
  COMPOUND<-data.frame(projectName = sapply(strsplit(sampleInfo$name, "_"), "[", 1),
                       cohortName = sapply(strsplit(sampleInfo$name, "_"), "[", 2),
                       sampleMatrixType = sapply(strsplit(sampleInfo$name, "_"), "[", 3),
                       runName = sapply(strsplit(sampleInfo$name, "_"), "[", 5),
                       plateID = sapply(strsplit(sampleInfo$name, "_"), "[", 6),
                       sampleID = make.unique(sapply(strsplit(sampleInfo$desc, "_"), "[", 1), sep = "#"),
                       sourceID = sapply(strsplit(sampleInfo$desc, "_"), "[", 2),
                       sampleType = ifelse(!substr(sampleInfo$desc,1,3) %in% c("BLK","CAL","LTR","SLT","VLT","SB","QC0","PQC","CON","CHK"),"Sample",substr(sampleInfo$desc,1,3)),
                       expname = basename(path),
                       COMPOUND)
  
  
  # casting the data
  # (as.numeric is mandatory otherwise dcast will order id in alphabetical order)
  .Data <- dcast(COMPOUND,
                 as.numeric(sampleid) ~ as.numeric(id),
                 value.var = "analconc")
  
  # checking that data are matched with sampleInfo
  if (!identical(as.character(.Data[, 1]), sampleInfo$id)) {
    cat(crayon::red(
      "fusion::getSCFA >> sampleInfo not in the same order as .Data"
    ))
  } else {
    .Data <- .Data[, -1]
  }
  
  obsDescr <- split(COMPOUND, 1:N) 
  #checking that description matches data columns
  if (!identical(unname(sapply(obsDescr, function(x)
    unique(x$id))), colnames(.Data))) {
    cat(crayon::red(
      "fusion::getSCFA >> order of data and obsDescr does not match\n"
    ))
  } else {
    varName <- unname(sapply(obsDescr, function(x)
      unique(x$name)))
  }
  
  # making data numeric
  .Data <- sapply(.Data, function(x)
    as.numeric(x))
  colnames(.Data) <- varName
  rownames(.Data) <- sampleInfo$name
  
  # removing "id", "sampleid", "groupid" columns from obsDescr
  obsDescr<-lapply(obsDescr, function(x){
    obsDescr <- x[, !colnames(x) %in% c("id", "sampleid", "groupid")]
    return(x)})
  
  
  # creating dataElement
  da <- new(
    "dataElement",
    .Data = .Data,
    obsDescr = obsDescr,
    varName = unlist(varName),
    type = "T-MS",
    method = "SCFA"
  )
  return(da)
}


