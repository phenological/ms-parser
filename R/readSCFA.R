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
  
  # get sampleID position in title
  if ("codePosition" %in% names(optns)) {
    codePosition <- optns$codePosition
  } else {
    codePosition <- 7
  }
  
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
    msg <- paste("fusion::getTryXML >> Missing metabolites:",
                 missing,
                 "\n")
    message(crayon::red(msg))
  }
  ## nrow(COMPOUND) must be a ntuple of the number of compounds
  if (!nrow(COMPOUND) / nrow(sampleInfo) == nrow(COMPOUND) %/% nrow(sampleInfo)) {
    cat(crayon::red("fusion::getSCFA >> dimension problems"))
  } else {
    N <- nrow(COMPOUND) / nrow(sampleInfo)
  }
  
  ## all samples are expected to have the same measurements
  if (!sum(unname(table(COMPOUND$sampleid)) == N) == nrow(sampleInfo)) {
    cat(crayon::red("fusion::getSCFA >> dimension problems with compounds"))
  }
  
  # matching sample IDs from SAMPLELISTDATA
  idx <- match(COMPOUND$sampleid, sampleInfo$id)
  if (!identical(order(unique(idx)), unique(idx))) {
    cat(crayon::red("fusion::getSCFA >> sampleInfo not in the same order as data"))
  }
  
  # creating proper columns for dataElement
  code <- sampleInfo$name[idx]
  sampleID <- gsub(" ", "", makeUnique(sapply(
    strsplit(code, "_"), "[", codePosition
  ), fromFirst = TRUE))
  
  sourceID <- gsub(" ", "", makeUnique(sapply(
    strsplit(code, "_"), "[", codePosition + 1
  ), fromFirst = TRUE))
  
  sampleType <- factor(
    sampleInfo$type[idx],
    levels = c("Analyte", "Blank", "ltr", "QC", "Standard"),
    labels = c("analyte", "blank", "ltr", "qc", "standard")
  )
  # creating LTR type
  idx <- grep("LTR", sampleID)
  sampleType[idx] <- "ltr"
  
  # adding required fields (columns)
  COMPOUND$sampleID <- sampleID
  COMPOUND$sourceID <- sourceID
  COMPOUND$sampleType <- sampleType
  COMPOUND$expname <- basename(path)
  
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
  
  obsDescr <- split(COMPOUND, 1:N) # remove the first 3 column (id,sampleid,groupid)
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
  
  # removing first 3 columns from obsDescr
  obsDescr<-lapply(obsDescr, function(x)
    obsDescr <- x[, -c(1, 2, 3)])
  
  obsDescr <- lapply(obsDescr, function(x) {
    x[,"sampleID"] <- make.unique(gsub("\\#.*", "", x[,"sampleID"]), sep = "#")
    return(x)
  })
  
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
# xml_name(xml)
# xml_attrs(xml)
# xml_name(xml_children(xml))
# xml_name(xml_children(xml_children(xml)[[3]]))
# xml_name(xml_children(xml_children(xml_children(xml)[[3]])))
# xml_name(xml_children(xml_children(xml_children(xml)[[3]]))[[1]])
# xml_name(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))
# xml_name(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[3]]))
#
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]])
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]]))
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]])[[1]]))
#
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[3]])))
#
# response <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/RESPONSE"))
# RESPONSE <- data.frame(do.call("rbind", lapply(response, function(x) unlist(x))))
# curve <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE"))
# CURVE <- data.frame(do.call("rbind", lapply(curve, function(x) unlist(x))))
# calcurve <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/CALIBRATIONCURVE"))
# CALCURVE <- data.frame(do.call("rbind", lapply(calcurve, function(x) unlist(x))))
# correlation <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/CORRELATION"))
# CORRELATION <- data.frame(do.call("rbind", lapply(correlation, function(x) unlist(x))))
# determination <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/DETERMINATION"))
# DETERMINATION <- data.frame(do.call("rbind", lapply(determination, function(x) unlist(x))))
# responsefactor <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/RESPONSEFACTOR"))
# RESPONSEFACTOR <- data.frame(do.call("rbind", lapply(responsefactor, function(x) unlist(x))))



