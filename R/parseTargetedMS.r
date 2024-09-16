#' importation function for targeted MS assays
#'
#' @param file - the file path to be imported
#' @param method - the method used to acquire the assay
#' @param options list of options
#' \itemize{
#'    \item codePosition - position of the code in the file name
#'    \item columnList - list of columns to be selected
#' }
#' @return a dataElement
#'
#' @export
#' @importFrom utils data read.table
#' @importFrom reshape2 dcast
#' @importFrom crayon red blue yellow white green bold
#' @importFrom utils read.delim2
parseTargetedMS <- function(file, method, options = list()) {
  cat(paste("fusion: using import method for",
            method, "\n"))

  if (method == "tryptophan") {
    da <- parseTRY(file, options)
  }
  if (method == "aminoAcids") {
    da <- parseMS_AA(file, options)
  }

  return(da)
}


# preParseTr <- function(file) {
#   res <- readLines(file)
#   li <- sapply(res, function(x) length(strsplit(x, "\t")[[1]]))
#   le <- unique(li)
#
#   ne <- lapply(res, function(x) {
#     ch <- strsplit(x, "\t")[[1]]
#     if (length(ch) == max(le)) {
#       return(ch)
#     } else if (length(ch) > 0){
#       if (grepl("Compound", ch)[1]) {
#         return(c(ch, rep("NA", max(le) - length(ch))))
#       }
#       if (length(ch) == max(le)) {
#         return(c(ch, rep("NA", max(le) - length(ch))))
#       }
#     }
#   })
#
#   new <- do.call("rbind", ne)
#   new <- new[-1,]
#
#   return(new)
# }

# parseMS_Tr <- function(file, options) {
#   mw <- tMsTestsets$mw
#
#   rawData <- preParseTr(file)
#
#   cat(paste("fusion: using import method for tryptophan\n"))
#   cat(paste("fusion:", nrow(rawData),
#             "line(s) read\n"))
#   oldHeaders <- rawData[2,] # capturing column order
#   oldHeaders[1] <- "rowIndex"
#
#   fi <- rawData[,1] == "" # excluding title rows
#   rawData <- rawData[!fi,]
#   cat(paste("fusion:",
#             sum(fi),
#             "empty line(s) removed\n"))
#
#   spliter <- which((grepl("Compound", rawData[,1])))
#   numberOfCompounds <- length(spliter)
#   cat(paste("fusion:",
#             numberOfCompounds,
#             "compound(s) found\n"))
#
#   dataChkLength <- c(spliter[2:length(spliter)],
#                      nrow(rawData) + 1) - spliter[1:length(spliter)]
#
#   if (!length(unique(dataChkLength)) == 1) {
#     stop("fusion: data chunks have different size, check your data")
#   } else {
#     cat(paste("fusion:",
#               dataChkLength[1],
#               "is data chunk size\n"))
#   }
#
#   dataLength <- sum(dataChkLength)
#   cat(paste("fusion:",
#             dataLength,
#             "line(s) of data found\n"))
#
#   newData <- list()
#   headersOrder <- c("Name",
#              "Sample Text",
#              "Type",
#              "%Dev",
#              "Primary Flags",
#              "Conc.",
#              "Std. Conc",
#              "RT",
#              "Area",
#              "IS Area",
#              "Response")
#
#   for (i in 1:numberOfCompounds){
#
#     rge <- spliter[i]:(spliter[i] + dataChkLength[i] - 1)
#     cpndName <- rawData[spliter[i], 1]
#     dataChk <- rawData[rge,]
#
#     cpndName <- strsplit(dataChk[1,1], ":  ")[[1]][2] # reading title
#     dataChk <- dataChk[-1,] # removing title
#
#     idx <- match(headersOrder, oldHeaders)
#     dataChk <- dataChk[,idx]
#     colnames(dataChk) <- headersOrder
#
#     newData[[i]] <- list(cpndName = cpndName, #reading data chunk
#                          dataChk = data.frame(dataChk))
#   }
#
#   newDataLength <- sum(unlist(lapply(newData, function(x) dim(x$dataChk)[1])))
#
#   if (dataLength != newDataLength + length(spliter)) {
#     stop(paste("fusion: the expected length of the data is:",
#                dataLength,
#                "/ received:",
#                newDataLength + length(spliter)))
#   } else {
#     cat(paste("fusion:", numberOfCompounds, "compounds imported\n"))
#   }
#
#   # flipping the matrix
#   dataMatrix <- list()
#   obsDescr <- list()
#   varName <- list()
#   sampleNames <- newData[[1]][[2]]$Name
#
#   extractCode <- function(path) {
#     l <- strsplit(path, "_")
#     len <- length(l[[1]])
#     code <- l[[1]][options$codePosition]
#     return(code)
#   }
#   code <- do.call("rbind",
#                   lapply(sampleNames,
#                          function(x) extractCode(x)
#                   )
#   )
#   uid <- makeUnique(code, "#")
#
#   cleaningList <- c("dopamine",
#                     "citrulline")
#   for (chk in newData) {
#     cpndName <- chk[[1]]
#     if (tolower(cpndName) %in% tolower(cleaningList)) {
#       cat(crayon::blue("fusion: ") %+%
#             crayon::blue$bold(cpndName) %+%
#             crayon::blue(" ignored for that method."), fill = TRUE)
#     } else {
#     dataCol <- data.frame(as.numeric(unlist(chk[[2]]$`Conc.`)))
#     names(dataCol) <- cpndName
#     if (identical(chk[[2]]$Name, sampleNames)){
#       if (cpndName %in% mw$analyte) {
#         idx <- which(mw$analyte == cpndName)
#         dataMatrix <- c(dataMatrix, dataCol / mw$mw[idx])
#       } else {
#         dataMatrix <- c(dataMatrix, dataCol)
#         warning(paste("fusion:", cpndName, "molecular weight not found,
#                     concentration is not exported correctly\n"))
#       }
#       varName <- c(varName, cpndName)
#     } else {
#       stop ("fusion: row order alterated, matrix cannot be flipped")
#     }
#     fi <- which(names(chk[[2]]) == "Conc.")
#     descr <- data.frame(chk[[2]][, -fi], check.names = FALSE)
#
#     # adding sampleID
#     descr <- cbind(sampleID = uid, descr)
#
#     # renaming sampleType
#     fi <- which(colnames(descr) == "Type")
#     colnames(descr)[fi] <- "sampleType"
#
#     # set LTR to type ltr
#     fi <- grepl("PLA", descr$sampleID) |
#       grepl("LTR", descr$sampleID) |
#       grepl("URI", descr$sampleID)
#     descr$sampleType[fi] <- "ltr"
#
#     descr$sampleType <- tolower(descr$sampleType)
#     descr$sampleType[descr$sampleType == "analyte"] <- "sample"
#
#     obsDescr <- c(obsDescr, list(descr))
#     }
#   }
#
#   .Data <- do.call("cbind", dataMatrix)
#   if (nrow(.Data) == dataChkLength[1] - 1
#       & ncol(.Data) == numberOfCompounds) {
#     cat(paste("fusion: matrix flipped\n"))
#   }
#
#   da <- new("dataElement",
#             .Data = .Data,
#             obsDescr = obsDescr,
#             varName = unlist(varName),
#             type = "T-MS",
#             method = "tryptophan")
#   return(da)
# }
