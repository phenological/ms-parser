
############# checking columns######## FOR FUSION
# if ("columnsList" %in% names(optns)) {
#   columnsList <- optns$`columnsList`
# } else {
#   columnsList <- c("AnalyteName",
#                    "AnalysisName",
#                    "SampleType",
#                    "expected m/z",
#                    "m/z",
#                    "Retention Time[min]",
#                    "Expected Retention Time[min]",
#                    "mSigma",
#                    "Area",
#                    "Height",
#                    "Visited",
#                    "Quantity",
#                    "Unit of Quantity",
#                    "Expected Quantity",
#                    "Residuals[%]",
#                    "R2",
#                    "Accuracy/Recovery[%]",
#                    "Internal Standard(ISTD)")
# }
# missingCol <- setdiff(columnsList, names(rawData))
# if (length(missingCol) > 0) {
#   cat(crayon::red("fusion: column ") %+%
#         crayon::red$bold(missingCol) %+%
#         crayon::red(" is missing from file."), fill = TRUE)
# } else {
#   cat("fusion: no missing columns")
# }
# 
# if ("columnsList" %in% names(optns)) {
#   idx <- match(optns$columnsList, names(rawData))
#   rawData <- rawData[,idx]
# }

##################removing unnecessary columns#########
# fi <- names(rawData) %in% columnsList
# rawData <- rawData[, fi]

#################removing unnecessary compounds ################## FOR FUSION
# cleaningList <- c("4--hydroxyproline",
#                   "5-Oxoproline",
#                   "Aminoadipic acid",
#                   "Ethanolamine",
                  # "Tryptophan",
#                   "Carnosine",
#                   "Cystathionine")
# 
# fi <- is.na(match(rawData$AnalyteName, cleaningList))
# rawData <- rawData[fi,]

#tell what's left after cleaning
# compoundList <- unique(rawData$AnalyteName)
# numberOfCompounds <- length(compoundList)
# cat(paste("fusion:",
#           numberOfCompounds,
#           "compound(s) retained\n"))
# 
# cat(crayon::blue("fusion: ") %+%
#       crayon::blue$bold(cleaningList) %+%
#       crayon::blue(" ignored for that method."), fill = TRUE)


