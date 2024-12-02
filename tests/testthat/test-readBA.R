test_that("readBA works",{
  test <- readBA(file = "~/git/phenological/ms-parser/inst/extdata/BA/BA.xml")
  
  expect_true(object = unique(test$projectName) == "ROCIT20")
  expect_true(unique(test$cohortName) == "C1")
  expect_true(unique(test$sampleMatrixType) == "PLA")
  
  expect_true(length(grep(pattern = "[IS]", x = unique(test$AnalyteName))) > 1)
  
  expect_true(unique(test[which(test$paramName == "plateID"), "paramValue"]) == "p01" )
  
  
  idx <- which(grepl(pattern = "LTR", x = unique(test$sampleID)))
  expect_true(length(idx) > 1)
  
})

test_that("analyte names are same between tsv and xml",{
  
  baxml <- readBA(file = "~/git/phenological/ms-parser/inst/extdata/BA/BA.xml")
  baxml <- unique(baxml$AnalyteName)
  batsv <- readBA(file = "~/git/phenological/ms-parser/inst/extdata/BA/BA.TSV")
  batsv <- unique(batsv$AnalyteName)
  
  #only MCA is in xml and not tsv analytes
  expect_true(all(grepl("MCA", x = setdiff(baxml, batsv))))
  
  #should be nothing in tsv that's not in xml
  expect_equal(object = length(setdiff(batsv, baxml)), expected = 0)
  
})

# baxml <- readXML2(path = "~/git/phenological/ms-parser/inst/extdata/BA/BA.xml")
# baxml <- unique(baxml$AnalyteName)
# batsv <- read.delim2(file = "~/git/phenological/ms-parser/inst/extdata/BA/BA.TSV",
#                      fileEncoding = "latin1",
#                      header = TRUE,
#                      check.names = FALSE)
# batsv<- unique(batsv$AnalyteName)
# 
# rawData <- readXML2(path = "~/Desktop/Biomica1/BCAp009_core18.xml")
# newer <- unique(rawData$AnalyteName)
# 
# 
# new <-  str_trim(str_replace_all(
#   newer,
#   c("-d4" = " [IS]",
#     "-D4" = " [IS]",
#     "-d5" = " [IS]",
#     "\\." = "-",
#     "_" = "-",
#     "Acid" = "acid",
#     "(^|[^a-zA-Z0-9])a-" = "\\1alpha-",
#     "(^|[^g-zA-Z0-9])g-" = "\\1gamma-",
#     "(^|[^b-zA-Z0-9])b-" = "\\1beta-",
#     "(?<!\\[IS\\])\\[IS\\](\\s*\\[IS\\])+" = "[IS]"
#   )
# ))
# 
# new <- 
#   str_replace(
#     string = new,
#     pattern = "^HDCA$",  # Matches only the string "HDCA" exactly
#     replacement = "HDCA-Hyodeoxycholic acid"
#   )
# 
# new <- str_replace_all(new, c("Glycochnodeoxycholic" = "Glycochenodeoxycholic", 
#                               "Tauro-ursodeoxycholic" = "Tauroursodeoxycholic"))
# 
# new <- sapply(new, function(name) {
#   # Check if name contains parentheses
#   if (grepl("\\(", name)) {
#     name <- sub("(.*) \\((.*)\\)", "\\2-\\1", name)
#   }
#   name
# })
# 
# new <- str_trim(new)
# 
# baxmlnew<-new
# batsvnew<-new
# 
# setdiff(essential, batsvnew)
# 
# 
# essential <- c(
#   "CDCA-Chenodeoxycholic acid",
#   "CA-Cholic acid",
#   "DCA-Deoxycholic acid",
#   "GCDCA-Glycochenodeoxycholic acid",
#   "GCA-Glycocholic acid",
#   "GDCA-Glycodeoxycholic acid",
#   "GLCA-Glycolithocholic acid",
#   "GUDCA-Glycoursodeoxycholic acid",
#   "HDCA-Hyodeoxycholic acid",
#   "LCA-Lithocholic acid",
#   "TUDCA-Tauroursodeoxycholic acid",
#   "TCDCA-Taurochenodeoxycholic acid",
#   "TCA-Taurocholic acid",
#   "TDCA-Taurodeoxycholic acid",
#   "THDCA-Taurohyodeoxycholic acid",
#   "TLCA-Taurolithocholic acid",
#   "UDCA-Ursodeoxycholic acid"
# )
# essential %in% new
