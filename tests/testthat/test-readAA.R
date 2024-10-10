
test_that("NA sampleID's removed and AnalysisNames listed in output", {
  expected_message <- "The following AnalysisName have no sampleID and were not processed:: "
  
  output <- capture.output(readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA4.TSV"))
  
  idx <- grep("The following AnalysisName have no sampleID and were not processed::", output)  
  
  expect_true(length(idx) > 0)
})

test_that("analyte names are consisten", {
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA3.TSV")
  
  #AccQTag should not be present 
  expect_true(length(grep("AccQTag", new$AnalyteName)) == 0)
  expect_true(length(grep("AccQTag", new$AnalyteName)) == 0)
  expect_true(length(grep("AccQTag", new$AnalyteName)) == 0)
  
  #[IS] should be the only thing signifying internal standards
  expect_true("Valine [IS]" %in% new$AnalyteName)  
  expect_true("Valine [IS]" %in% new$AnalyteName) 
  expect_true("Valine [IS]" %in% new$AnalyteName) 
  
})

test_that("Sample information columns are the same", {
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA3.TSV")
  
  #newest exports have 20 extra columns
  expect_true(length(setdiff(unique(new$paramName), unique(oldest$paramName))) == 20) 
  expect_true(length(setdiff(unique(new$paramName), unique(recent$paramName))) == 20) 
  #should be no other differences
  expect_true(length(setdiff(unique(oldest$paramName), unique(new$paramName))) == 0)
  expect_true(length(setdiff(unique(oldest$paramName), unique(recent$paramName))) == 0)
  expect_true(length(setdiff(unique(recent$paramName), unique(oldest$paramName))) == 0)
  
  })

test_that("sampleID is correct on older cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa$sampleID)), length(unique(aa$AnalysisName)))
  
  #should have COV sample IDs
  expect_true(c("COV00261") %in% aa$sampleID)

})

test_that("sampleID is correct on newer cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa$sampleID)), length(unique(aa$AnalysisName)))
  
  #should have COV sample IDs
  expect_true(c("COV17242") %in% aa$sampleID)
})

test_that("can supply optional arguments", {
  aa<- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV", 
              optns = list(sampleMatrixType = "URI", projectName = "notReal", cohortName = "first"))
  
  expect_true(unique(aa$sampleMatrixType) == "URI")
  expect_true(unique(aa$projectName) == "notReal")
  expect_true(unique(aa$cohortName) == "first")
  })

# test_that("read multiple plates", {
#   plasmaFolder <- "~/git/phenological/ms-parser/inst/extdata/"
#   plates <- dir(plasmaFolder, pattern = "\\.TSV$")
#   
# 
# # sampleID, paramName, paramValue, paramType, description (analysisName??)
# c("COV00262", "AnalyteName", "Valine", "Character", "covid_cambridge_MS_AA_PAI04_PLASMA8B_100920_COV00262_CV0043_22")
#   
#   d <- list()
#   for (i in plates) {
#     d[[i]] <- readAA(file = file.path(plasmaFolder, i))
#   }
#   combined <- do.call(rbind, d)
#   
#   
#   #as if were making dae
#   idx <- which(combined$paramName == "Quantity")
#   data <- dcast(combined[ idx,],
#                 AnalysisName + sampleID ~ AnalyteName, 
#                 value.var = "paramValue")
#   
#   cat(paste(shQuote(colnames(dataeg), type="cmd"), collapse=", "))
#   
#   obCols <- c(
#     "AnalysisName",
#     "sampleID",
#     "AnalyteName",
#     "m/z",
#     "expected m/z",
#     "Area",
#     "Height",
#     "mSigma",
#     "Retention Time[min]",
#     "Expected Retention Time[min]",
#     "Quantity",
#     "sampleType",
#     "Internal Standard(ISTD)",
#     "Expected Quantity",
#     "Unit of Quantity",
#     "R2",
#     "Accuracy/Recovery[%]",
#     "Residuals[%]",
#     "Visited"
#   )
#   
#   analytes <- unique(combined$AnalyteName)
#   
#   for(analyte in analytes){
#     idx<- which(combined$AnalyteName == analyte)
#     newish <- combined[idx, ]
#     newish[which(newish$paramName %in% obCols),]
#     
#   }
#   
# })

