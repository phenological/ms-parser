
test_that("NA sampleID's removed and AnalysisNames listed in output", {
  expected_message <- "The following AnalysisName have no sampleID and were not processed:: "
  
  output <- capture.output(readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA4.TSV"))
  
  idx <- grep("The following AnalysisName have no sampleID and were not processed::", output)  
  
  expect_true(length(idx) > 0)
})

test_that("analyte names are consisten", {
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA3.TSV")
  
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
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA3.TSV")
  
  #newest exports have 20 extra columns
  expect_true(length(setdiff(unique(new$paramName), unique(oldest$paramName))) == 20) 
  expect_true(length(setdiff(unique(new$paramName), unique(recent$paramName))) == 20) 
  #should be no other differences
  expect_true(length(setdiff(unique(oldest$paramName), unique(new$paramName))) == 0)
  expect_true(length(setdiff(unique(oldest$paramName), unique(recent$paramName))) == 0)
  expect_true(length(setdiff(unique(recent$paramName), unique(oldest$paramName))) == 0)
  
  })

test_that("sampleID is correct on older cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa$sampleID)), length(unique(aa$AnalysisName)))
  
  #should have COV sample IDs
  expect_true(c("COV00261") %in% aa$sampleID)

})

test_that("sampleID is correct on newer cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa$sampleID)), length(unique(aa$AnalysisName)))
  
  #should have COV sample IDs
  expect_true(c("COV17242") %in% aa$sampleID)
})

test_that("can supply optional arguments", {
  aa<- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV", 
              optns = list(sampleMatrixType = "URI", projectName = "notReal", cohortName = "first"))
  
  expect_true(unique(aa$sampleMatrixType) == "URI")
  expect_true(unique(aa$projectName) == "notReal")
  expect_true(unique(aa$cohortName) == "first")
  })


test_that("plateID can be added", {
file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV"
file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV"
file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA3.TSV"
  file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA4.TSV"

  #requies position change
p1 <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA.TSV",
             optns = list(platePosition = 6))
idx <- which(p1$paramName == "plateID")
expect_true(unique(p1[idx, "paramValue"]) == "PLASMA8B") 

#regular
 p2 <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA2.TSV")
 idx <- which(p2$paramName == "plateID")
expect_contains(unique(p2[idx, "paramValue"]), expected = "COVp263") 

 #regular
 p3 <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA3.TSV")
 idx <- which(p3$paramName == "plateID")
 expect_true(unique(p3[idx, "paramValue"]) == "SPTp003")
 
 #rows that are automatically removed
 p4 <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/AA/plaAA4.TSV")
 idx <- which(p4$paramName == "plateID")
 expect_true(unique(p4[idx, "paramValue"]) == "DWAp016")
 
})

