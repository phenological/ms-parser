test_that("readTRy works",{
  test <- readBA(file = "~/git/phenological/ms-parser/inst/extdata/BA.xml")
  
  expect_true(object = unique(test$projectName) == "ROCIT20")
  expect_true(unique(test$cohortName) == "C1")
  expect_true(unique(test$sampleMatrixType) == "PLA")
  
  expect_true(length(grep(pattern = "[IS]", x = unique(test$AnalyteName))) > 1)
  
  expect_true(unique(test[which(test$paramName == "plateID"), "paramValue"]) == "p01" )
  
  
  idx <- which(grepl(pattern = "LTR", x = unique(test$sampleID)))
  expect_true(length(idx) > 1)
})