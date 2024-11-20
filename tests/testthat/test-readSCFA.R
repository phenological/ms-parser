test_that("readSCFA works",{
  test <- readSCFA(file = "~/git/phenological/ms-parser/inst/extdata/SCFA.xml")
  
  expect_true(object = unique(test$projectName) == "fibers")
  expect_true(unique(test$cohortName) == "C1")
  expect_true(unique(test$sampleMatrixType) == "PLA")
  
  expect_true(length(grep(pattern = "[IS]", x = unique(test$AnalyteName))) > 1)
  
  expect_true(unique(test[which(test$paramName == "plateID"), "paramValue"]) == "FIBp041" )
  
  expect_true("check" %in% unique(test$sampleType))
  

})