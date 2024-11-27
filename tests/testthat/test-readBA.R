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
