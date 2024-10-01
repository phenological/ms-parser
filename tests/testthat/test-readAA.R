
test_that("sampleID is correct on older cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa[["data"]][["sampleID"]])), nrow(aa[["data"]]))
  
  #should have COV sample IDs
  expect_true(c("COV00261") %in% aa$data$sampleID)

})

test_that("sampleID is correct on newer cohorts", {
  aa <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  
  #should have all unique sampleIDs
  expect_equal(length(unique(aa[["data"]][["sampleID"]])), nrow(aa[["data"]]))
  
  #should have COV sample IDs
  expect_equal(aa[["data"]][["sampleID"]][25], "COV17242")
  
})


