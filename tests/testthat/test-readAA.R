
test_that("analyte names are consisten", {
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA3.TSV")
  
  #AccQTag should not be present 
  expect_true(length(grep("AccQTag", colnames(new))) == 0)
  expect_true(length(grep("AccQTag", colnames(new))) == 0)
  expect_true(length(grep("AccQTag", colnames(new))) == 0)
  
  #[IS] should be the only thing signifying internal standards
  expect_true("Valine [IS]" %in% colnames(oldest$data))  
  expect_true("Valine [IS]" %in% colnames(recent$data)) 
  expect_true("Valine [IS]" %in% colnames(new$data)) 
  
})

test_that("Sample information columns are the same", {
  oldest <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA.TSV")
  recent <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA2.TSV")
  new <- readAA(file = "~/git/phenological/ms-parser/inst/extdata/plaAA3.TSV")
  
  #newest exports have 20 extra columns
  expect_true(length(setdiff(colnames(new$meta), colnames(oldest$meta))) == 20) 
  
  #should be no other differences
  expect_true(length(setdiff(colnames(oldest$meta), colnames(new$meta))) == 0)
  expect_true(length(setdiff(colnames(oldest$meta), colnames(recent$meta))) == 0)
  expect_true(length(setdiff(colnames(recent$meta), colnames(oldest$meta))) == 0)
  
  })

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


