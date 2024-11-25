test_that("read xml has rsquared", {
  #has the rsquared been included and recorded correctly
  
  test <- readXML2(path = "~/git/phenological/ms-parser/inst/extdata/TRY.xml")
  
  idx <- which(test$AnalyteName == "Melatonin")
  
  #should be one unique entry per analyte
  expect_true(length(unique(test[idx, "rsquared"])) == 1)
  
  #melatonin for given example of TRY xml file should be 0.7811044628, if open in excel should see this
  expect_true(unique(test[idx, "rsquared"]) == "0.7811044628")
  
})

test_that("BA has rquared included", {
  
  test <- readXML2(path = "~/git/phenological/ms-parser/inst/extdata/BA.xml")
  
  idx <- which(test$AnalyteName == "b-MCA-d5")
  
  #should be one unique entry per analyte
  expect_true(length(unique(test[idx, "rsquared"])) == 1)
  
  #"b-MCA-d5" for given example of BA xml file should be 0, if open in excel should see this, is an IS but records an rsquared
  expect_true(as.numeric(unique(test[idx, "rsquared"])) == 0)
})