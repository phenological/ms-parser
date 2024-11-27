
test_that("xml colnames translated properly", {
  rawData <- readXML2(path = "~/git/phenological/ms-parser/inst/extdata/TRY/TRY.xml")
  before <- colnames(rawData)
  rawData <- columnTranslation(rawData = rawData, file = "~/git/phenological/ms-parser/inst/extdata/TRY/TRY.xml")
  after <- colnames(rawData)
  
  #desc should remain desc, there is no tsv equivalent
  expect_contains(object = after, expected = "desc")
  
  #rsquared should now be R2
  idx <- which(before == "rsquared")
  expect_equal(object = after[idx], expected = "R2")
  
})

test_that("txt colnames translated properly", {
  rawData <- readTXT(path = "~/git/phenological/ms-parser/inst/extdata/TRY/TRY.TXT")
  before <- colnames(rawData)
  rawData <- columnTranslation(rawData = rawData, file = "~/git/phenological/ms-parser/inst/extdata/TRY/TRY.TXT" )
  after <- colnames(rawData)
  
  #txt staying the same (no alternative)
  expect_contains(object = after, expected = "X")
  
  #txt to xml (only alternative)
  idx <- which(before == "Sample.Text")
  expect_equal(object = after[idx], expected = "desc")
  
  #txt to tsv (tsv available)
  idx <- which(before == "Conc.")
  expect_equal(object = after[idx], expected = "Quantity")
})
