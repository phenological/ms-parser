
test_that("readTRy works",{
  test <- readTRY(file = "~/git/phenological/ms-parser/inst/extdata/TRY/TRY.xml")
  
  expect_true(object = unique(test$projectName) == "covid19")
  expect_true(unique(test$cohortName) == "harvard")
  expect_true(unique(test$sampleMatrixType) == "SER")
  
  expect_true(length(grep(pattern = "[IS]", x = unique(test$AnalyteName))) > 1)
  expect_true(unique(test[which(test$paramName == "plateID"), "paramValue"]) == "COVp41" )
  
  idx <- which(grepl(pattern = "LTR", x = unique(test$sampleID)))
  expect_true(length(idx) > 1)
})





###TXT files#####
#Tryptophan
 # harvard <- readTXT(path = "~/Downloads/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P41P42.txt")
 # heidleberg <- readTXT(path =  "~/Downloads/PAT-01 COVID_Heidelberg_TRP_COVp93_urine_ plate1_export.TXT")
 # biogune <- readTXT(path =   "~/Downloads/BioGune_Plate20 and 22_11022021.TXT")
 # cambridge <- readTXT(path =  "~/Downloads/plate_5_export.txt")
 
 
#BA
 # ba <- readTXT(path = "~/Downloads/ACL-BA-003 - Full Plate.txt")
 # baxml <- readXML2(path = "~/Downloads/ACL-BA-003 - Full Plate.xml")
#SCFA
#  scfa <- readXML2(path = "~/Documents/LucyDatasets/xml/fibers_C1_PLA_MS-AT-SCFA@fEPP_FIBr04_FIBp041.xml")
# ba <- readXML2(path = "/Documents/LucyDatasets/xml/ROCIT20_C1_PLA_MS-BILE_PAT02_ROCIT_MS_p01.xml")
#   
#   colnames(data)[colnames(data) == "Name"] <- "AnalysisName"
 
  

##does xml have extra info###
# harvardtxt <- data
# library(XML)
# file_path <- path <-  "~/Documents/LucyDatasets/xml/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P41P42.xml"
#   "~/Documents/LucyDatasets/xml/ACL-BA-003 - Full Plate.xml"
#   "~/Documents/LucyDatasets/xml/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P41P42.xml"
#   "~/Documents/LucyDatasets/xml/fibers_C1_PLA_MS-AT-SCFA@fEPP_FIBr04_FIBp041.xml"



