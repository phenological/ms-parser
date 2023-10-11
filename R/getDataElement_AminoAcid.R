

getDE <- function(file = "/path/file",      # dataElement path to file
                  samplePrefix = "OIH",     # prefix used by samples in sampleID/file list
                  analytes = analytes,      # vector containing list of analytes
                  RSDprop = 20,             # prop/percent of RSD
                  QCprop = 20,              # prop/percent of QC
                  QCinjectionsno = 9,       # number of QC injections performed
                  QCinjectionprop = 50,     # percentage of QC injections needed to pass
                  R2prop = 0.99,            # R2 required to pass
                  CVprop = 30,              #
                  incConc = TRUE,           # include concentration  ??
                  outData = TRUE,           # output cleaned data file
                  outName = "out",          # output folder name to save report/data
                  ...){



  require(tidyverse)
  require(fusion)



  # create 'call' of function


  # get data element
  da<-local(get(load(file)))



  # get annotation data
  AA <-data.frame(apply(da@.Data,2,as.numeric))

  AA_ANN<-da@obsDescr[[1]] %>%
    mutate(sampleType = case_when(
      grepl(samplePrefix, sampleID) ~ "SAMPLE",
      grepl("LTR", toupper(AnalysisName)) ~ "LTR",
      TRUE ~ sampleType)) %>%
    mutate(sampleType = case_when(
      grepl("SLTR", toupper(sampleID)) ~ "SLTR",
      grepl("CAL", toupper(AnalysisName)) ~ "CALIBRANT",
      grepl("QC", toupper(AnalysisName)) ~ "QC",
      TRUE ~ sampleType)) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    mutate(removeSample = ifelse(grepl("Suitability", AnalysisName), 1, 0),
           plateOrder = as.numeric(str_split_i(AnalysisName, "_", -1)))



  AnnData <- AA_ANN %>%
    select(AnalysisName, sampleID, sampleType, removeSample, plateOrder) %>%
    #rename("PlateID" = AnalysisName) %>%
    mutate(PlateID = gsub("(.*p)([0-9][0-9])", "\\2", AnalysisName)) %>%
    mutate(PlateID = as.numeric(gsub("([0-9][0-9][0-9])(.*)", "\\1", PlateID))) %>%
    arrange(PlateID, plateOrder) %>%
    mutate(studyOrder = seq_along(1:nrow(.)))



  # create QC dataset, calculate recovery %
  QCdata <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "Accuracy/Recovery[%]"))
  QCdata <- do.call("rbind", QCdata)
  QCdata <- QCdata %>%
    rename("Recovery" = `Accuracy/Recovery[%]`) %>%
    mutate(Recovery = na_if(Recovery, "")) %>%
    filter(AnalyteName %in% analytes) %>%
    mutate(Recovery = as.numeric(Recovery)-100) %>%
    pivot_wider(names_from = AnalyteName,
                values_from = Recovery) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    left_join(AnnData) %>%
    filter(sampleType == "QC") %>%
    filter(removeSample != 1) %>%
    mutate(QC = as.numeric(gsub("(QC)([0-9]*)(.*)", "\\2", sampleID))) %>%
    pivot_longer(cols = -c(AnalysisName, sampleID:QC),
                 names_to = "Analyte",
                 values_to = "Value") %>%
    mutate(flagQC = if_else(abs(Value)>QCprop, 1, 0))


  # calculate number of QC that pass
  QCdata1 <- QCdata %>%
    group_by(PlateID, Analyte, QC) %>%
    mutate(flagQ1 = if_else(flagQC==1 & QC==1, 1, 0, NA_real_),
           flagQ2 = if_else(flagQC==1 & QC==2, 1, 0, NA_real_),
           flagQ3 = if_else(flagQC==1 & QC==3, 1, 0, NA_real_),
           flagQ4 = if_else(flagQC==1 & QC==4, 1, 0, NA_real_)) %>%
    mutate(across(c(flagQ1:flagQ4), ~sum(.)),
           flagQC = max(flagQC)) %>%
    ungroup() %>%
    select(PlateID, Analyte, flagQC:flagQ4)%>%
    distinct() %>%
    group_by(PlateID, Analyte) %>%
    mutate(flagQC = max(flagQC),
           across(c(flagQ1:flagQ4), ~max(.))) %>%
    distinct() %>%
    mutate(n = flagQ1 + flagQ2 + flagQ3 + flagQ4,
           QCinjectionpass = QCinjectionsno-n,
           QCpass = case_when(
             c(flagQ1<3 & flagQ2<2 & flagQ3<2) ~ 1,
             c(flagQ1<3 & flagQ2<2 & flagQ4<2) ~ 1,
             c(flagQ1<3 & flagQ3<2 & flagQ4<2) ~ 1,
             c(flagQ2<3 & flagQ3<2 & flagQ4<2) ~ 1,
             TRUE ~ 0),
           QCinjectionpassprop = QCinjectionpass/QCinjectionsno*100) %>%
    mutate(flagQCinjectionno = if_else(QCinjectionpassprop < QCinjectionprop, 1, 0),
           flagQCpass = if_else(QCpass == 1, 0, 1))



   # create LTR/SLTR dataset
  LTRdata <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "Quantity"))
  LTRdata <- do.call("rbind", LTRdata)
  LTRdata <- LTRdata %>%
    filter(AnalyteName %in% analytes) %>%
    pivot_wider(names_from = AnalyteName,
                values_from = Quantity) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    left_join(AnnData) %>%
    filter(sampleType %in% c("LTR", "SLTR")) %>%
    filter(removeSample != 1) %>%
    pivot_longer(cols = -c(AnalysisName, sampleID:studyOrder),
                 names_to = "Analyte",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    mutate(Value = na_if(Value, 0)) %>%
    select(-removeSample)



  # calculate CV within and between
  CVdata <- LTRdata %>%
    group_by(sampleType, PlateID, Analyte) %>%
    mutate(CVwithin = (sd(Value, na.rm=TRUE)/mean(Value, na.rm=TRUE))*100) %>%
    ungroup() %>%
    group_by(sampleType, Analyte) %>%
    mutate(CVbetween = (sd(Value, na.rm=TRUE)/mean(Value, na.rm=TRUE))*100) %>%
    ungroup() %>%
    select(sampleType, PlateID, Analyte, CVwithin, CVbetween) %>%
    distinct() %>%
    arrange(Analyte)


  CVtab <- CVdata %>%
    mutate(Title = paste0("Plate", PlateID)) %>%
    pivot_wider(id_cols = c(sampleType, Analyte, CVbetween),
                names_from = Title,
                values_from = CVwithin) %>%
    select(Analyte, sampleType, paste0("Plate", min(CVdata$PlateID)):paste0("Plate", max(CVdata$PlateID)), CVbetween)



  # create calibration dataset, determine calibrant concentration levels
  Caldata <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "Quantity", "Expected Quantity"))
  Caldata <- do.call("rbind", Caldata)
  Caldata <- Caldata %>%
    filter(AnalyteName %in% analytes) %>%
    pivot_wider(names_from = AnalyteName,
                values_from = Quantity) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName))  %>%
    left_join(AnnData) %>%
    filter(removeSample != 1) %>%
    filter(sampleType %in% c("CALIBRANT")) %>%
    mutate(CalID = as.numeric(gsub("(CAL)([0-9]*)(.*)", "\\2", sampleID))) %>%
    arrange(studyOrder) %>%
    group_by(PlateID, CalID) %>%
    mutate(CalNo = as.factor(seq_along(CalID))) %>%
    ungroup() %>%
    rename("CalConc" = `Expected Quantity`) %>%
    select(-c(removeSample)) %>%
    pivot_longer(cols = -c(AnalysisName, CalConc, sampleID:CalNo),
                 names_to = "Analyte",
                 values_to = "Value") %>%
    mutate(CalConc = as.numeric(CalConc),
           Value = as.numeric(Value)) %>%
    mutate(Value = na_if(Value, 0)) %>%
    drop_na() %>%
    mutate(ExpectedValueRatio = Value/CalConc)



  # create QC data but in a slightly different format to combine with sample data for plotting
  QCdata2 <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "Quantity", "Expected Quantity"))
  QCdata2 <- do.call("rbind", QCdata2)
  QCdata2 <- QCdata2 %>%
    filter(AnalyteName %in% analytes) %>%
    pivot_wider(names_from = AnalyteName,
                values_from = Quantity) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    left_join(AnnData) %>%
    filter(removeSample != 1) %>%
    filter(sampleType %in% c("QC")) %>%
    mutate(QC = as.numeric(gsub("(QC)([0-9]*)(.*)", "\\2", sampleID))) %>%
    arrange(studyOrder) %>%
    group_by(PlateID, QC) %>%
    mutate(QCNo = as.factor(seq_along(QC))) %>%
    ungroup() %>%
    rename("QCConc" = `Expected Quantity`) %>%
    select(-c(removeSample)) %>%
    pivot_longer(cols = -c(AnalysisName, QCConc, sampleID:QCNo),
                 names_to = "Analyte",
                 values_to = "Value") %>%
    mutate(QCConc = as.numeric(QCConc),
           Value = as.numeric(Value)) %>%
    mutate(Value = na_if(Value, 0)) %>%
    drop_na() %>%
    mutate(ExpectedValueRatio = Value/QCConc)



  # create QC summary tables
  RSDsumms <- LTRdata %>%
    filter(sampleType == "LTR") %>%
    select(PlateID, Analyte, Value) %>%
    group_by(PlateID, Analyte) %>%
    summarise(n = length(Value), sd = sd(Value), mean = mean(Value)) %>%
    mutate(RSD = (sqrt((n-1)/n)*sd)/mean*100) %>%
    mutate(flagRSD = ifelse(RSD>RSDprop, 1, 0)) %>%
    select(-c(sd, mean))

  RSDsumms1 <- LTRdata %>%
    filter(sampleType == "SLTR") %>%
    select(PlateID, Analyte, Value) %>%
    group_by(PlateID, Analyte) %>%
    summarise(n = length(Value), sd = sd(Value), mean = mean(Value)) %>%
    mutate(RSD = (sqrt((n-1)/n)*sd)/mean*100) %>%
    mutate(flagRSD = ifelse(RSD>RSDprop, 1, 0)) %>%
    select(-c(sd, mean))



  # create QC data but in a slightly different format to combine with sample data for plotting
  R2 <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "R2"))
  R2 <- do.call("rbind", R2)
  R2 <- R2 %>%
    filter(AnalyteName %in% analytes) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    left_join(AnnData) %>%
    filter(removeSample != 1) %>%
    rename("Analyte" = AnalyteName) %>%
    select(Analyte, PlateID, R2) %>%
    distinct() %>%
    mutate(flagR2 = if_else(R2 < R2prop, 1, 0))


  # create sample data
  sampleData <- lapply(da@obsDescr, "[", , c("AnalysisName", "AnalyteName", "Quantity"))
  sampleData <- do.call("rbind", sampleData)
  sampleData <- sampleData %>%
    filter(AnalyteName %in% analytes) %>%
    mutate(AnalysisName = gsub("_rerun", "", AnalysisName),
           AnalysisName = gsub("_repeat.*", "", AnalysisName)) %>%
    left_join(AnnData) %>%
    filter(removeSample != 1)%>%
    pivot_wider(names_from = AnalyteName,
                values_from = Quantity) %>%
    filter(sampleType %in% c("SAMPLE")) %>%
    select(-c(removeSample)) %>%
    pivot_longer(cols = -c(AnalysisName:sampleType, PlateID, plateOrder, studyOrder),
                 names_to = "Analyte",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    mutate(Value = na_if(Value, 0)) %>%
    drop_na()



  #save options modified in function
  func_call <- match.call()




  # create master list of results and annotation data
  # save function call, original data loaded and annotation info
  master_list <- list()
  master_list$call <- func_call
  master_list$Annotation <- AnnData

  # save processed summary data & tables
  master_list$SampleData <- sampleData
  master_list$CalibrantData <- Caldata
  master_list$QCData <- QCdata2

  # summary data
  master_list$summary$QCRecovery <- QCdata
  master_list$summary$QCPass <- QCdata1
  master_list$summary$LTRData <- LTRdata
  master_list$summary$CVtable <- CVtab
  master_list$summary$LTR_RSDsummary <- RSDsumms
  master_list$summary$SLTR_RSDsummary <- RSDsumms1
  master_list$summary$R2 <- R2


  return(master_list)

}
