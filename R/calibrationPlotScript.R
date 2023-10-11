# function to generate calibration plots
# works inside Mass Spec QC script, calling upon the produced master_list object


calPlotFunc <- function(analytes = analytes) {
  
  
  tmp <- master_list$QCData %>%
    select(sampleType, PlateID, Analyte, QCConc, Value) %>% 
    rename("x" = QCConc, "y" = Value) %>% 
    filter(Analyte == analytes) %>%
    distinct() 
  
  tmp1 <- master_list$SampleData %>%
    select(sampleType, PlateID, Analyte, Value) %>% 
    rename("y" = Value) %>% 
    mutate(x = y) %>% 
    filter(Analyte == analytes) %>%
    distinct()
  
  p1 <- master_list$CalibrantData %>%
    select(sampleType, PlateID, Analyte, CalConc, Value, CalNo) %>% 
    rename("x" = CalConc, "y" = Value) %>%
    mutate(sampleType = paste0(sampleType, "_", CalNo)) %>% 
    filter(Analyte == analytes) %>%
    full_join(tmp) %>% 
    full_join(tmp1) %>% 
    mutate(Title = paste0("Plate", PlateID, "; ", analytes)) %>%
    ggplot(aes(x = x, y = y, group = sampleType)) +
    geom_smooth(
      aes(x = x, y = x),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_smooth(
      aes(x = x, y = x * 1.2),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_smooth(
      aes(x = x, y = x * 0.8),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_point(aes(colour = sampleType, shape = sampleType, size = sampleType)) +
    theme_bw() +
    scale_colour_manual(values = c("firebrick2", "black", "blue2", "orange3")) +
    scale_shape_manual(values = c(19,19,19,4)) +
    scale_size_manual(values = c(3,3,3,2)) +
    xlab("Calibration Concentration (uM)") +
    ylab("Concentration (uM)") +
    facet_wrap(~ Title, ncol = 1, scales = "free")
  
  
  p2 <- master_list$CalibrantData %>%
    select(sampleType, PlateID, Analyte, CalConc, Value, CalNo) %>% 
    rename("x" = CalConc, "y" = Value) %>%
    mutate(sampleType = paste0(sampleType, "_", CalNo)) %>% 
    filter(Analyte == analytes) %>%
    full_join(tmp) %>% 
    full_join(tmp1) %>% 
    mutate(Title = paste0("Plate", PlateID, "; ", analytes)) %>%
    ggplot(aes(x = x, y = y, group = sampleType)) +
    geom_smooth(
      aes(x = x, y = x),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_smooth(
      aes(x = x, y = x * 1.2),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_smooth(
      aes(x = x, y = x * 0.8),
      formula = y ~ x,
      method = "lm",
      se = FALSE,
      linetype = "dashed",
      inherit.aes = FALSE,
      colour = "grey"
    ) +
    geom_point(aes(colour = sampleType, shape = sampleType, size = sampleType)) +
    theme_bw() +
    scale_colour_manual(values = c("firebrick2", "black", "blue2", "orange3")) +
    scale_shape_manual(values = c(19,19,19,4)) +
    scale_size_manual(values = c(3,3,3,2)) +
    xlab("Calibration Concentration (uM)") +
    ylab("Concentration (uM)") +
    xlim(0,20) + ylim(0,26) +
    facet_wrap(~ Title, ncol = 1, scales = "free")
  
  
  ggarrange(
    p1, p2, ncol = 2, common.legend = TRUE, legend = "top")
  
}


