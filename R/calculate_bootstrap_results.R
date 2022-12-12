rm(list=ls())
library(plyr)
library(dplyr)
#library(plyr)


# Define directories and load functions ----------------------------------------

DIR0 <- "C:/Users/Lisa/Dropbox/PhD/Stroke/Stroke_perfusion/"
# <- paste0(DIR0, "figures/")
DIR <- paste0(DIR0, "callbacks/ontrams_3d_resent_clinical_dwi/")
DIR_TMAX <- paste0(DIR0, "callbacks/ontrams_3d_resent_dwi_tmax/")


# Import functions
#source(paste0(DIR0, "code/R/functions/metrics.R"))
#source(paste0(DIR0, "code/R/functions/calibration.R"))
source(paste0(DIR0, "code/R/functions/bootstrapping.R"))


# Load data --------------------------------------------------------------------

# Models

# Binary outcome
dat_model_clinical <- read.csv(paste0(DIR, "SI_LSx/test_pdf_bin.csv"))
dat_model_image <- read.csv(paste0(DIR, "SI_CSb/test_pdf_bin.csv"))
dat_model_clinical_and_image <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf_bin.csv"))

# Ordinal outcome
dat_model_clinical_ordinal <- read.csv(paste0(DIR, "SI_LSx/test_pdf.csv"))
dat_model_image_ordinal <- read.csv(paste0(DIR, "SI_CSb/test_pdf.csv"))
dat_model_clinical_and_image_ordinal <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf.csv"))

# TMAX perfusion maps: Binary outcome
dat_model_tmax <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf_bin.csv"))
dat_model_tmax_dwi <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf_bin.csv"))

# TMAX perfusion maps: Ordinal outcome
dat_model_tmax_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf.csv"))
dat_model_tmax_dwi_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf.csv"))


# Raters

# dat_clinical
load(paste0(DIR0, "/code/data/dat_clinical_raters.R"))
# dat_image
load(paste0(DIR0, "/code/data/dat_image_raters.R"))
# dat_clinical_and_image
load(paste0(DIR0, "/code/data/dat_clinical_and_image_raters.R"))




# Analysis ---------------------------------------------------------------------


dat_clinical_and_image$pred_clinical_and_image <- as.numeric(dat_clinical_and_image$pred_clinical_and_image)

# For each patient, calculate the average prediction across the raters

# for ordinal outcomes
dat_clinical <- dat_clinical %>% group_by(p_id_clinical) %>%
  mutate(pred_clinical_med = round(mean(pred_clinical), digits = 0))
dat_image <- dat_image %>% group_by(p_id_image) %>%
  mutate(pred_image_med = round(mean(pred_image), digits = 0))
dat_clinical_and_image <- dat_clinical_and_image %>% group_by(p_id_clinical_and_image) %>%
  mutate(pred_clinical_and_image_med = round(mean(pred_clinical_and_image), digits = 0))

# for binary outcomes
dat_clinical <- dat_clinical %>% group_by(p_id_clinical) %>%
  mutate(pred_clinical_bin_med = round(mean(pred_clinical_bin), digits = 0))
dat_image <- dat_image %>% group_by(p_id_image) %>%
  mutate(pred_image_bin_med = round(mean(pred_image_bin), digits = 0))
dat_clinical_and_image <- dat_clinical_and_image %>% group_by(p_id_clinical_and_image) %>%
  mutate(pred_clinical_and_image_bin_med = round(mean(pred_clinical_and_image_bin), digits = 0))

dat_clinical <- as.data.frame(dat_clinical)
dat_image <- as.data.frame(dat_image)
dat_clinical_and_image <- as.data.frame(dat_clinical_and_image)






# Get bootstrap results for the average prediction of the raters:

# Ordinal
res_clinical_all <- bootstrap_results(dat_clinical,
                                      pred = "pred_clinical_med",
                                      true = "mrs_3months",
                                      rater = "rater",
                                      data_modality = "clinical")
res_image_all <- bootstrap_results(dat_image,
                                   pred = "pred_image_med",
                                   true = "mrs_3months",
                                   rater = "rater",
                                   data_modality = "image")
res_clinical_and_image_all <- bootstrap_results(dat_clinical_and_image,
                                                pred = "pred_clinical_and_image_med",
                                                true = "mrs_3months",
                                                rater = "rater",
                                                data_modality = "clinical_and_image")

# Binary
res_clinical_bin_all <- bootstrap_results(dat_clinical,
                                        pred = "pred_clinical_bin_med",
                                        true = "mrs_3months_binary",
                                        rater = "rater",
                                        data_modality = "clinical")
res_image_bin_all <- bootstrap_results(dat_image,
                                      pred = "pred_image_bin_med",
                                      true = "mrs_3months_binary",
                                      rater = "rater",
                                      data_modality = "image")
res_clinical_and_image_bin_all <- bootstrap_results(dat_clinical_and_image,
                                                    pred = "pred_clinical_and_image_bin_med",
                                                    true = "mrs_3months_binary",
                                                    rater = "rater",
                                                    data_modality = "clinical_and_image")


# Get bootstrap results for the predictions for each rater:
n_raters <- length(levels(factor(dat_clinical$rater)))
dat_raters <- data.frame()
for(i in 1:n_raters){
  # print(i)
  
  # ordinal 
  res_clinical <- bootstrap_results(dat_clinical[dat_clinical$rater == i,],
                                    pred = "pred_clinical",
                                    true = "mrs_3months",
                                    rater = "rater",
                                    data_modality = "clinical")
  res_image <- bootstrap_results(dat_image[dat_image$rater == i,],
                                 pred = "pred_image",
                                 true = "mrs_3months",
                                 rater = "rater",
                                 data_modality = "image")
  res_clinical_and_image <- bootstrap_results(dat_clinical_and_image[dat_clinical_and_image$rater == i,],
                                              pred = "pred_clinical_and_image",
                                              true = "mrs_3months",
                                              rater = "rater",
                                              data_modality = "clinical_and_image")

  # binary
  res_clinical_bin <- bootstrap_results(dat_clinical[dat_clinical$rater == i,],
                                        pred = "pred_clinical_bin",
                                        true = "mrs_3months_binary",
                                        rater = "rater",
                                        data_modality = "clinical")
  res_image_bin <- bootstrap_results(dat_image[dat_image$rater == i,],
                                     pred = "pred_image_bin",
                                     true = "mrs_3months_binary",
                                     rater = "rater",
                                     data_modality = "image")
  res_clinical_and_image_bin <- bootstrap_results(dat_clinical_and_image[dat_clinical_and_image$rater == i,],
                                                  pred = "pred_clinical_and_image_bin",
                                                  true = "mrs_3months_binary",
                                                  rater = "rater",
                                                  data_modality = "clinical_and_image")

  # Combine results in one dataframe
  dat_raters <- rbind(dat_raters, data.frame(rbind(cbind(res_clinical, "method" = "ordinal"),
                                                   cbind(res_image, "method" = "ordinal"),
                                                   cbind(res_clinical_and_image, "method" = "ordinal"),
                                                   cbind(res_clinical_bin, "method" = "binary"),
                                                   cbind(res_image_bin, "method" = "binary"),
                                                   cbind(res_clinical_and_image_bin, "method" = "binary")),
                                             rater = i))
}

# Combine bootstrap results for the single raters and the average prediction
dat_raters <- rbind(dat_raters, cbind(res_clinical_all, "method" = "ordinal", "rater" = 6),
                    cbind(res_image_all, "method" = "ordinal", "rater" = 6),
                    cbind(res_clinical_and_image_all, "method" = "ordinal", "rater" = 6),
                    cbind(res_clinical_bin_all, "method" = "binary", "rater" = 6),
                    cbind(res_image_bin_all, "method" = "binary", "rater" = 6),
                    cbind(res_clinical_and_image_bin_all, "method" = "binary", "rater" = 6))

# rename
dat_raters$data <- factor(dat_raters$data, levels = c("image", "clinical", "clinical_and_image"))
dat_raters$data <- revalue(dat_raters$data, c("clinical" = "Clinical", "image" = "Imaging",
                                              "clinical_and_image" = "Clinical+Imaging"))





# Get bootstrap results for the models based on DWI:

# 1) Results per model for all patients (five fold CV)

# Ordinal
res_model_clinical <- bootstrap_results(dat_model_clinical_ordinal,
                                        pred = "y_pred",
                                        true = "y_true",
                                        rater = "model",
                                        data_modality = "clinical")
# warning because values do not exactly sum to one 
# (rounding issue on the 10th digit or so)
res_model_image <- bootstrap_results(dat_model_image_ordinal,
                                     pred = "y_pred",
                                     true = "y_true",
                                     rater = "model",
                                     data_modality = "image")
res_model_clinical_and_image <- bootstrap_results(dat_model_clinical_and_image_ordinal,
                                                  pred = "y_pred",
                                                  true = "y_true",
                                                  rater = "model",
                                                  data_modality = "clinical_and_image")

# Binary
res_model_clinical_bin <- bootstrap_results(dat_model_clinical,
                                            pred = "y_pred",
                                            true = "y_true",
                                            rater = "model",
                                            data_modality = "clinical")
res_model_image_bin <- bootstrap_results(dat_model_image,
                                         pred = "y_pred",
                                         true = "y_true",
                                         rater = "model",
                                         data_modality = "image")
res_model_clinical_and_image_bin <- bootstrap_results(dat_model_clinical_and_image,
                                                      pred = "y_pred",
                                                      true = "y_true",
                                                      rater = "model",
                                                      data_modality = "clinical_and_image")

dat_models <- data.frame(rbind(cbind(res_model_clinical, "method" = "ordinal"),
                               cbind(res_model_image, "method" = "ordinal"),
                               cbind(res_model_clinical_and_image, "method" = "ordinal"),
                               cbind(res_model_clinical_bin, "method" = "binary"),
                               cbind(res_model_image_bin, "method" = "binary"),
                               cbind(res_model_clinical_and_image_bin, "method" = "binary")),
                         rater = 0)

# rename data
dat_models$data <- factor(dat_models$data, levels = c("image", "clinical", "clinical_and_image"))
dat_models$data <- revalue(dat_models$data, c("clinical" = "Clinical", "image" = "Imaging",
                                              "clinical_and_image" = "Clinical+Imaging"))


# 2) Results per model for the same patients as the raters

# consider only the patients selected by the raters as well -> add leading zero to IDs
dat_model_clinical <- dat_model_clinical[dat_model_clinical$p_id %in% as.numeric(dat_clinical$p_id_clinical),]
dat_model_image <- dat_model_image[dat_model_image$p_id %in% as.numeric(dat_image$p_id_image),]
dat_model_clinical_and_image <- dat_model_clinical_and_image[dat_model_clinical_and_image$p_id %in% as.numeric(dat_clinical_and_image$p_id_clinical_and_image),]

dat_model_clinical_ordinal <- dat_model_clinical_ordinal[dat_model_clinical_ordinal$p_id %in% as.numeric(dat_clinical$p_id_clinical),]
dat_model_image_ordinal <- dat_model_image_ordinal[dat_model_image_ordinal$p_id %in% as.numeric(dat_image$p_id_image),]
dat_model_clinical_and_image_ordinal <- dat_model_clinical_and_image_ordinal[dat_model_clinical_and_image_ordinal$p_id %in% as.numeric(dat_clinical_and_image$p_id_clinical_and_image),]

# Ordinal
res_model_clinical2 <- bootstrap_results(dat_model_clinical_ordinal,
                                        pred = "y_pred",
                                        true = "y_true",
                                        rater = "model",
                                        data_modality = "clinical")
res_model_image2 <- bootstrap_results(dat_model_image_ordinal,
                                     pred = "y_pred",
                                     true = "y_true",
                                     rater = "model",
                                     data_modality = "image")
res_model_clinical_and_image2 <- bootstrap_results(dat_model_clinical_and_image_ordinal,
                                                  pred = "y_pred",
                                                  true = "y_true",
                                                  rater = "model",
                                                  data_modality = "clinical_and_image")

# Binary
res_model_clinical_bin2 <- bootstrap_results(dat_model_clinical,
                                            pred = "y_pred",
                                            true = "y_true",
                                            rater = "model",
                                            data_modality = "clinical")
res_model_image_bin2 <- bootstrap_results(dat_model_image,
                                         pred = "y_pred",
                                         true = "y_true",
                                         rater = "model",
                                         data_modality = "image")
res_model_clinical_and_image_bin2 <- bootstrap_results(dat_model_clinical_and_image,
                                                      pred = "y_pred",
                                                      true = "y_true",
                                                      rater = "model",
                                                      data_modality = "clinical_and_image")

dat_models2 <- data.frame(rbind(cbind(res_model_clinical2, "method" = "ordinal"),
                               cbind(res_model_image2, "method" = "ordinal"),
                               cbind(res_model_clinical_and_image2, "method" = "ordinal"),
                               cbind(res_model_clinical_bin2, "method" = "binary"),
                               cbind(res_model_image_bin2, "method" = "binary"),
                               cbind(res_model_clinical_and_image_bin2, "method" = "binary")),
                         rater = 0)

dat_models2$data <- factor(dat_models2$data, levels = c("image", "clinical", "clinical_and_image"))
dat_models2$data <- revalue(dat_models2$data, c("clinical" = "Clinical", "image" = "Imaging",
                                                "clinical_and_image" = "Clinical+Imaging"))






# Get bootstrap results for the models based on TMAX/DWI:

# Ordinal
res_model_image_tmax <- bootstrap_results(dat_model_tmax_ordinal,
                                          pred = "y_pred",
                                          true = "y_true",
                                          rater = "model",
                                          data_modality = "tmax")
res_model_image_tmax_dwi <- bootstrap_results(dat_model_tmax_dwi_ordinal,
                                          pred = "y_pred",
                                          true = "y_true",
                                          rater = "model",
                                          data_modality = "tmax_dwi")

# Binary
res_model_image_tmax_bin <- bootstrap_results(dat_model_tmax,
                                          pred = "y_pred",
                                          true = "y_true",
                                          rater = "model",
                                          data_modality = "tmax")
res_model_image_tmax_dwi_bin <- bootstrap_results(dat_model_tmax_dwi,
                                              pred = "y_pred",
                                              true = "y_true",
                                              rater = "model",
                                              data_modality = "tmax_dwi")

# Combine
dat_models_tmax <- data.frame(rbind(cbind(res_model_image_tmax, "method" = "ordinal"),
                                    cbind(res_model_image_tmax_dwi, "method" = "ordinal"),
                                    cbind(res_model_image_tmax_bin, "method" = "binary"),
                                    cbind(res_model_image_tmax_dwi_bin, "method" = "binary"),
                                    cbind(res_model_image, "method" = "ordinal"),
                                    cbind(res_model_image_bin, "method" = "binary")),
                              rater = 0)
dat_models_tmax$data <- factor(dat_models_tmax$data, levels = c("tmax", "tmax_dwi", "image"))
dat_models_tmax$data <- revalue(dat_models_tmax$data, c("tmax" = "TMAX", "tmax_dwi" = "DWI+TMAX", "image" = "DWI"))





# Save all results
save(dat_raters, file = paste0(DIR0, "code/data/dat_raters.R"))
save(dat_models, file = paste0(DIR0, "code/data/dat_models.R"))
save(dat_models2, file = paste0(DIR0, "code/data/dat_models2.R"))
save(dat_models_tmax, file = paste0(DIR0, "code/data/dat_models_tmax.R"))
