rm(list=ls())
library(openxlsx)
library(stringr)
library(vcd)
library(plyr)
library(mltools)
library(data.table)
library(scoring)
library(pROC)
library(ggpubr)
library(caret)



#DIR0 <- "C:/Users/hezo/Dropbox/PhD/Stroke/Stroke_perfusion/"
DIR0 <- "C:/Users/Lisa/Dropbox/PhD/Stroke/Stroke_perfusion/"
FIG_DIR <- paste0(DIR0, "figures/")
DIR <- paste0(DIR0, "callbacks/3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet/")
DIR_TMAX <- paste0(DIR0, "callbacks/3D_CNN_ONTRAM_Bern_DWI_mrs_preprocessed_ENSEMBLE_CV_ORDINAL_Resnet_AddTMAX/")


# functions --------------------------------------------------------------------

# metrics
qwk <- function(dat, pred, true){
  nlevels <- length(levels(as.factor(dat[, true])))
  if(nlevels > 2){
    tab <- table(factor(dat[, pred], levels = c(0:6)), factor(dat[, true], levels = c(0:6)))
  } else{
    tab <- table(factor(dat[, pred], levels = c(0:1)), factor(dat[, true], levels = c(0:1)))
  }
  res <- Kappa(tab)
  return(res$Weighted["value"])
}

# for models and raters
acc <- function(dat, pred, true){
  acc <- mean(dat[, pred] == dat[, true])*100
  return(acc)
}

sens <- function(dat, pred, true){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat <- table(dat[, true], dat[, pred])
  sens <- conf_mat[2,2]/sum(conf_mat[2,])
  return(round(sens*100, 2))
}

spec <- function(dat, pred, true){
  # true: vector of true classes
  # pred: vector of predicted classes
  conf_mat <- table(dat[,true], dat[,pred])
  spec <- conf_mat[1,1]/sum(conf_mat[1,])
  return(round(spec*100,2))
}

# for models
auc <- function(dat){
  res <- roc(dat[,"y_true"], dat[, "y_pred1_fav"], levels = c(0,1), direction = "<")
  return(res$auc)
}

nll <- function(dat, nlevels){
  if(nlevels > 2){
    # ensure that values sum to exactly 1 (problem wegen zu vielen Nachkommastellen)
    dat[, "y_pred6"] <- 1-(dat[, "y_pred0"] + dat[, "y_pred1"] + dat[, "y_pred2"] + dat[, "y_pred3"] + dat[, "y_pred4"] + dat[, "y_pred5"])
    nll <- mean(logscore((dat[, "y_true"] + 1) ~ dat[, "y_pred0"] + dat[, "y_pred1"] + dat[, "y_pred2"] + dat[, "y_pred3"] + dat[, "y_pred4"] + dat[, "y_pred5"] + dat[, "y_pred6"])) 
  } else{
    nll <- mean(logscore(dat[, "y_true"] ~ (1-dat[, "y_pred1_fav"]) + dat[, "y_pred1_fav"]))
  }
  return(nll)
}

bs <- function(dat, nlevels){
  if(nlevels > 2){
    bs <- NA
  } else{
    bs <- mean(brierscore(dat[, "y_true"] ~ (1-dat[, "y_pred1_fav"]) + dat[, "y_pred1_fav"]))
  }
  return(bs)
}


# bootstrap results
bootstrap_results <- function(dat, pred, true, rater = c("rater", "model"), 
                              data_modality = c("clinical", "image", "clinical_and_image"),
                              nsamples = 1000, prob = NULL){
  # datasets = list of datasets
  # nsample = number of bootstrap samples
  acc_boot <- rep(NA, 1000)
  qwk_boot <- rep(NA, 1000)
  nll_boot <- rep(NA, 1000)
  auc_boot <- rep(NA, 1000)
  bs_boot <- rep(NA, 1000)
  sens_boot <- rep(NA, 1000)
  spec_boot <- rep(NA, 1000)
  nlevels <- length(levels(as.factor(dat[, true])))
  
  set.seed(123)
  for(i in 1:nsamples){
    # get bootstrap sample (n = ntest)
    idx <- sample(1:nrow(dat), size = 500, replace = T)
    boots <- dat[idx,]
    
    # get metrics
    acc_boot[i] <- acc(boots, pred, true)
    qwk_boot[i] <- qwk(boots, pred, true)
    if(nlevels<=2){
      sens_boot[i] <- sens(boots, pred, true)
      spec_boot[i] <- spec(boots, pred, true)
    }
    if(rater == "model"){
      nll_boot[i] <- nll(boots, nlevels)
      bs_boot[i] <- bs(boots, nlevels)
      if(nlevels<=2){
        auc_boot[i] <- auc(boots)
      }
    }
  }
  
  # get the confidence intervals
  if(rater == "rater"){
    acc_out <- round(quantile(acc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    qwk_out <- round(quantile(qwk_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    nll_out <- nll_boot
    auc_out <- auc_boot
    bs_out <- bs_boot
    sens_out <- sens_boot
    spec_out <- spec_boot
    if(nlevels<=2){
      sens_out <- round(quantile(sens_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      spec_out <- round(quantile(spec_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    }
  } else{
    acc_out <- round(quantile(acc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    qwk_out <- round(quantile(qwk_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    nll_out <- round(quantile(nll_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    auc_out <- auc_boot
    bs_out <- bs_boot
    sens_out <- sens_boot
    spec_out <- spec_boot
    if(nlevels <= 2){
      auc_out <- round(quantile(auc_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      bs_out <- round(quantile(bs_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      sens_out <- round(quantile(sens_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
      spec_out <- round(quantile(spec_boot, probs = c(0.025, 0.5, 0.975), na.rm = T), digits = 3)
    }
  }
  
  return(data.frame("acc" = acc_boot, 
                    "accl" = rep(acc_out[1], nsamples), 
                    "accm" = rep(acc_out[2], nsamples), 
                    "accu" = rep(acc_out[3], nsamples),
                    "qwk" = qwk_boot, 
                    "qwkl" = rep(qwk_out[1], nsamples), 
                    "qwkm" = rep(qwk_out[2], nsamples), 
                    "qwku" = rep(qwk_out[3], nsamples),
                    "nll" = nll_boot, 
                    "nlll" = rep(nll_out[1], nsamples), 
                    "nllm" = rep(nll_out[2], nsamples), 
                    "nllu" = rep(nll_out[3], nsamples),
                    "auc" = auc_boot, 
                    "aucl" = rep(auc_out[1], nsamples), 
                    "aucm" = rep(auc_out[2], nsamples), 
                    "aucu" = rep(auc_out[3], nsamples),
                    "bs" = bs_boot, 
                    "bsl" = rep(bs_out[1], nsamples), 
                    "bsm" = rep(bs_out[2], nsamples), 
                    "bsu" = rep(bs_out[3], nsamples),
                    "sens" = sens_boot, 
                    "sensl" = rep(sens_out[1], nsamples), 
                    "sensm" = rep(sens_out[2], nsamples), 
                    "sensu" = rep(sens_out[3], nsamples),
                    "spec" = spec_boot, 
                    "specl" = rep(spec_out[1], nsamples), 
                    "specm" = rep(spec_out[2], nsamples), 
                    "specu" = rep(spec_out[3], nsamples),
                    "data" = rep(data_modality, nsamples)))
}



# change format for plotting
long_format <- function(dat, metric){
  dat_tmp <- dat[, c(metric, paste0(metric, "l"),
                     paste0(metric, "m"), paste0(metric, "u"), 
                     "data", "method", "rater")]
  newnames <- c("boot", "ci_lower", "median", "ci_upper", "data", "method", "rater")
  arg <- setNames(newnames, names(dat_tmp))
  dat_tmp <- rename(dat_tmp, arg)
  dat_tmp$metric <- metric
  return(dat_tmp)
}


# calibration
cal_ens <- function(pdf, y_true, cuts = 12, cumulative = TRUE, pool = TRUE, emp = FALSE,
                    custom_cuts_fun = function(x) quantile(x, c(0.5, 0.9, 0.99, 0.999))
) {
  #cdf <- as.matrix(cdf)
  pdf <- as.matrix(pdf)
  K <- ncol(pdf)
  #pdf <- t(apply(cbind(0, cdf), 1, diff))
  lys_cal <- list()
  start <- 1
  if (cumulative) end <- K-1 else end <- K
  if (K == 2) start <- end <- 2
  for (cl in start:end) {
    if (cumulative) {
      yt <- factor(apply(y_true, 1, function(x) sum(x[start:cl])), levels = c(0, 1)) #levels = c(1, 0)) P(Y<y_k)
      yp <- 1 - apply(pdf, 1, function(x) sum(x[start:cl]))
    } else {
      yt <- factor(y_true[, cl], levels = c(0, 1)) #levels = c(1, 0))
      yp <- 1 - pdf[, cl]
    }
    df <- data.frame(true = yt, pred = yp)
    if (emp) {
      cuts <- custom_cuts_fun(yp)
    }
    if (K == 2) {
      lys_cal <- list(calibration(true ~ pred, data = df, cuts = cuts)$data)
    } else {
      lys_cal[[cl]] <- calibration(true ~ pred, data = df, cuts = cuts)$data
    }
  }
  tmp <- lapply(lys_cal, function(x) {
    as.matrix(x[, c("Percent", "Lower", "Upper", "Count", "midpoint")])
  })
  # ensure that the intervals with zero observations are marked with NA
  # otherwise we include those intervals (with prop = 0) into the mean
  for (i in 1:length(tmp)) {
    k <- which(tmp[[i]][,"Count"] == 0)
    tmp[[i]][k, "Percent"] <- NA
    tmp[[i]][k, "Lower"] <- NA
    tmp[[i]][k, "Upper"] <- NA
  }
  avg <- apply(simplify2array(tmp), 1:2, function(x) mean(x, na.rm = T))
  
  if (pool) {
    ret <- data.frame(bin = lys_cal[[1]]$bin,
                      prop = avg[, "Percent"]/100,
                      lwr = avg[, "Lower"]/100,
                      upr = avg[, "Upper"]/100,
                      cases = avg[, "Count"],
                      midpoint = avg[, "midpoint"]/100)
    ret$prop <- ifelse(ret$cases == 0, NA, ret$prop)
    ret$lwr <- ifelse(ret$cases == 0, NA, ret$lwr)
    ret$upr <- ifelse(ret$cases == 0, NA, ret$upr)
  } else {
    ret <- lys_cal
  }
  return(ret)
}

  
  
# Load data --------------------------------------------------------------------

# Model

# Binary outcome
dat_model_clinical <- read.csv(paste0(DIR, "SI_LSx/test_pdf_bin.csv"))
dat_model_image <- read.csv(paste0(DIR, "SI_CSb/test_pdf_bin.csv"))
dat_model_clinical_and_image <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf_bin.csv"))

# ordinal outcome
dat_model_clinical_ordinal <- read.csv(paste0(DIR, "SI_LSx/test_pdf.csv"))
dat_model_image_ordinal <- read.csv(paste0(DIR, "SI_CSb/test_pdf.csv"))
dat_model_clinical_and_image_ordinal <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf.csv"))

# TMAX perfusion maps: binary outcome
dat_model_tmax <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf_bin.csv"))
dat_model_tmax_dwi <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf_bin.csv"))

# TMAX perfusion maps: oprdinal outcome
dat_model_tmax_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf.csv"))
dat_model_tmax_dwi_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf.csv"))


# Raters

# dat_clinical
load(paste0(DIR0, "/code/data/dat_clinical_raters.R"))
# dat_image
load(paste0(DIR0, "/code/data/dat_image_raters.R"))
# dat_clinical_and_image
load(paste0(DIR0, "/code/data/dat_clinical_and_image_raters.R"))

#dat_clinical[dat_clinical$p_id_clinical == "1097032" & dat_clinical$rater == 1,"pred_clinical"]
#[dat_image$p_id_image == "1097032" & dat_image$rater == 1,"pred_image"]
#dat_clinical_and_image[dat_clinical$p_id_clinical == "1097032" & dat_clinical_and_image$rater == 1,"pred_clinical_and_image"]



# analysis ---------------------------------------------------------------------

# dat_clinical_and_image$pred_clinical_and_image <- as.numeric(dat_clinical_and_image$pred_clinical_and_image)
# 
# # get the average prediction
# dat_clinical <- dat_clinical %>% group_by(p_id_clinical) %>%
#   mutate(pred_clinical_med = round(mean(pred_clinical), digits = 0))
# dat_image <- dat_image %>% group_by(p_id_image) %>%
#   mutate(pred_image_med = round(mean(pred_image), digits = 0))
# dat_clinical_and_image <- dat_clinical_and_image %>% group_by(p_id_clinical_and_image) %>%
#   mutate(pred_clinical_and_image_med = round(mean(pred_clinical_and_image), digits = 0))
# 
# dat_clinical <- dat_clinical %>% group_by(p_id_clinical) %>%
#   mutate(pred_clinical_bin_med = round(mean(pred_clinical_bin), digits = 0))
# dat_image <- dat_image %>% group_by(p_id_image) %>%
#   mutate(pred_image_bin_med = round(mean(pred_image_bin), digits = 0))
# dat_clinical_and_image <- dat_clinical_and_image %>% group_by(p_id_clinical_and_image) %>%
#   mutate(pred_clinical_and_image_bin_med = round(mean(pred_clinical_and_image_bin), digits = 0))
# 
# dat_clinical <- as.data.frame(dat_clinical)
# dat_image <- as.data.frame(dat_image)
# dat_clinical_and_image <- as.data.frame(dat_clinical_and_image)
# 
# # results over all:
# res_clinical_all <- bootstrap_results(dat_clinical,
#                                       pred = "pred_clinical_med",
#                                       true = "mrs_3months",
#                                       rater = "rater",
#                                       data_modality = "clinical")
# res_image_all <- bootstrap_results(dat_image,
#                                    pred = "pred_image_med",
#                                    true = "mrs_3months",
#                                    rater = "rater",
#                                    data_modality = "image")
# res_clinical_and_image_all <- bootstrap_results(dat_clinical_and_image,
#                                                 pred = "pred_clinical_and_image_med",
#                                                 true = "mrs_3months",
#                                                 rater = "rater",
#                                                 data_modality = "clinical_and_image")
# 
# res_clinical_bin_all <- bootstrap_results(dat_clinical,
#                                         pred = "pred_clinical_bin_med",
#                                         true = "mrs_3months_binary",
#                                         rater = "rater",
#                                         data_modality = "clinical")
# res_image_bin_all <- bootstrap_results(dat_image,
#                                       pred = "pred_image_bin_med",
#                                       true = "mrs_3months_binary",
#                                       rater = "rater",
#                                       data_modality = "image")
# res_clinical_and_image_bin_all <- bootstrap_results(dat_clinical_and_image,
#                                                     pred = "pred_clinical_and_image_bin_med",
#                                                     true = "mrs_3months_binary",
#                                                     rater = "rater",
#                                                     data_modality = "clinical_and_image")
# 
# # results per rater:
# n_raters <- length(levels(factor(dat_clinical$rater)))
# 
# dat_raters <- data.frame()
# for(i in 1:n_raters){
#   print(i)
#   # prediction performance: accuracy & qwk
#   res_clinical <- bootstrap_results(dat_clinical[dat_clinical$rater == i,],
#                                     pred = "pred_clinical",
#                                     true = "mrs_3months",
#                                     rater = "rater",
#                                     data_modality = "clinical")
#   res_image <- bootstrap_results(dat_image[dat_image$rater == i,],
#                                  pred = "pred_image",
#                                  true = "mrs_3months",
#                                  rater = "rater",
#                                  data_modality = "image")
#   res_clinical_and_image <- bootstrap_results(dat_clinical_and_image[dat_clinical_and_image$rater == i,],
#                                               pred = "pred_clinical_and_image",
#                                               true = "mrs_3months",
#                                               rater = "rater",
#                                               data_modality = "clinical_and_image")
# 
#   res_clinical_bin <- bootstrap_results(dat_clinical[dat_clinical$rater == i,],
#                                         pred = "pred_clinical_bin",
#                                         true = "mrs_3months_binary",
#                                         rater = "rater",
#                                         data_modality = "clinical")
#   res_image_bin <- bootstrap_results(dat_image[dat_image$rater == i,],
#                                      pred = "pred_image_bin",
#                                      true = "mrs_3months_binary",
#                                      rater = "rater",
#                                      data_modality = "image")
#   res_clinical_and_image_bin <- bootstrap_results(dat_clinical_and_image[dat_clinical_and_image$rater == i,],
#                                                   pred = "pred_clinical_and_image_bin",
#                                                   true = "mrs_3months_binary",
#                                                   rater = "rater",
#                                                   data_modality = "clinical_and_image")
# 
# 
#   dat_raters <- rbind(dat_raters, data.frame(rbind(cbind(res_clinical, "method" = "ordinal"),
#                                                    cbind(res_image, "method" = "ordinal"),
#                                                    cbind(res_clinical_and_image, "method" = "ordinal"),
#                                                    cbind(res_clinical_bin, "method" = "binary"),
#                                                    cbind(res_image_bin, "method" = "binary"),
#                                                    cbind(res_clinical_and_image_bin, "method" = "binary")),
#                                              rater = i))
# }
# 
# # combine with overall prediction
# dat_raters <- rbind(dat_raters, cbind(res_clinical_all, "method" = "ordinal", "rater" = 6),
#                                 cbind(res_image_all, "method" = "ordinal", "rater" = 6),
#                                 cbind(res_clinical_and_image_all, "method" = "ordinal", "rater" = 6),
#                                 cbind(res_clinical_bin_all, "method" = "binary", "rater" = 6),
#                                 cbind(res_image_bin_all, "method" = "binary", "rater" = 6),
#                                 cbind(res_clinical_and_image_bin_all, "method" = "binary", "rater" = 6))
# 
# # rename
# dat_raters$data <- factor(dat_raters$data, levels = c("image", "clinical", "clinical_and_image"))
# dat_raters$data <- revalue(dat_raters$data, c("clinical" = "Clinical", "image" = "Imaging",
#                                               "clinical_and_image" = "Clinical+Imaging"))
# #
# 
# # results per model: all patients
# # warning because values do not exactly sum to one?
# res_model_clinical <- bootstrap_results(dat_model_clinical_ordinal,
#                                         pred = "y_pred",
#                                         true = "y_true",
#                                         rater = "model",
#                                         data_modality = "clinical")
# res_model_image <- bootstrap_results(dat_model_image_ordinal,
#                                      pred = "y_pred",
#                                      true = "y_true",
#                                      rater = "model",
#                                      data_modality = "image")
# res_model_clinical_and_image <- bootstrap_results(dat_model_clinical_and_image_ordinal,
#                                                   pred = "y_pred",
#                                                   true = "y_true",
#                                                   rater = "model",
#                                                   data_modality = "clinical_and_image")
# res_model_clinical_bin <- bootstrap_results(dat_model_clinical,
#                                             pred = "y_pred",
#                                             true = "y_true",
#                                             rater = "model",
#                                             data_modality = "clinical")
# res_model_image_bin <- bootstrap_results(dat_model_image,
#                                          pred = "y_pred",
#                                          true = "y_true",
#                                          rater = "model",
#                                          data_modality = "image")
# res_model_clinical_and_image_bin <- bootstrap_results(dat_model_clinical_and_image,
#                                                       pred = "y_pred",
#                                                       true = "y_true",
#                                                       rater = "model",
#                                                       data_modality = "clinical_and_image")
# 
# dat_models <- data.frame(rbind(cbind(res_model_clinical, "method" = "ordinal"),
#                                cbind(res_model_image, "method" = "ordinal"),
#                                cbind(res_model_clinical_and_image, "method" = "ordinal"),
#                                cbind(res_model_clinical_bin, "method" = "binary"),
#                                cbind(res_model_image_bin, "method" = "binary"),
#                                cbind(res_model_clinical_and_image_bin, "method" = "binary")),
#                          rater = 0)
# 
# # rename data
# dat_models$data <- factor(dat_models$data, levels = c("image", "clinical", "clinical_and_image"))
# dat_models$data <- revalue(dat_models$data, c("clinical" = "Clinical", "image" = "Imaging",
#                                               "clinical_and_image" = "Clinical+Imaging"))
# 
# 
# # results per model: same patients as raters
# # consider only the patients selected by the raters as well -> add leading zero to IDs
# dat_model_clinical <- dat_model_clinical[dat_model_clinical$p_id %in% as.numeric(dat_clinical$p_id_clinical),]
# dat_model_image <- dat_model_image[dat_model_image$p_id %in% as.numeric(dat_image$p_id_image),]
# dat_model_clinical_and_image <- dat_model_clinical_and_image[dat_model_clinical_and_image$p_id %in% as.numeric(dat_clinical_and_image$p_id_clinical_and_image),]
# 
# dat_model_clinical_ordinal <- dat_model_clinical_ordinal[dat_model_clinical_ordinal$p_id %in% as.numeric(dat_clinical$p_id_clinical),]
# dat_model_image_ordinal <- dat_model_image_ordinal[dat_model_image_ordinal$p_id %in% as.numeric(dat_image$p_id_image),]
# dat_model_clinical_and_image_ordinal <- dat_model_clinical_and_image_ordinal[dat_model_clinical_and_image_ordinal$p_id %in% as.numeric(dat_clinical_and_image$p_id_clinical_and_image),]
# 
# res_model_clinical2 <- bootstrap_results(dat_model_clinical_ordinal,
#                                         pred = "y_pred",
#                                         true = "y_true",
#                                         rater = "model",
#                                         data_modality = "clinical")
# res_model_image2 <- bootstrap_results(dat_model_image_ordinal,
#                                      pred = "y_pred",
#                                      true = "y_true",
#                                      rater = "model",
#                                      data_modality = "image")
# res_model_clinical_and_image2 <- bootstrap_results(dat_model_clinical_and_image_ordinal,
#                                                   pred = "y_pred",
#                                                   true = "y_true",
#                                                   rater = "model",
#                                                   data_modality = "clinical_and_image")
# res_model_clinical_bin2 <- bootstrap_results(dat_model_clinical,
#                                             pred = "y_pred",
#                                             true = "y_true",
#                                             rater = "model",
#                                             data_modality = "clinical")
# res_model_image_bin2 <- bootstrap_results(dat_model_image,
#                                          pred = "y_pred",
#                                          true = "y_true",
#                                          rater = "model",
#                                          data_modality = "image")
# res_model_clinical_and_image_bin2 <- bootstrap_results(dat_model_clinical_and_image,
#                                                       pred = "y_pred",
#                                                       true = "y_true",
#                                                       rater = "model",
#                                                       data_modality = "clinical_and_image")
# 
# dat_models2 <- data.frame(rbind(cbind(res_model_clinical2, "method" = "ordinal"),
#                                cbind(res_model_image2, "method" = "ordinal"),
#                                cbind(res_model_clinical_and_image2, "method" = "ordinal"),
#                                cbind(res_model_clinical_bin2, "method" = "binary"),
#                                cbind(res_model_image_bin2, "method" = "binary"),
#                                cbind(res_model_clinical_and_image_bin2, "method" = "binary")),
#                          rater = 0)
# 
# dat_models2$data <- factor(dat_models2$data, levels = c("image", "clinical", "clinical_and_image"))
# dat_models2$data <- revalue(dat_models2$data, c("clinical" = "Clinical", "image" = "Imaging",
#                                                 "clinical_and_image" = "Clinical+Imaging"))
# 
# 
# # results TMAX
# res_model_image_tmax <- bootstrap_results(dat_model_tmax_ordinal,
#                                           pred = "y_pred",
#                                           true = "y_true",
#                                           rater = "model",
#                                           data_modality = "tmax")
# res_model_image_tmax_dwi <- bootstrap_results(dat_model_tmax_dwi_ordinal,
#                                           pred = "y_pred",
#                                           true = "y_true",
#                                           rater = "model",
#                                           data_modality = "tmax_dwi")
# res_model_image_tmax_bin <- bootstrap_results(dat_model_tmax,
#                                           pred = "y_pred",
#                                           true = "y_true",
#                                           rater = "model",
#                                           data_modality = "tmax")
# res_model_image_tmax_dwi_bin <- bootstrap_results(dat_model_tmax_dwi,
#                                               pred = "y_pred",
#                                               true = "y_true",
#                                               rater = "model",
#                                               data_modality = "tmax_dwi")
# 
# dat_models_tmax <- data.frame(rbind(cbind(res_model_image_tmax, "method" = "ordinal"),
#                                     cbind(res_model_image_tmax_dwi, "method" = "ordinal"),
#                                     cbind(res_model_image_tmax_bin, "method" = "binary"),
#                                     cbind(res_model_image_tmax_dwi_bin, "method" = "binary"),
#                                     cbind(res_model_image, "method" = "ordinal"),
#                                     cbind(res_model_image_bin, "method" = "binary")),
#                               rater = 0)
# dat_models_tmax$data <- factor(dat_models_tmax$data, levels = c("tmax", "tmax_dwi", "image"))
# dat_models_tmax$data <- revalue(dat_models_tmax$data, c("tmax" = "TMAX", "tmax_dwi" = "DWI+TMAX", "image" = "DWI"))
# 
# ## save the results
# save(dat_raters, file = paste0(DIR0, "code/data/dat_raters.R"))
# save(dat_models, file = paste0(DIR0, "code/data/dat_models.R"))
# save(dat_models2, file = paste0(DIR0, "code/data/dat_models2.R"))
# save(dat_models_tmax, file = paste0(DIR0, "code/data/dat_models_tmax.R"))

# load results
load(paste0(DIR0, "code/data/dat_raters.R"))
load(paste0(DIR0, "code/data/dat_models.R"))
load(paste0(DIR0, "code/data/dat_models2.R"))
load(paste0(DIR0, "code/data/dat_models_tmax.R"))




# Figures Models: all patients -------------------------------------------------

dat_modelsl <- rbind(long_format(dat_models, "nll"),
                    long_format(dat_models, "qwk"),
                    long_format(dat_models, "acc"),
                    long_format(dat_models, "auc"),
                    long_format(dat_models, "bs"))

# adapt metrics: lower always better
#dat_modelsl[dat_modelsl$metric == "acc", c("boot", "ci_lower", "median", "ci_upper")] <- 1-(dat_modelsl[dat_modelsl$metric == "acc", c("boot", "ci_lower", "median", "ci_upper")]/100)
#dat_modelsl[dat_modelsl$metric == "qwk", c("boot", "ci_lower", "median", "ci_upper")] <- 1-(dat_modelsl[dat_modelsl$metric == "qwk", c("boot", "ci_lower", "median", "ci_upper")])
#dat_modelsl[dat_modelsl$metric == "auc", c("boot", "ci_lower", "median", "ci_upper")] <- 1-(dat_modelsl[dat_modelsl$metric == "auc", c("boot", "ci_lower", "median", "ci_upper")])
#dat_modelsl[dat_modelsl$metric == "nll", c("boot", "ci_lower", "median", "ci_upper")] <- -(dat_modelsl[dat_modelsl$metric == "nll", c("boot", "ci_lower", "median", "ci_upper")])
dat_modelsl$metric <- factor(dat_modelsl$metric, levels = c("nll", "acc", "qwk", "auc", "bs"))
metric_names <- c("nll" = "NLL", "acc" = "ACC", "qwk" = "QWK", "auc" = "AUC", "bs" = "BS")
dat_modelsl$method <- factor(dat_modelsl$method, levels = c("ordinal", "binary"))
dat_modelsl$data <- factor(dat_modelsl$data, levels = c("Clinical+Imaging", "Imaging", "Clinical"))


pd <- position_dodge(0.3)

# ordinal
pdf(paste0(FIG_DIR, "Results_models_ordinal_bootstrap.pdf"), height = 8, width = 10)
dat_modelsl_ord <- dat_modelsl[dat_modelsl$method == "ordinal" & dat_modelsl$metric %in% c("nll", "acc", "qwk"),]
ggplot(dat_modelsl_ord, aes(x = data, y = boot)) +
  geom_jitter(width = 0.1, size = 1, colour = "lightcoral") + 
  geom_point(position = pd, aes(x = data, y = median), size = 2) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 3, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Data") + 
  ylab("Values")
dev.off()

# binary
pdf(paste0(FIG_DIR, "Results_models_binary_bootstrap.pdf"), height = 8, width = 14)
dat_modelsl_bin <- dat_modelsl[dat_modelsl$method == "binary" & dat_modelsl$metric %in% c("nll", "acc", "auc", "bs"),]
ggplot(dat_modelsl_bin, aes(x = data, y = boot)) +
  geom_jitter(width = 0.1, size = 1, colour = "lightcoral") + 
  geom_point(position = pd, aes(x = data, y = median), size = 3) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.11) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 5, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Data") + 
  ylab("Values")
dev.off()


# model results ordinal vs. binary
dat_modelsl$method <- factor(dat_modelsl$method, levels = c("ordinal", "binary"))
method_names <- c("binary" = "Binary functional outcome", "ordinal" = "Ordinal functional outcome")

pdf(paste0(FIG_DIR, "Results_models_ordinal_binary_bootstrap.pdf"), height = 8, width = 10)
ggplot(dat_modelsl[dat_modelsl$metric == "nll",], aes(x = data, y = boot)) +
  geom_jitter(width = 0.1, size = 1, colour = "lightblue3") + 
  geom_point(position = pd, aes(x = data, y = median), size = 3) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.11) +
  coord_flip() +
  facet_wrap(~method, scales="free_x", ncol = 4, labeller = as_labeller(method_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Data") + 
  ylab("NLL")
dev.off()



# TMAX
dat_modelsl_tmax <- rbind(long_format(dat_models_tmax, "nll"),
                          long_format(dat_models_tmax, "qwk"),
                          long_format(dat_models_tmax, "acc"),
                          long_format(dat_models_tmax, "auc"),
                          long_format(dat_models_tmax, "bs"))
dat_modelsl_tmax$metric <- factor(dat_modelsl_tmax$metric, levels = c("nll", "acc", "qwk", "auc", "bs"))
metric_names <- c("nll" = "NLL", "acc" = "ACC", "qwk" = "QWK", "auc" = "AUC", "bs" = "BS")
dat_modelsl_tmax$method <- factor(dat_modelsl_tmax$method, levels = c("ordinal", "binary"))
dat_modelsl_tmax$data <- factor(dat_modelsl_tmax$data, levels = c("TMAX+DWI", "TMAX", "DWI"))


# ordinal
pdf(paste0(FIG_DIR, "Results_models_ordinal_bootstrap_TMAX.pdf"), height = 8, width = 14)
dat_modelsl_ord <- dat_modelsl_tmax[dat_modelsl_tmax$method == "ordinal" & dat_modelsl_tmax$metric %in% c("nll", "acc", "qwk"),]
ggplot(dat_modelsl_ord, aes(x = data, y = boot)) +
  geom_jitter(width = 0.1, size = 1, colour = "lightcoral") + 
  geom_point(position = pd, aes(x = data, y = median), size = 3) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.11) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 4, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Data") + 
  ylab("Values")
dev.off()

# binary
pdf(paste0(FIG_DIR, "Results_models_binary_bootstrap_TMAX.pdf"), height = 8, width = 14)
dat_modelsl_bin <- dat_modelsl_tmax[dat_modelsl_tmax$method == "binary" & dat_modelsl_tmax$metric %in% c("nll", "acc", "auc", "bs"),]
ggplot(dat_modelsl_bin, aes(x = data, y = boot)) +
  geom_jitter(width = 0.1, size = 1, colour = "lightcoral") + 
  geom_point(position = pd, aes(x = data, y = median), size = 3) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.11) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 5, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Data") + 
  ylab("Values")
dev.off()



# pdf(paste0(FIG_DIR, "Results_MODELS_NLL_mrs_ordinal_BOOTSTRAP.pdf"), height = 7, width = 8)
# ggplot(dat_models[dat_models$method == "ordinal",], aes(x = data, y = nll)) +
#   geom_jitter(width = 0.1, size = 1, colour = "lightsalmon") + 
#   geom_point(position = pd, aes(x = data, y = nllm), size = 3) +
#   geom_errorbar(position = pd, aes(ymin = nlll, ymax = nllu), width = 0.15) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("NLL") + 
#   coord_flip()
# dev.off()
# 
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_NLL_mrs_binary_BOOTSTRAP.pdf"), height = 7, width = 8)
# ggplot(dat_models[dat_models$method == "binary",], aes(x = data, y = nll)) +
#   geom_jitter(width = 0.1, size = 1, colour = "lightsalmon") + 
#   geom_point(position = pd, aes(x = data, y = nllm), size = 3) +
#   geom_errorbar(position = pd, aes(ymin = nlll, ymax = nllu), width = 0.15) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("NLL") +
#   coord_flip()
# dev.off()
# 
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_AUC_BOOTSTRAP.pdf"), height = 7, width = 8)
# pd <- position_dodge(0.3)
# ggplot(dat_models[dat_models$method == "binary",], aes(x = data, y = aucm))+
#   geom_point(position = pd, size = 3) + 
#   #geom_line(position = pd, size = 0.01) +
#   geom_linerange(position = pd, aes(ymin = aucl, ymax = aucu)) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("AUC") 
# dev.off()
# 
# 
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_Accuracy_mrs_ordinal_BOOTSTRAP.pdf"), height = 7, width = 8)
# pd <- position_dodge(0.3)
# ggplot(dat_models[dat_models$method == "ordinal",], aes(x = data, y = accm))+
#   geom_point(position = pd, size = 3) + 
#   #geom_line(position = pd, size = 0.01) +
#   geom_linerange(position = pd, aes(ymin = accl, ymax = accu)) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("Accuracy") +
#   ylim(20, 35)
# dev.off()
# 
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_Accuracy_mrs_binary_BOOTSTRAP.pdf"), height = 7, width = 8)
# pd <- position_dodge(0.3)
# ggplot(dat_models[dat_models$method == "binary",], aes(x = data, y = accm))+
#   geom_point(position = pd, size = 3) + 
#   #geom_line(position = pd, size = 0.01) +
#   geom_linerange(position = pd, aes(ymin = accl, ymax = accu)) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("Accuracy") +
#   ylim(60,80)
# dev.off()
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_QWK_mrs_ordinal_BOOTSTRAP.pdf"), height = 7, width = 8)
# pd <- position_dodge(0.3)
# ggplot(dat_models[dat_models$method == "ordinal",], aes(x = data, y = qwkm))+
#   geom_point(position = pd, size = 3) + 
#   #geom_line(position = pd, size = 0.01) +
#   geom_linerange(position = pd, aes(ymin = qwkl, ymax = qwku)) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("Quadratic Weighted Kappa") 
# dev.off()
# 
# 
# pdf(paste0(FIG_DIR, "Results_MODELS_QWK_mrs_binary_BOOTSTRAP.pdf"), height = 7, width = 8)
# pd <- position_dodge(0.3)
# ggplot(dat_models[dat_models$method == "binary",], aes(x = data, y = qwkm))+
#   geom_point(position = pd, size = 3) + 
#   #geom_line(position = pd, size = 0.01) +
#   geom_linerange(position = pd, aes(ymin = qwkl, ymax = qwku)) + 
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 12),
#         legend.title = element_text(color = "black", size = 12),
#         legend.position="bottom") +
#   xlab("Data") + 
#   ylab("Quadratic Weighted Kappa") 
# dev.off()




# Calibration

dat_model_clinical$y_true <- factor(dat_model_clinical$y_true)
dat_model_image$y_true <- factor(dat_model_image$y_true)
dat_model_clinical_and_image$y_true <- factor(dat_model_clinical_and_image$y_true)
dat_model_clinical_ordinal$y_true <- factor(dat_model_clinical_ordinal$y_true)
dat_model_image_ordinal$y_true <- factor(dat_model_image_ordinal$y_true)
dat_model_clinical_and_image_ordinal$y_true <- factor(dat_model_clinical_and_image_ordinal$y_true)

# binary
pdf <- dat_model_clinical[, c("y_pred0_unfav", "y_pred1_fav")]
dummy <- dummyVars(" ~ .", data = dat_model_clinical)
y_true <- data.frame(predict(dummy, newdata = dat_model_clinical))[, c("y_true.0", "y_true.1")]
cal_clinical <- cal_ens(pdf = pdf, y_true = y_true, cumulative = F)
pdf <- cbind(dat_model_image$y_pred0_unfav, dat_model_image$y_pred1_fav)
dummy <- dummyVars(" ~ .", data = dat_model_image)
y_true <- data.frame(predict(dummy, newdata = dat_model_image))[, c("y_true.0", "y_true.1")]
cal_image <- cal_ens(pdf = pdf, y_true = y_true, cumulative = F)
pdf <- cbind(dat_model_clinical_and_image$y_pred0_unfav, dat_model_clinical_and_image$y_pred1_fav)
dummy <- dummyVars(" ~ .", data = dat_model_clinical_and_image)
y_true <- data.frame(predict(dummy, newdata = dat_model_clinical_and_image))[, c("y_true.0", "y_true.1")]
cal_clinical_and_image <- cal_ens(pdf = pdf, y_true = y_true, cumulative = F)

# ordinal
pdf <- dat_model_clinical_ordinal[, c("y_pred0", "y_pred1", "y_pred2", "y_pred3", "y_pred4", "y_pred5", "y_pred6")]
dummy <- dummyVars(" ~ .", data = dat_model_clinical_ordinal)
y_true <- data.frame(predict(dummy, newdata = dat_model_clinical_ordinal))[, c("y_true.0", "y_true.1", "y_true.2", "y_true.3", "y_true.4", "y_true.5", "y_true.6")]
cal_clinical_ordinal <- cal_ens(pdf = pdf, y_true = y_true, cumulative = F)
pdf <- dat_model_image_ordinal[, c("y_pred0", "y_pred1", "y_pred2", "y_pred3", "y_pred4", "y_pred5", "y_pred6")]
dummy <- dummyVars(" ~ .", data = dat_model_image_ordinal, cumulative = F)
y_true <- data.frame(predict(dummy, newdata = dat_model_image_ordinal))[, c("y_true.0", "y_true.1", "y_true.2", "y_true.3", "y_true.4", "y_true.5", "y_true.6")]
cal_image_ordinal <- cal_ens(pdf = pdf, y_true = y_true)
pdf <- dat_model_clinical_and_image_ordinal[, c("y_pred0", "y_pred1", "y_pred2", "y_pred3", "y_pred4", "y_pred5", "y_pred6")]
dummy <- dummyVars(" ~ .", data = dat_model_clinical_and_image_ordinal)
y_true <- data.frame(predict(dummy, newdata = dat_model_clinical_and_image_ordinal))[, c("y_true.0", "y_true.1", "y_true.2", "y_true.3", "y_true.4", "y_true.5", "y_true.6")]
cal_clinical_and_image_ordinal <- cal_ens(pdf = pdf, y_true = y_true, cumulative = F)

# plot calibration
dat_calib <- rbind(data.frame(cal_clinical, method = "Binary", data = "Clinical"),
                   data.frame(cal_image, method = "Binary", data = "Imaging"),
                   data.frame(cal_clinical_and_image, method = "Binary", data = "Clinical+Imaging"),
                   data.frame(cal_clinical_ordinal, method = "Ordinal", data = "Clinical"),
                   data.frame(cal_image_ordinal, method = "Ordinal", data = "Imaging"),
                   data.frame(cal_clinical_and_image_ordinal, method = "Ordinal", data = "Clinical+Imaging"))
dat_calib$data <- factor(dat_calib$data, levels = c("Clinical", "Imaging", "Clinical+Imaging"))
dat_calib$method <- factor(dat_calib$method, levels = c("Ordinal", "Binary"))


pdf(paste0(FIG_DIR, "Calibration_models_ordinal_binary_bootstrap.pdf"), height = 8, width = 14)
ggplot(dat_calib, aes(x = midpoint, y = prop)) +
  geom_abline(intercept = 0, slope = 1, colour = "darkgrey") +
  geom_line(size = 1, colour = "lightblue3") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 0.1, width = 0, colour = "lightblue3") +
  geom_point(size = 1) +
  facet_wrap(~ method + data, scales="free_x", ncol = 3, 
             labeller = labeller(method = ~ paste(., "functional outcome"),
                                 data = ~ paste("Data:", .))) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Predicted probability") + 
  ylab("Observed proportion")
dev.off()


pdf(paste0(FIG_DIR, "Calibration_models_ordinal_bootstrap.pdf"), height = 8, width = 14)
ggplot(dat_calib[dat_calib$method == "Ordinal",], aes(x = midpoint, y = prop)) +
  geom_abline(intercept = 0, slope = 1, colour = "darkgrey") +
  geom_line(size = 1, colour = "lightblue3") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 0.1, width = 0, colour = "lightblue3") +
  geom_point(size = 1) +
  facet_wrap(~data, scales="free_x", ncol = 3) + #, labeller = labeller(data = ~ paste("Data:", .))) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Predicted probability") + 
  ylab("Observed proportion")
dev.off()



pdf(paste0(FIG_DIR, "Calibration_models_binary_bootstrap.pdf"), height = 8, width = 14)
ggplot(dat_calib[dat_calib$method == "Binary",], aes(x = midpoint, y = prop)) +
  geom_abline(intercept = 0, slope = 1, colour = "darkgrey") +
  geom_line(size = 1, colour = "lightblue3") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), size = 0.1, width = 0, colour = "lightblue3") +
  geom_point(size = 1) +
  facet_wrap(~data, scales="free_x", ncol = 3) + #, labeller = labeller(data = ~ paste("Data:", .))) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain")) +
  xlab("Predicted probability") + 
  ylab("Observed proportion")
dev.off()




# Figures models vs. raters ----------------------------------------------------

# long format for raters
dat_ratersl <- rbind(long_format(dat_raters, "qwk"),
                     long_format(dat_raters, "acc"),
                     long_format(dat_raters, "auc"),
                     long_format(dat_raters, "sens"),
                     long_format(dat_raters, "spec"))


# consider only the patients from the model which were also available for raters
dat_modelsl2 <- rbind(long_format(dat_models2, "qwk"),
                      long_format(dat_models2, "acc"),
                      long_format(dat_models2, "auc"),
                      long_format(dat_models2, "sens"),
                      long_format(dat_models2, "spec"))

# combine raters with model results
dat_models_raters <- rbind(dat_modelsl2, dat_ratersl)
# dat_models_raters$rater_avg <- ifelse(dat_models_raters$rater == 0, 0, 1)
# dat_models_raters$rater_avg <- factor(dat_models_raters$rater_avg, levels = c(0,1))
dat_models_raters$rater <- factor(dat_models_raters$rater, levels = c(0:6))

dat_models_raters$metric <- factor(dat_models_raters$metric, levels = c("acc", "qwk", "auc", "sens", "spec"))
metric_names <- c("acc" = "ACC", "qwk" = "QWK", "auc" = "AUC", "sens" = "SENS", "spec" = "SPEC")
dat_models_raters$data <- factor(dat_models_raters$data, levels = c("Clinical+Imaging", "Imaging", "Clinical"))
dat_models_raters$method <- factor(dat_models_raters$method, levels = c("ordinal", "binary"))

# # average raters
# 
# dat_models_raters <- dat_models_raters %>%
#   group_by(metric, data, method, rater_avg)
# dat_models_raters <- dat_models_raters %>%
#   mutate(m_ci_lower = mean(ci_lower),
#          m_ci_upper = mean(ci_upper),
#          m_median = mean(median))
# dat_models_raters$m_ci_lower[dat_models_raters$rater == 0] <- NA
# dat_models_raters$m_ci_upper[dat_models_raters$rater == 0] <- NA
# dat_models_raters$m_median[dat_models_raters$rater == 0] <- NA
# dat_models_raters <- as.data.frame(dat_models_raters)

pd <- position_dodge(0.4)

# ordinal
pdf(paste0(FIG_DIR, "Results_models_raters_ordinal_bootstrap.pdf"), height = 8, width = 10)
dat_models_raters_ord <- dat_models_raters[dat_models_raters$method == "ordinal" & dat_models_raters$metric %in% c("acc", "qwk"),]
ggplot(dat_models_raters_ord, aes(x = data, y = median, group = rater, color = rater)) +
  geom_jitter(aes(shape = rater), position = pd, size = 2) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.05) +
  #geom_point(aes(x = data, y = m_median), colour = "lightblue4", size = 2) +
  #geom_errorbar(aes(ymin = m_ci_lower, ymax = m_ci_upper), colour = "lightblue4", width = 0.05) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 2, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  scale_colour_manual(breaks = c("0", "1", "6"),
                      values = c("0" = "lightcoral",
                                 "1" = "lightblue3",
                                 "2" = "lightblue3",
                                 "3" = "lightblue3",
                                 "4" = "lightblue3",
                                 "5" = "lightblue3",
                                 "6" = "lightblue4"),
                      labels = c("Model", "Raters","Raters AVG"),
                      name = " ") +
  scale_shape_manual(breaks = c("0", "1", "6"),
                     values = c("0" = 16,
                                "1" = 8,
                                "2" = 0,
                                "3" = 2,
                                "4" = 5,
                                "5" = 4,
                                "6" = 16),
                     labels = c("Model", "Raters", "Raters AVG")) +
  xlab("Data") + 
  ylab("Values") +
  guides(colour = guide_legend(nrow = 1), shape = "none")
dev.off()

unique(dat_models_raters$median[dat_models_raters$metric == "acc" & dat_models_raters$rater == 1 & dat_models_raters$method == "binary"])



pdf(paste0(FIG_DIR, "Results_models_raters_binary_bootstrap.pdf"), height = 8, width = 10)
dat_models_raters_bin <- dat_models_raters[dat_models_raters$method == "binary" & dat_models_raters$metric %in% c("acc", "sens", "spec"),]
ggplot(dat_models_raters_bin, aes(x = data, y = median, group = rater, color = rater)) +
  geom_jitter(aes(shape = rater), position = pd, size = 2) +
  geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.05) +
  #geom_point(aes(x = data, y = m_median), colour = "lightblue4", size = 2) +
  #geom_errorbar(aes(ymin = m_ci_lower, ymax = m_ci_upper), colour = "lightblue4", width = 0.05) +
  coord_flip() +
  facet_wrap(~metric, scales="free_x", ncol = 3, labeller = as_labeller(metric_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  scale_colour_manual(breaks = c("0", "1", "6"),
                      values = c("0" = "lightcoral",
                                 "1" = "lightblue3",
                                 "2" = "lightblue3",
                                 "3" = "lightblue3",
                                 "4" = "lightblue3",
                                 "5" = "lightblue3",
                                 "6" = "lightblue4"),
                      labels = c("Model", "Raters","Raters AVG"),
                      name = " ") +
  scale_shape_manual(breaks = c("0", "1", "6"),
                     values = c("0" = 16,
                                "1" = 8,
                                "2" = 0,
                                "3" = 2,
                                "4" = 5,
                                "5" = 4,
                                "6" = 16),
                     labels = c("Model", "Raters", "Raters AVG")) +
  xlab("Data") + 
  ylab("Values") +
  guides(colour = guide_legend(nrow = 1), shape = "none")
dev.off()




### Estimates ------------------------------------------------------------------

# importance: standardized coefficients
get_estimates <- function(mod_name){
  est <- data.frame()
  for(i in 0:4){
    esti <- read.table(paste0(DIR, mod_name, "/fold", i, "/estimates.csv"), 
                       sep =",", header = T)
    esti <- exp(esti)
    esti <- esti[,!grepl("intercept", colnames(esti))]
    esti$fold = i
    est <- rbind(est, esti)
  }
  lest <- reshape2::melt(est, id.vars = c("fold"))
  # get average prediction
  mest <- apply(est[,1:(ncol(est)-1)], 2, mean)
  mest <- data.frame(mest)
  
  lest <- merge(lest, mest, by.x = "variable", by.y = 0)
  lest <- lest[order(abs(lest$mest), decreasing = TRUE),]
  
  lest$variable <- factor(lest$variable, levels = unique(lest$variable))
  lest$variable <- revalue(lest$variable, replace = c("nihss_bl" = "NIHSS",
                                                      "age" = "Age",
                                                      "sys_bloodpressure_bl" = "SBP",
                                                      "time_to_groin_puncture" = "Time",
                                                      "rf_diabetes" = "Diabetes",
                                                      "rf_hypertonia" = "Hypertension",
                                                      "rf_tia_stroke" = "Prior stroke",
                                                      "lyse" = "IVT",
                                                      "rf_smoker" = "Smoking"))
  lest$fold <- factor(lest$fold)
  return(lest)
}

est_clinical <- get_estimates("SI_LSx")
est_clinical_and_image <- get_estimates("SI_LSx_CSb")
est_clinical$model <- "clinical"
est_clinical_and_image$model <- "clinical_and_image"
est <- rbind(est_clinical, est_clinical_and_image)

model_names <- c("clinical" = "Clinical", "clinical_and_image" = "Clinical+Imaging")

# one standard unit increase
pd <- position_dodge(0.4)
pdf(paste0(FIG_DIR,"ESTIMATES_importance.pdf"), height = 8, width = 12)
ggplot(est, aes(x = variable, y = value, group = fold, color = fold)) +
  geom_hline(yintercept=1, linetype = "dashed", color = "darkgrey") + 
  geom_point(position = pd) + 
  geom_point(aes(x = variable, y = mest), col = "black", fill = "black", shape = 24, size = 3) +
  facet_wrap(~model, ncol = 1, labeller = as_labeller(model_names)) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  scale_colour_manual(values = c("0" = "lightblue4",
                                 "1" = "lightcoral",
                                 "2" = "plum3",
                                 "3" = "lavenderblush3",
                                 "4" = "lightblue3"),
                      name = "CV") +
  xlab(" ") + 
  ylab("Odds ratio") 
dev.off()



# one standard unit increase
pdf(paste0(FIG_DIR,"ESTIMATES_importance_SI_LSx.pdf"), height = 8, width = 12)
ggplot(est_clinical, aes(x = variable, y = value, group = fold, color = fold)) +
  geom_hline(yintercept=1, linetype = "dashed", color = "darkgrey") + 
  geom_point(position = pd) + 
  geom_point(aes(x = variable, y = mest), col = "black", fill = "black", shape = 24, size = 3) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  scale_colour_manual(values = c("0" = "lightblue4",
                                 "1" = "lightcoral",
                                 "2" = "plum3",
                                 "3" = "lavenderblush3",
                                 "4" = "lightblue3"),
                      name = "CV") +
  xlab(" ") + 
  ylab("Odds ratio") 
dev.off()



# one standard unit increase
pdf(paste0(FIG_DIR,"ESTIMATES_importance_SI_LSx_CSb.pdf"), height = 8, width = 12)
ggplot(est_clinical_and_image, aes(x = variable, y = value, group = fold, color = fold)) +
  geom_hline(yintercept=1, linetype = "dashed", color = "darkgrey") + 
  geom_point(position = pd) + 
  geom_point(aes(x = variable, y = mest), col = "black", fill = "black", shape = 24, size = 3) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, hjust = 0.5, face = "plain"),
        axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
        legend.background = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position="bottom",
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
  scale_colour_manual(values = c("0" = "lightblue4",
                                 "1" = "lightcoral",
                                 "2" = "plum3",
                                 "3" = "lavenderblush3",
                                 "4" = "lightblue3"),
                      name = "CV") +
  xlab(" ") + 
  ylab("Odds ratio") 
dev.off()




# Interrater reliability -------------------------------------------

data("diagnoses", package = "irr")
head(diagnoses[, 1:3])

# interrater: how consistent are raters within one mode
irr_clinical <- reshape2::dcast(dat_clinical, p_id_clinical ~ rater, value.var="pred_clinical")[,-1]
irr_image <- reshape2::dcast(dat_image, p_id_image ~ rater, value.var="pred_image")[,-1]
irr_clinical_and_image <- reshape2::dcast(dat_clinical_and_image, p_id_clinical_and_image ~ rater, value.var="pred_clinical_and_image")[,-1]

kappam.fleiss(irr_clinical)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.0697 
# 
#         z = 3.23 
#   p-value = 0.00122 
kappam.fleiss(irr_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 49 
#    Raters = 5 
#     Kappa = 0.0559 
# 
#         z = 2.5 
#   p-value = 0.0124 
kappam.fleiss(irr_clinical_and_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.0719 
# 
#         z = 3.22 
#   p-value = 0.00127 


irr_clinical <- reshape2::dcast(dat_clinical, p_id_clinical ~ rater, value.var="pred_clinical_bin")[,-1]
irr_image <- reshape2::dcast(dat_image, p_id_image ~ rater, value.var="pred_image_bin")[,-1]
irr_clinical_and_image <- reshape2::dcast(dat_clinical_and_image, p_id_clinical_and_image ~ rater, value.var="pred_clinical_and_image_bin")[,-1]

kappam.fleiss(irr_clinical)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.205 
# 
#         z = 4.58 
#   p-value = 4.69e-06 
kappam.fleiss(irr_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 49 
#    Raters = 5 
#     Kappa = 0.134 
# 
#         z = 2.96 
#   p-value = 0.00312 
kappam.fleiss(irr_clinical_and_image)
# Fleiss' Kappa for m Raters
# 
#  Subjects = 50 
#    Raters = 5 
#     Kappa = 0.216 
# 
#         z = 4.82 
#   p-value = 1.44e-06