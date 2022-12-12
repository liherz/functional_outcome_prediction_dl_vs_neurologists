rm(list=ls())
library(ggplot2)
library(caret)


DIR0 <- "C:/Users/Lisa/Dropbox/PhD/Stroke/Stroke_perfusion/"
FIG_DIR <- paste0(DIR0, "figures/")
DIR <- paste0(DIR0, "callbacks/ontrams_3d_resent_clinical_dwi/")
DIR_TMAX <- paste0(DIR0, "callbacks/ontrams_3d_resent_dwi_tmax/")


# Import functions
source(paste0(DIR0, "code/R/functions/helper.R"))
source(paste0(DIR0, "code/R/functions/calibration.R"))


# Load data --------------------------------------------------------------------

# Models

# Model DWI: Binary outcome
dat_model_clinical <- read.csv(paste0(DIR, "SI_LSx/test_pdf_bin.csv"))
dat_model_image <- read.csv(paste0(DIR, "SI_CSb/test_pdf_bin.csv"))
dat_model_clinical_and_image <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf_bin.csv"))

# Model DWI: Ordinal outcome
dat_model_clinical_ordinal <- read.csv(paste0(DIR, "SI_LSx/test_pdf.csv"))
dat_model_image_ordinal <- read.csv(paste0(DIR, "SI_CSb/test_pdf.csv"))
dat_model_clinical_and_image_ordinal <- read.csv(paste0(DIR, "SI_LSx_CSb/test_pdf.csv"))

# Model TMAX perfusion maps: Binary outcome
dat_model_tmax <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf_bin.csv"))
dat_model_tmax_dwi <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf_bin.csv"))

# Model TMAX perfusion maps: Ordinal outcome
dat_model_tmax_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX/test_pdf.csv"))
dat_model_tmax_dwi_ordinal <- read.csv(paste0(DIR_TMAX, "SI_CSb_TMAX_DWI/test_pdf.csv"))


# Raters

# dat_clinical
load(paste0(DIR0, "/code/data/dat_clinical_raters.R"))

# dat_image
load(paste0(DIR0, "/code/data/dat_image_raters.R"))

# dat_clinical_and_image
load(paste0(DIR0, "/code/data/dat_clinical_and_image_raters.R"))


# Bootstrap results: Created with R file: bootstrapping

load(paste0(DIR0, "code/data/dat_raters.R"))
load(paste0(DIR0, "code/data/dat_models.R"))
load(paste0(DIR0, "code/data/dat_models2.R"))
load(paste0(DIR0, "code/data/dat_models_tmax.R"))




# Figures Models: 5-fold CV ----------------------------------------------------


# For DWI 

# Change format for plotting
dat_modelsl <- rbind(long_format(dat_models, "nll"),
                     long_format(dat_models, "qwk"),
                     long_format(dat_models, "acc"),
                     long_format(dat_models, "auc"),
                     long_format(dat_models, "bs"))

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


# # Ordinal vs. binary
# dat_modelsl$method <- factor(dat_modelsl$method, levels = c("ordinal", "binary"))
# method_names <- c("binary" = "Binary functional outcome", "ordinal" = "Ordinal functional outcome")
# 
# pdf(paste0(FIG_DIR, "Results_models_ordinal_binary_bootstrap.pdf"), height = 8, width = 10)
# ggplot(dat_modelsl[dat_modelsl$metric == "nll",], aes(x = data, y = boot)) +
#   geom_jitter(width = 0.1, size = 1, colour = "lightblue3") + 
#   geom_point(position = pd, aes(x = data, y = median), size = 3) +
#   geom_errorbar(position = pd, aes(ymin = ci_lower, ymax = ci_upper), width = 0.11) +
#   coord_flip() +
#   facet_wrap(~method, scales="free_x", ncol = 4, labeller = as_labeller(method_names)) +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 90, vjust = 0.5, hjust = 0.5, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 15),
#         legend.title = element_text(color = "black", size = 15),
#         legend.position="bottom",
#         strip.background = element_rect(colour="black", fill="white"),
#         strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
#   xlab("Data") + 
#   ylab("NLL")
# dev.off()





# For DWI vs. TMAX

# Change format for plotting
dat_modelsl_tmax <- rbind(long_format(dat_models_tmax, "nll"),
                          long_format(dat_models_tmax, "qwk"),
                          long_format(dat_models_tmax, "acc"),
                          long_format(dat_models_tmax, "auc"),
                          long_format(dat_models_tmax, "bs"))
dat_modelsl_tmax$metric <- factor(dat_modelsl_tmax$metric, levels = c("nll", "acc", "qwk", "auc", "bs"))
metric_names <- c("nll" = "NLL", "acc" = "ACC", "qwk" = "QWK", "auc" = "AUC", "bs" = "BS")
dat_modelsl_tmax$method <- factor(dat_modelsl_tmax$method, levels = c("ordinal", "binary"))
dat_modelsl_tmax$data <- factor(dat_modelsl_tmax$data, levels = c("DWI+TMAX", "TMAX", "DWI"))


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






# Calibration plots

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




# Figures Models vs. Raters ----------------------------------------------------

# change format for plotting
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


# Combine raters with model results
dat_models_raters <- rbind(dat_modelsl2, dat_ratersl)
dat_models_raters$rater <- factor(dat_models_raters$rater, levels = c(0:6))

dat_models_raters$metric <- factor(dat_models_raters$metric, levels = c("acc", "qwk", "auc", "sens", "spec"))
metric_names <- c("acc" = "ACC", "qwk" = "QWK", "auc" = "AUC", "sens" = "SENS", "spec" = "SPEC")
dat_models_raters$data <- factor(dat_models_raters$data, levels = c("Clinical+Imaging", "Imaging", "Clinical"))
dat_models_raters$method <- factor(dat_models_raters$method, levels = c("ordinal", "binary"))


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




### Figures: Model estimates ---------------------------------------------------

# Get the estimates for the different models for plotting
est_clinical <- get_estimates(paste0(DIR, "SI_LSx"))
est_clinical_and_image <- get_estimates(paste0(DIR, "SI_LSx_CSb"))
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


# Single plots for SI-LSx and SI-LSx-CSb

# # one standard unit increase
# pdf(paste0(FIG_DIR,"ESTIMATES_importance_SI_LSx.pdf"), height = 8, width = 12)
# ggplot(est_clinical, aes(x = variable, y = value, group = fold, color = fold)) +
#   geom_hline(yintercept=1, linetype = "dashed", color = "darkgrey") + 
#   geom_point(position = pd) + 
#   geom_point(aes(x = variable, y = mest), col = "black", fill = "black", shape = 24, size = 3) +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, hjust = 0.5, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 15),
#         legend.title = element_text(color = "black", size = 15),
#         legend.position="bottom",
#         strip.background = element_rect(colour="black", fill="white"),
#         strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
#   scale_colour_manual(values = c("0" = "lightblue4",
#                                  "1" = "lightcoral",
#                                  "2" = "plum3",
#                                  "3" = "lavenderblush3",
#                                  "4" = "lightblue3"),
#                       name = "CV") +
#   xlab(" ") + 
#   ylab("Odds ratio") 
# dev.off()


# pdf(paste0(FIG_DIR,"ESTIMATES_importance_SI_LSx_CSb.pdf"), height = 8, width = 12)
# ggplot(est_clinical_and_image, aes(x = variable, y = value, group = fold, color = fold)) +
#   geom_hline(yintercept=1, linetype = "dashed", color = "darkgrey") + 
#   geom_point(position = pd) + 
#   geom_point(aes(x = variable, y = mest), col = "black", fill = "black", shape = 24, size = 3) +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black"),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.text.x = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, face = "plain"),
#         axis.text.y = element_text(color = "black", size = 15, angle = 0, vjust = 0.5, hjust = 0.5, face = "plain"),
#         axis.title.x = element_text(color = "black", size = 16, angle = 0, hjust = 0.5, vjust = 0, face = "plain"),
#         axis.title.y = element_text(color = "black", size = 16, angle = 90, hjust = 0.5, vjust = .5, face = "plain"),
#         legend.background = element_blank(),
#         legend.text = element_text(color = "black", size = 15),
#         legend.title = element_text(color = "black", size = 15),
#         legend.position="bottom",
#         strip.background = element_rect(colour="black", fill="white"),
#         strip.text = element_text(color = "black", size = 16, angle = 0, vjust = 0.5, face = "plain")) +
#   scale_colour_manual(values = c("0" = "lightblue4",
#                                  "1" = "lightcoral",
#                                  "2" = "plum3",
#                                  "3" = "lavenderblush3",
#                                  "4" = "lightblue3"),
#                       name = "CV") +
#   xlab(" ") + 
#   ylab("Odds ratio") 
# dev.off()